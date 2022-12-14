options(scipen = 999)
#libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# for saving models (bulbulia only - use own paths for simulated data)
push_mods <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/attacks/mods")
push_figs <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/attacks/figs")

# install.packages("arrow", repos = c(arrow = "https://nightlies.apache.org/arrow/r", getOption("repos")))
# read data (again bulbulia only) If replicating use the jittered data in the data folder
pull_path <-
  fs::path_expand(
    "/Users/joseph/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# libraries
#source(here::here("R", "libs.R"))

# read data
df <- readRDS(here::here("data", "df_s"))

dat <- arrow::read_parquet(pull_path)


# rdd graph ---------------------------------------------------------------

### Wave 10 Only
library(dplyr)
sadi <- dat %>%
  dplyr::filter((Wave == 2012 &  YearMeasured == 1) |
                  (Wave == 2013 &  YearMeasured == 1) |
                  (Wave == 2014 &  YearMeasured == 1) |
                  (Wave == 2015 &  YearMeasured == 1) |
                  (Wave == 2016 &  YearMeasured == 1) |
                  (Wave == 2017 &  YearMeasured == 1) |
                  (Wave == 2018 &  YearMeasured == 1) |
                  (Wave = 2019 & YearMeasured == 1)
  ) %>%
  droplevels() %>%
  select(Warm.Muslims, TSCORE, Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr::filter(timeline > "2012-06-06") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(Attack_Condition = factor(
    ifelse(timeline < "2019-03-15", 0, 1),
    labels = c("Pre-Attack", "Post-attack")
  )) %>%
  arrange(timeline, Attack_Condition)

sadi <- as.data.frame(sadi)
head(sadi)
min(sadi$day)

ids <- df %>%
  dplyr::filter((Wave == 2012 &  YearMeasured == 1) |
                  (Wave == 2013 &  YearMeasured == 1) |
                  (Wave == 2014 &  YearMeasured == 1) |
                  (Wave == 2015 &  YearMeasured == 1) |
                  (Wave == 2016 &  YearMeasured == 1) |
                  (Wave == 2017 &  YearMeasured == 1) |
                  (Wave == 2018 &  YearMeasured == 1) |
                  (Wave = 2019 & YearMeasured == 1)
  ) %>%
  group_by(Id, Wave) %>%
  select(Id, Wave)

length(unique(ids$Id))

# Overall
# (N=228406)
# Attack_Condition
# Baseline	140793 (61.6%)
# Post-attack	87613 (38.4%)

table1::table1(~ Attack_Condition, data = sadi)

library(ggsci)
rdd <-
  ggplot(sadi, aes(x = timeline, y = Warm.Muslims, color = Attack_Condition)) +
  geom_jitter(alpha = .01, width = 1) +
  stat_smooth(method = "gam") +
  theme(legend.position = "bottom") +
  labs(
    title = "Discontinuity at attacks (GAM)",
    subtitle = "Boost to Warmth increase in the years following the attacks",
    y = "Muslim Warmth",
    x = "NZAVS Time 4 - 12 (2012-2022), N = 67858"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic()

# graph
rdd
# combine graph
library(patchwork)
comb_sens_graph3 <- ldsA / rdd  + plot_annotation(tag_levels = "A")

# check
comb_sens_graph3

ggsave(
  comb_sens_graph3,
  path = here::here(here::here("figs")),
  width = 9,
  height = 9,
  units = "in",
  filename = "comb_sens_graph3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)
citation("rdrobust")


# RDD ANALYSIS ------------------------------------------------------------
library("splines")
library("mgcv")
library("rdrobust")
library("stargazer")
library("broom")



# for rdd
sadi3 <- sadi %>%
  mutate(timeline = as.numeric(timeline) - min(as.numeric(timeline)))
table(sadi3$Attack_Condition)

# find lowest post-attack case
sadi3 %>% dplyr::filter(Attack_Condition == "Post-attack") %>%
  arrange(timeline, Id) %>%
  head()

# rdd
outr <-
  rdrobust(
    y = sadi3$Warm.Muslims,
    x = sadi3$timeline,
    c = 2468,
    all = TRUE
  )

# summary
summary(outr)


rdr_export <- function(rdr_out, prec = 3) {
  outrows = c("coef", "se", "z", "ci", "bws", "N_h", "N_b")
  out = rdr_out[outrows]
  # conventional
  ids = 1
  CI = paste0("(", as.numeric(round(out$ci[ids,][1], prec)),
              ",", as.numeric(round(out$ci[ids,][2], prec)), ")")
  conventional = rbind(
    Coef = round(out$coef[ids], prec),
    SE = round(out$se[ids],
               prec),
    `t-stat` = round(out$z[ids], prec),
    CI
  )
  # robust
  ids = 3
  CI = paste0("(", as.numeric(round(out$ci[ids,][1], prec)),
              ",", as.numeric(round(out$ci[ids,][2], prec)), ")")
  robust = rbind(
    Coef = round(out$coef[ids], prec),
    SE = round(out$se[ids],
               prec),
    `t-stat` = round(out$z[ids], prec),
    CI
  )
  common_rows = rbind(
    bw = paste0("(", round(out$bws[1, 1],
                           prec), ",", round(out$bws[1, 2], prec), ")"),
    Nobs = paste0("(",
                  out$N_h[1], ",", out$N_h[2], ")"),
    poly_order = rdr_out$p
  )
  # prep output table
  resout = cbind(c(rep("Local-Linear", 4), rep("Robust", 4), rep("--", 3)),
                 rbind(conventional, robust, common_rows)) %>%
    data.frame(rowname = row.names(.), .)
  rownames(resout) <- NULL
  names(resout) = c('stat', 'type', 'val')
  resout$stat = gsub("poly_order", "Polynomial Order", resout$stat)
  resout$stat = gsub("bw", "Bandwidth", resout$stat)
  resout$stat = gsub("nobs", "Obs", resout$stat)
  return(resout[, c(2, 1, 3)])
}

rdr_export(outr, prec = 3) %>%
  pivot_wider(names_from = "stat", values_from = "val") %>%
  select(-c(`Polynomial Order`, Bandwidth, Nobs)) %>%
  slice(1:2)


# graph
rdplot(
  y = sadi3$Warm.Muslims,
  x = sadi3$timeline,
  c = 2468,
  title = "Robust Regression Discontinuity",
  x.label = "Days from start of Time 4 (2012)",
  y.label = "Warmth to Muslims (1-7)",
  x.lim = NULL,
  y.lim = NULL,
  col.dots = "dodgerblue",
  col.lines = NULL
)




# another approach
yola
outr_simple <-
  gam(Warm.Muslims ~ Attack_Condition + s(timeline, bs = "cs"), data = sadi3)
summary(outr_simple)

gab <- model_parameters(outr_simple)
tablex

%>%
  kbl("latex", booktabs = TRUE, digits = 2)



kbl("latex", booktabs = TRUE, digits = 2)




### Graph of Timeline
#
# sadi2 <- df %>%
#   filter(Wave == 2018 & YearMeasured == 1) %>%
#   # drop_levels() %>%
#   select(Warm.Muslims, TSCORE) %>%
#   dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
#   dplyr::filter(timeline > "2018-06-06") %>%
#   dplyr:::count(day = floor_date(timeline, "day")) %>%
#   dplyr::mutate(Attack_Condition = factor(
#     ifelse(day < "2019-03-15", 0, 1),
#     labels = c("Baseline", "Post-attack")
#   )) %>%
#   arrange(day, Attack_Condition)
#
# sadi2
# dates_vline2<- as.Date("2019-03-15")
# dates_vline2b<-which(sadi2$day %in% dates_vline2)
#
# dates_vline3<- as.Date("2019-06-15")
# dates_vline3b<-which(sadi2$day %in% dates_vline3)
#
# #dates_vline3<- as.Date("2019-06-18")
#
# lds3 <- ggplot(sadi2, aes(day, n)) +
#   geom_col(aes(fill = Attack_Condition)) +
#   scale_x_date(date_labels = "%b/%Y") +     #  limits = c(as.Date("2018-06-01"), as.Date("2021-10-16")))  +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#   geom_vline(xintercept = as.numeric(sadi2$day[dates_vline2b]),
#              col = "red",
#              linetype = "dashed") +
#   geom_vline(xintercept = as.numeric(sadi2$day[dates_vline3b]),
#              col = "red",
#              linetype = "dashed") +
#   labs(x = "NZAVS Waves years 2018 - 2021 daily counts by condition",
#        y = "Count of Responses",
#        #  title = "Data Collection for RDD Analysis of Mosque Attacks",
#        subtitle = "Data Collection Wave 10 (after June 6)")+
#   scale_colour_npg(alpha =.1) +
#   annotate(
#     geom = "text",
#     x = as.Date("2019-05-01"),
#     y = 2300,
#     label = "90 Days\nafter attacks") +
#   theme_classic()
#
#
# rdd_graph <- lds3/rdd + plot_layout(guides = 'collect') + plot_annotation(tag_levels = "i", "Replication of Regression Discontinuity Analysis")
#

rdd_graph

# zero table --------------------------------------------------------------


zt_s <-
  table1::table1(~ Ys |
                   As * Wave,
                 data = bind_zero1_s,
                 overall = F,
                 transpose = F)

t1kable(zt_s, format = "latex")

zt_s



