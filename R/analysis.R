# Analyisis
# author Joseph Bulbulia: joseph.bulbulia@gmail.com
# created by assembling materials from original analysis into one script.
# note: jb to improve annotatation of script

# packages ----------------------------------------------------------------

#libraries
library("here") # file management
library("equatiomatic") # equations
library("lubridate") # working with dates
library("ggplot2") # graphs
library("ggthemes") #themes
library("Amelia") # missing data imputation
library("patchwork") # combine graphs
library("lme4") # multilevel estimation
library("ggpubr") # graphs
library("easystats") # reporting
library("kableExtra") # tables
library("broom") # working with  models
library("broom.mixed") # mixed effects models
library("brms") # bayesian estimation
library("rstan") # backend brms
library("rstanarm") # graphing
library("cmdstanr") # backend brms', not used
library("tidybayes") # workign with posterior probability distributions
library("bayesplot") # graphs
library("ggokabeito")   # color palette
library("gghalves")     #  half geoms, not used
library("ggbeeswarm")   # Special distribution-shaped point jittering, not used
library("emmeans") # estimate marginal means
library("table1") # tables /now with latex
library("tidyverse") # data wrangling
library("sjstats") # stat summaries
library("magick") # images
library("simstudy") # simulation
library("future") #parrallel processing
library("brms") # bayesian estimation
library("ggpubr") # graphs
library("ggsci") # graphs

# increase vecor limit
# library(usethis)
# usethis::edit_r_environ()

# rstan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores ())



# import data -------------------------------------------------------------
df <- readRDS(here::here("data_raw", "df"))# orig data

#df <- readRDS(here::here("data", "df_s")) # gitterd data for reproduction (ethics requires)

# create timeline ---------------------------------------------------------

rarep <- df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack!
  ungroup(Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr:::count(day = floor_date(timeline, "day")) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      day >= "2018-06-18" & day < "2019-03-15",
      0,
      ifelse(
        day >= "2019-03-15" & day < "2019-06-18",
        1,
        ifelse(day >= "2019-06-18" &
                 day < "2020-10-15", 2, 3)
      )
    ),
    labels = c(
      "Baseline",
      "Post-attack",
      "Post-attack one year",
      "Post-attack two years"
    )
  )) %>%
  arrange(day, Condition)


# get attack date
dates_vline2 <- as.Date("2019-03-15")
dates_vline3 <- as.Date("2019-06-18")
dates_vline4 <- as.Date("2021-06-18")
#3665 - min = 13 July 2019
# 4125 max = 15 October 2021

# for line in a graph
dates_vline2b <- which(rarep$day %in% dates_vline2)
dates_vline3b <- which(rarep$day %in% dates_vline3)
dates_vline4b <- which(rarep$day %in% dates_vline4)

# graph

lds2 <- ggplot(rarep, aes(day, n)) +
  geom_col(aes(fill = Condition)) +
  scale_x_date(date_labels = "%b/%Y",
               limits = c(as.Date("2018-06-01"), as.Date("2021-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  xlab("NZAVS Waves years 2018 - 2021 daily counts by condition") + ylab("Count of Responses") +
  theme_classic()  +
  annotate(
    "rect",
    xmin = dates_vline2,
    xmax = dates_vline3,
    ymin = 0,
    ymax = 2000,
    alpha = .3,
    fill = "darkred"
  ) +
  annotate(
    "rect",
    xmin = dates_vline3,
    xmax = as.Date("2020-10-15"),
    ymin = 0,
    ymax = 2000,
    alpha = .05,
    fill = "orange"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2020-10-15"),
    xmax = as.Date("2021-10-15"),
    ymin = 0,
    ymax = 2000,
    alpha = .05,
    fill = "yellow"
  ) +
  annotate("text",
           x = as.Date("2018-10-15"),
           y = 2600,
           label = "Time 10\npre-attacks") +
  annotate("text",
           x = as.Date("2019-01-01"),
           y = 2950,
           label = "**attack**") +
  annotate("text",
           x = as.Date("2019-06-15"),
           y = 2600,
           label = "Time 10\npost-attacks") +
  annotate("text",
           x = as.Date("2020-03-01"),
           y = 2600,
           label = "Time 11 Year\nFollowing") +
  annotate("text",
           x = as.Date("2021-03-01"),
           y = 2600,
           label = "Time 12 2 years \nFollowing") +
  annotate(
    geom = "curve",
    x = as.Date("2018-11-15"),
    y = 2000,
    xend = as.Date("2020-02-15"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2018-02-15"),
    y = 2300,
    xend = as.Date("2018-10-15"),
    yend = 2300,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2018-02-15"),
    y = 2000,
    xend = as.Date("2019-01-01"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2019-01-01"),
    y = 2000,
    xend = as.Date("2020-10-15"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2020-03-15"),
    y = 2000,
    xend = as.Date("2021-06-15"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 6),
    legend.title = element_text(color = "Black", size = 8)
  ) +  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(limits = c(0, 2950))
#theme(legend.position="none")


# inspect graph
lds2

# save graph
ggsave(
  lds2,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "timeline.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# timeline for the 2016-2021 cohort

# create timeline 2016 - 2021 ---------------------------------------------------------


rarepA <- df %>%
 # dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(Wave == 2012 | Wave == 2013 | Wave == 2014 | Wave == 2015 |
                  Wave == 2016 | Wave == 2017 | Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  droplevels() %>%
#  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
#                                    YearMeasured == 1, 1, 0)) %>%
#  group_by(Id) %>%
#  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
#  filter(hold > 0) %>% # hack!
#  ungroup(Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr:::count(day = floor_date(timeline, "day")) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      day < "2019-03-19",
      0, 1 ),
    labels = c(
      "Pre-attack",
      "Post-attack"
    )
  )) %>%
  arrange(day, Condition)

rarepA  # min day 2012-09-19

max(rarepA$day, na.rm = TRUE)  # max day day 2021-09-30
# get  dates
dates_vline2 <- as.Date("2019-03-15")
#dates_vline3 <- as.Date("2019-06-18")
#dates_vline4 <- as.Date("2021-06-18")
#3665 - min = 13 July 2019

# for line in a graph
dates_vline2b <- which(rarepA$day %in% dates_vline2)
# dates_vline3b <- which(rarep2$day %in% dates_vline3)
# dates_vline4b <- which(rarep2$day %in% dates_vline4)

# graph

ldsA <- ggplot(rarepA, aes(day, n)) +
  geom_col(aes(fill = Condition)) +
  scale_x_date(date_labels = "%Y", #"%b/%Y",
               date_breaks = "1 year",
               limits = c(as.Date("2012-06-01"), as.Date("2021-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  labs(title = "New Zealand Attitudes and Values Study (panel)", subtitle = "N = 67,858; years 2012-2021") +
  xlab("NZAVS Waves years 2012 - 2021 daily counts by condition") + ylab("Count of All Responses") +
  theme_classic() +
  # annotate(
  #   "rect",
  #   xmin = dates_vline2,
  #   xmax = dates_vline3,
  #   ymin = 0,
  #   ymax = 2500,
  #   alpha = .3,
  #   fill = "darkred"
  # ) +
#   annotate(
#     "rect",
#     xmin = dates_vline3,
#     xmax = as.Date("2020-10-15"),
#     ymin = 0,
#     ymax = 2500,
#     alpha = .05,
#     fill = "orange"
#   ) +
#   annotate(
#     "rect",
#     xmin = as.Date("2020-10-15"),
#     xmax = as.Date("2021-10-15"),
#     ymin = 0,
#     ymax = 1500,
#     alpha = .05,
#     fill = "yellow"
#   ) +
#   annotate("text",
#            x = as.Date("2017-06-01"),
#            y = 2000,
#            label = "Time 8-10\npre-attacks waves") +
#   annotate("text",
#            x = as.Date("2019-01-01"),
#            y = 1700,
#            label = "**attack**") +
#   annotate("text",
#            x = as.Date("2019-06-15"),
#            y = 2000,
#            label = "Time 10\npost-attacks") +
#   annotate("text",
#            x = as.Date("2020-03-01"),
#            y = 2000,
#            label = "Time 11 Year\nFollowing") +
#   annotate("text",
#            x = as.Date("2021-03-01"),
#            y = 2000,
#            label = "Time 12 2 years \nFollowing") +
# #   annotate(
#     geom = "curve",
#     x = as.Date("2016-11-15"),
#     y = 2000,
#     xend = as.Date("2020-02-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2016-02-15"),
#     y = 2300,
#     xend = as.Date("2016-10-15"),
#     yend = 2300,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2016-02-15"),
#     y = 2000,
#     xend = as.Date("2019-01-01"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2019-01-01"),
#     y = 2000,
#     xend = as.Date("2020-10-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2020-03-15"),
#     y = 2000,
#     xend = as.Date("2021-06-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 6),
#     legend.title = element_text(color = "Black", size = 8)
#   )
scale_fill_okabe_ito() + theme_classic() +
# scale_fill_viridis_d(option = "plasma") +
 scale_y_continuous(limits = c(0, 2300))
 #theme(legend.position="none")


# inspect graph
ldsA

# save graph
ggsave(
  ldsA,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "timeline_all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# HISTOGRAM ALL 2012 - 2021 -----------------------------------------------



rarep2 <- df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(Wave == 2016 | Wave == 2017 | Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  droplevels() %>%
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack!
  ungroup(Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr:::count(day = floor_date(timeline, "day")) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      day >= "2016-06-18" & day < "2019-03-15",
      0,
      ifelse(
        day >= "2019-03-15" & day < "2019-06-18",
        1,
        ifelse(day >= "2019-06-18" &
                 day < "2020-10-15", 2, 3)
      )
    ),
    labels = c(
      "Baseline",
      "Post-attack",
      "Post-attack one year",
      "Post-attack two years"
    )
  )) %>%
  arrange(day, Condition)

rarep2

# get  dates
dates_vline2 <- as.Date("2019-03-15")
dates_vline3 <- as.Date("2019-06-18")
dates_vline4 <- as.Date("2021-06-18")
#3665 - min = 13 July 2019

# for line in a graph
dates_vline2b <- which(rarep2$day %in% dates_vline2)
dates_vline3b <- which(rarep2$day %in% dates_vline3)
dates_vline4b <- which(rarep2$day %in% dates_vline4)

# graph

lds3 <- ggplot(rarep2, aes(day, n)) +
  geom_col(aes(fill = Condition)) +
  scale_x_date(date_labels = "%b/%Y",
               date_breaks = "1 year",
               limits = c(as.Date("2016-09-10"), as.Date("2021-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  xlab("NZAVS Waves years 2016 - 2021 daily counts by condition") + ylab("Count of Responses") +
  theme_classic() +
  annotate(
    "rect",
    xmin = dates_vline2,
    xmax = dates_vline3,
    ymin = 0,
    ymax = 1500,
    alpha = .3,
    fill = "darkred"
  ) +
  annotate(
    "rect",
    xmin = dates_vline3,
    xmax = as.Date("2020-10-15"),
    ymin = 0,
    ymax = 1500,
    alpha = .05,
    fill = "orange"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2020-10-15"),
    xmax = as.Date("2021-10-15"),
    ymin = 0,
    ymax = 1500,
    alpha = .05,
    fill = "yellow"
  ) +
  annotate("text",
           x = as.Date("2017-06-01"),
           y = 2000,
           label = "Time 8-10\npre-attacks waves") +
  annotate("text",
           x = as.Date("2019-01-01"),
           y = 1700,
           label = "**attack**") +
  annotate("text",
           x = as.Date("2019-06-15"),
           y = 2000,
           label = "Time 10\npost-attacks") +
  annotate("text",
           x = as.Date("2020-03-01"),
           y = 2000,
           label = "Time 11 Year\nFollowing") +
  annotate("text",
           x = as.Date("2021-03-01"),
           y = 2000,
           label = "Time 12 2 years \nFollowing") +
  #   annotate(
  #     geom = "curve",
  #     x = as.Date("2016-11-15"),
  #     y = 2000,
  #     xend = as.Date("2020-02-15"),
  #     yend = 2000,
  #     curvature = -.3,
  #     arrow = arrow(length = unit(2, "mm"))
  #   ) +
  #   annotate(
  #     geom = "curve",
#     x = as.Date("2016-02-15"),
#     y = 2300,
#     xend = as.Date("2016-10-15"),
#     yend = 2300,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2016-02-15"),
#     y = 2000,
#     xend = as.Date("2019-01-01"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2019-01-01"),
#     y = 2000,
#     xend = as.Date("2020-10-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2020-03-15"),
#     y = 2000,
#     xend = as.Date("2021-06-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 6),
#     legend.title = element_text(color = "Black", size = 8)
#   )
scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(limits = c(0, 2300))
#theme(legend.position="none")


# inspect graph
lds3

# save graph
ggsave(
  lds3,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "timeline3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)




# pre-attack trend in muslim cohort from t4 -t10 --------------------------

# wrangle data into shape. get baseline for all cohort and impute all missing values, except when mortality is known
all_d <- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    Partner,
    Parent,
    EthnicCats,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    GenCohort,
    Urban,
    TSCORE,
    EthnicCats,
    Employed,
    Warm.Muslims,
    Muslim,
    TSCORE,
    WSCORE,
    YearMeasured,
    Religious,
    GenCohort
  ) %>%
  dplyr::filter(
    Wave == 2012 & YearMeasured == 1 |
      Wave == 2013 & YearMeasured != -1 |
      Wave == 2014 & YearMeasured != -1 |
      Wave == 2015 & YearMeasured != -1 |
      Wave == 2016 & YearMeasured != -1 |
      Wave == 2017 & YearMeasured != -1
  ) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2012", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b) %>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave == 2013,
    TSCORE_b + 365,
    ifelse(
      YearMeasured == 0 & Wave == 2014,
      TSCORE_b + 730,
      ifelse(
        YearMeasured == 0 & Wave == 2015,
        TSCORE_b + 1094,
        # leap
        ifelse(
          YearMeasured == 0 & Wave == 2016,
          TSCORE_b + 1459,
          ifelse(YearMeasured == 0 &
                   Wave == 2017, TSCORE_b + 1824, TSCORE)
        )
      )
    )
  )) %>%
  dplyr::mutate(Attack = as.numeric(ifelse(TSCORE_i >= 3545, 1, 0))) %>% # All 0
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)),
                yrs = dys / 365) %>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Wave, Id) %>%
  droplevels() %>%
  arrange(Wave, Id) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
  group_by(Id) %>%
  dplyr::mutate(Ys = Warm.Muslims) %>%
  group_by(Id) %>% # need to fill this way
  dplyr::mutate(pol_bz = if_else(Wave == "2012", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2012", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2012", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2012", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2012", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2012", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2012", (as.numeric(Male)) / 2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2012", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2012", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2012", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2012", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>%
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup() %>%
  dplyr::mutate(As = replace_na(As, 0)) %>%
  arrange(Wave, Id)
levels(all_d$Wave) <-
  c("Time4", "Time5", "Time6", "Time7", "Time8", "Time9")


head(all_d_selected)

# select variables
all_d_selected <- all_d %>%
  dplyr::select(
    Id,
    Wave,
    wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs,
    TSCORE_i
  )



# latex table
tall <-
  table1::table1( ~ Ys |
                    Wave, data = all_d_selected, overall = FALSE)

tall
kable(tall, format = "latex", booktabs = TRUE)


# multiple imputation
library(Amelia)

# We impute the entire distribution of Attack = 0,


# assume Y^0|A=2018 = Y^0 2019

match("Ys",names(all_d_selected))  # for obtaining bounds for Muslim outcome

# needs to be a data frame
all_d_selected <- as.data.frame(all_d_selected)

bds <- matrix(c(5, 1, 7), nrow = 1, ncol = 3) # bounds for mus

imputed_m <- amelia(
  set.seed = 1234,
  all_d_selected,
  #dataset to impute
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
 # ords = "Ys",    # used for published analysis , must use numeric because values have been jittered
  lags = "Ys",
  # leads="Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "yrs", "dys", "TSCORE_i"),
  #  polytime = 2, # Allow polynomial?
  intercs = F,
  # too many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(all_d_selected)
)

# save
saveRDS(imputed_m, here::here("mods","imputed_m"))

# fetch
imputed_m <-
  readRDS(here::here( "mods", "imputed_m"))

# check
max(imputed_m$imputations[[1]]$yrs)

# to compare with linear model
library(splines)

m <- 10
model_m <- NULL
for (i in 1:m) {
  model_m$model[[i]] <-
    lmer(Ys ~ bs(yrs)  + (1 |
                            Id), data = imputed_m$imputations[[i]])
}

# to recover linear trajectory
library(lme4)
m <- 10
model_ml <- NULL
for (i in 1:m) {
  model_ml$model[[i]] <-
    lmer(Ys ~ yrs  + (1 | Id), data = imputed_m$imputations[[i]])
}

pool_parameters(model_ml$model)

library(parameters)
# spline

tab <- pool_parameters(model_ml$model)
tab

# Latex table
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)


# graph with uncertainty

fitted_lines_yrs <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_ml$model[[.]], terms = c("yrs[0:6.15, by=.05]")
  ))) %>%
  data_frame() %>%
  unnest()

fitted_lines_yrs <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(model_ml$model[[.]], terms = c("yrs[all]")))) %>%
  data_frame() %>%
  unnest()


plot_time <- fitted_lines_yrs %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.5)) +
  labs(title = "National New Zealand trajectory in Muslim acceptance",
       subtitle = "years: 2012-2017/18; N = 12179") +
  labs(y = "Muslim Warmth",
       x = "Years:2012-2017/19") +
  scale_y_continuous(limits = c(3, 4.5)) +
  theme_classic() #coord_flip()

plot_time


# save graph
ggsave(
  plot_time,
  path = here::here(here::here( "figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "pplot_time,jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


### DATA FOR MISSIN YEARS



# data wrangling for main analysis ----------------------------------------

km_all3 <- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    EthnicCats,
    Employed,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    Religious,
    GenCohort,
    Urban,
    TSCORE,
    Partner,
    Parent,
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2018", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b) %>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave == 2019,
    TSCORE_b + 364,
    ifelse(YearMeasured == 0 &
             Wave == 2020, TSCORE_b + 729, TSCORE)
  )) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 &
       Wave == 2018) |
      (Wave == 2019 |
         Wave == 2020), 1, 0
  )))) %>% # All 2019s even if NA need to be 1
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)),
                yrs = dys / 365) %>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
  group_by(Id) %>% # need to fill this way
  dplyr::mutate(pol_bz = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2018", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2018", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2018", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2018", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2018", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2018", (as.numeric(Male)) / 2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2018", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2018", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2018", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2018", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>%
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup() %>%
  arrange(Id, Wave)
levels(km_all3$Wave) <- c("Time10", "Time11", "Time12")


summary(km_all3$Wave)

# check
length(unique(km_all3$Id))


#table
t13 <-
  table1::table1( ~ Warm.Muslims |
                    as.factor(Attack) * Wave,
                  data = km_all3,
                  overall = FALSE)
t13

# latex
kable(t13, format = "latex", booktabs = TRUE)



# implement flawed model that does not adjust for missingness. ------------

bad_mod1 <-
  lmer(Ys ~ as.factor(As) + Wave + (1 | Id), data = km_all3)


model_parameters(bad_mod1)

pl_drop_bad <-
  ggeffects::ggemmeans(bad_mod1, terms = c("Wave", "As"))

drop__graph <- plot(pl_drop_bad) + labs(x = "Years 2018-2021",
                                        y = "Warmth to Muslims",
                                        title = "Pairwise deletion graph: no imputation")
drop__graph


tab_drop <- model_parameters(bad_mod1,  summary = FALSE)
tab_drop

tab_drop[, c(1:5)] %>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab_drop,  show_labels = TRUE)


# save graph
ggsave(
  drop__graph,
  path = here::here(here::here( "figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "drop__graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



# table for study ---------------------------------------------------------


# table  for
table1::table1( ~ Warm.Muslims |
                  Wave * as.factor(Attack),
                data = km_all3,
                overall = FALSE)

obtl <-
  table1::table1( ~ Warm.Muslims |
                    as.factor(Attack) * Wave,
                  data = km_all3,
                  overall = FALSE)

# clearer table
kable(obtl, format = "latex", booktabs = TRUE)




# clone data to enable imputation of missing outcomes  --------------------

library(tidyverse)
kf3  <- km_all3 %>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (
    ifelse(
      Wave == "Time10" & Attack == 1 | Wave == "Time11" |
        Wave == "Time12",
      0,
      ifelse(Wave == "Time10" &
               Attack == 0, 1, Attack)
    )
  )) %>%
  mutate(Ys = ifelse(
    Wave == "Time10" & Attack == 1 | Wave == "Time11" |
      Wave == "Time12",
    NA,
    ifelse(Wave == "Time10" &
             Attack == 0, NA, Warm.Muslims)
  )) %>%
  ungroup() %>%
  arrange(Wave, Id)


# dat_all N
# correct

# CHeck NAs = Correct
table1::table1( ~ Ys |
                  Wave * as.factor(As), data = kf3, overall = F)



## Bind - double dataset to creat missing values
ka3 <- km_all3 %>%
  bind_rows(kf3) %>%
  arrange(Id, Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

saveRDS(ka3, here::here( "mods", "ka3"))

#head(ka3$Ys)
# Check

# Missing data problem
t2 <- table1::table1(~ Ys | Wave * As, data = ka3, overall = F)
t2
t2
t1kable(t2, format = "latex")



# link dfs for zero estimate -----------------------------------------
km_zero <- ka3 %>%
  filter((As == 0)) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs,
    TSCORE_i,
  ) %>%
  arrange(Wave, Id)

str(km_zero$As)
summary(km_zero$yrs)
summary(km_zero$TSCORE_i)


head(km_zero)
dim(km_zero)


# remove wave 10 from the pre-attack sample (above)

km_pre <- all_d_selected %>%
  # dplyr::filter(Wave != "Time10")%>%
  # droplevels() %>%
  dplyr::select(c(-wave)) %>% # wrong
  dplyr::mutate(As = as.factor(As)) %>%
  group_by(Id) %>%
  arrange(Wave, Id)

dim(km_pre)
head(km_pre)
table1::table1( ~ Ys | Wave * As, data = km_pre, overall = FALSE)


str(km_pre$As)
length(unique(km_pre$Id))
str(km_pre$As)
summary(km_pre$dys)
# bind rows and arrange
bind_zero <- full_join(km_pre, km_zero) %>%
  arrange(Wave, Id)

table(bind_zero$Wave)
# relevel
levels = c("Time4",
           "Time5",
           "Time6",
           "Time7",
           "Time8",
           "Time9",
           "Time10",
           "Time11",
           "Time12")

bind_zero$Wave <- fct_relevel(bind_zero$Wave, levels)
levels(bind_zero$Wave)
str(bind_zero$Wave)
head(bind_zero)
## Make numeric var

bind_zero1 <- bind_zero %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  dplyr::select(-c(dys, yrs)) %>%  # not working
  arrange(Wave, Id) %>%
  group_by(Id) %>%
  mutate(dys = TSCORE_i - min(TSCORE_i)) %>% # Not used
  mutate(yrs = dys / 365) %>% # note used
  ungroup()

summary(bind_zero1$wave)
# correct



# zero table --------------------------------------------------------------


zt <-
  table1::table1( ~ Ys |
                    As * Wave,
                  data = bind_zero1,
                  overall = F,
                  transpose = F)
zt
t1kable(zt, format = "latex")

# Impute 0s ------------------------------------------------------------


bind_zero1 <- as.data.frame(bind_zero1)

library(Amelia)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.
# assume Y^0|A=2018 = Y^0 2019


# limit Ys to range 1 to 7

# find col number
head(bind_zero1)
# create bounds

match("Ys",names(bind_zero1))  # for obtaining bounds for Muslim outcome

bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

# cannot get dys/years easily, use wave
imputed0 <- amelia(
  set.seed = 1234,
  bind_zero1,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  #ords = "Ys",  #*** NOTE THAT IN ORIGINAL ANLYSIS WE USE ORDS
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "TSCORE_i", "dys", "yrs"),
  # yrs not working
  lags = "Ys",
  #leads="Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero1)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0, here::here( "mods", "imputed0"))

#52965 Total N

length(unique(imputed0$imputations$imp1$Id))
head(imputed0$imputations$imp1$Id)



# inspect
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0$imputations$imp10,
                overall = FALSE)


# impute 1s ---------------------------------------------------------------

km_one <- ka3 %>%
  filter((As == 1)) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs,
    TSCORE_i,
  ) %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  arrange(Wave, Id)

summary(km_one$wave)
#check looks good
table1::table1( ~ Ys | Wave * As, data = km_one, overall = FALSE)



head(km_one)
dim(km_one)
# make data frame
km_one <- as.data.frame(km_one)


at <-
  table1::table1( ~ Ys |
                    As * Wave,
                  data = km_one,
                  overall = F,
                  transpose = F)

t1kable(at, format = "latex")

# create bounds for Ys
head(km_one)

match("Ys",names(km_one))  # for obtaining bounds for Muslim outcome


# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1 <- amelia(
  set.seed = 1234,
  km_one,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  # ords = "Ys",  in analysis preserved ords
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys", "yrs", "TSCORE_i"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  #  polynomials perhaps not sensible given
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(km_one)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1, here::here( "mods", "imputed1"))



# traceplot 1s ------------------------------------------------------------

imputed1 <- readRDS(here::here( "mods", "imputed1"))
dev.off()
set.seed(0)
out1 <-
  as.character(sample(imputed1$imputations$imp1$Id, 12, replace = FALSE))
out1


imputed0 <- readRDS(here::here( "mods", "imputed0"))
dev.off()
set.seed(0)
out0 <-
  as.character(sample(imputed0$imputations$imp1$Id, 12, replace = FALSE))
out0


# traceplot 00's
tscsPlot(
  imputed0,
  cs = c(out0),
  main = "no-attack exposure",
  var = "Ys",
  nr = 3,
  ylim = c(0, 8)
)

# traceplot 01
tscsPlot(
  imputed1,
  cs = c(out1),
  main = "attack exposure",
  var = "Ys",
  nr = 3,
  ylim = c(0, 8)
)



# check imputation of 1's -------------------------------------------------


table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1$imputations$imp10,
                overall = FALSE)

# make frames compatible --------------------------------------------------
imputed0 <- readRDS(here::here( "mods", "imputed0"))
imputed1 <- readRDS(here::here( "mods", "imputed1"))


imp0 <- transform(imputed0, Wave = as.character(Wave))
imp1 <- transform(imputed1, Wave = as.character(Wave))


# bind data frames --------------------------------------------------------
levels_old <- c("Time4",
                "Time5",
                "Time6",
                "Time7",
                "Time8",
                "Time9",
                "Time10",
                "Time11",
                "Time12")
newlevels = c("Time10", "Time11", "Time12")

m <- 10
zero <- NULL
for (i in 1:m) {
  zero$imputations$imp[[i]] <- imp0$imputations[[i]] %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, levels_old)) %>%
    droplevels() %>%
    dplyr::group_by(Id) %>%
    arrange(Wave, Id)
}

one <- NULL

for (i in 1:m) {
  one$imputations$imp[[i]] <- imp1$imputations[[i]] %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, newlevels)) %>%
    droplevels() %>%
    arrange(Id)
}


m <- 10
imps_bind <- NULL
for (i in 1:m) {
  imps_bind$imputations$imp[[i]] <-
    dplyr::bind_rows(zero$imputations$imp[[i]],
                     one$imputations$imp[[i]]) %>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    droplevels() %>%
    dplyr::mutate(Wave = as.numeric(Wave) - 1) %>%
    dplyr::arrange(Wave, Id)


}



# Works
summary(imps_bind$imputations$imp[[1]]$Wave)


# save
saveRDS(imps_bind, here::here("mods", "imps_bind"))

# read
imps_bind <- readRDS(here::here("mods", "imps_bind"))

# make list for bayesian models
listbayes <- imps_bind$imputations$imp

# save list for bayesian models
saveRDS(listbayes, here::here("mods", "listbayes"))

#readRDS
listbayes <-
  readRDS(here::here( "mods", "listbayes"))


# ML model ----------------------------------------------------------------

# model
library(lme4)
m <- 10
model_all <- NULL
for (i in 1:m) {
  model_all$model[[i]] <-
    lmer(Ys ~ As * Wave + (1 |
                             Id), data = imps_bind$imputations$imp[[i]])
}

# table
tab <- pool_parameters(model_all$model)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)


fitted_lines_all <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()


plot_3 <- fitted_lines_all %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.5)) +
  labs(subtitle = "Multiple imputation: sample from previous 6 waves prior to attack + full attack wave sample + 2 post-attack waves",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =
                                                                        .5) + theme_classic()
plot_3


ggsave(
  plot_3,
  path = here::here(here::here( "figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


fitted_lines_all <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()

plot_3 <- fitted_lines_all %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.5)) +
  labs(subtitle = "MI from six previous waves + full attack wave",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_colour_npg(alpha = .5) + theme_classic()
# scale_colour_okabe_ito(alpha =.5) +
theme_classic()
plot_3


fitted_lines_all <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest() %>%
  rename(As = group)


library(ggsci)

plot_3 <- fitted_lines_all %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = As,
    colour = As
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = As),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.5)) +
  labs(subtitle = "Frequentist MI from six previous waves + full attack wave",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_colour_npg(alpha = .5) + theme_classic()
plot_3


ggsave(
  plot_3,
  path = here::here(here::here( "figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



# bayesian model ----------------------------------------------------------

imps_bind <-
  readRDS(here::here( "mods", "imps_bind"))


# Model form

bform =   bf(Ys ~ As  *  Wave + (0 + As || Id),
             sigma ~ 0 + As, set_rescor(rescor = FALSE))


# Data for testing model priors



# make list
imp1 <- as.data.frame(imps_bind$imputations$imp[[1]])
imp2 <- as.data.frame(imps_bind$imputations$imp[[2]])
imp3 <- as.data.frame(imps_bind$imputations$imp[[3]])
imp4 <- as.data.frame(imps_bind$imputations$imp[[4]])
imp5 <- as.data.frame(imps_bind$imputations$imp[[5]])
imp6 <- as.data.frame(imps_bind$imputations$imp[[6]])
imp7 <- as.data.frame(imps_bind$imputations$imp[[7]])
imp8 <- as.data.frame(imps_bind$imputations$imp[[8]])
imp9 <- as.data.frame(imps_bind$imputations$imp[[9]])
imp10 <- as.data.frame(imps_bind$imputations$imp[[10]])



# make test data w/ 22 Ids

i1 <- imp1 %>% arrange(Id, Wave) %>% slice(1:128)
i2 <- imp2 %>% arrange(Id, Wave) %>% slice(1:128)
i3 <- imp3 %>% arrange(Id, Wave) %>% slice(1:128)
i4 <- imp4 %>% arrange(Id, Wave) %>% slice(1:128)
i5 <- imp5 %>% arrange(Id, Wave) %>% slice(1:128)
i6 <- imp6 %>% arrange(Id, Wave) %>% slice(1:128)
i7 <- imp7 %>% arrange(Id, Wave) %>% slice(1:128)
i8 <- imp8 %>% arrange(Id, Wave) %>% slice(1:128)
i9 <- imp9 %>% arrange(Id, Wave) %>% slice(1:128)
i10 <- imp10 %>% arrange(Id, Wave) %>% slice(1:128)


ameliadata <-
  list(imp1, imp2, imp3, imp4, imp5, imp6, imp7, imp8, imp9, imp10)



prior = c(
  prior(normal(0.2, 0.25), class = b, coef = "As1"),
  prior(normal(0.05, 0.25), class = b, coef = "Wave"),
  prior(normal(0, 0.25),  class = b, coef = "As1:Wave"),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As0",
    dpar = "sigma"
  ),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As1",
    dpar = "sigma"
  ),
  prior(student_t(3, 4.1, 1), class = Intercept),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As0",
    group = "Id"
  ),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As1",
    group = "Id"
  )
)


# test


system.time(
  m_cluster  <- brms::brm_multiple(
    bform,
    family = gaussian,
    data = ameliadata,
    prior = prior,
    #seed = 1234,
    init = 0,
    warmup = 1000,
    iter =  2000,
    chains = 2,
    future = TRUE
    ,
    file = here::here("mods", "m_cluster.rds")
  )
)


# get priors
prior_summary(m_cluster)

stancode(m_cluster)




# simulate priors ---------------------------------------------------------

prior = c(set_prior("normal(0, 1)", class = "b"))

bform =   bf(Ys ~ As  *  Wave + (0 + As || Id),
             sigma ~ 0 + As, set_rescor(rescor = FALSE))


sim_prior_strong  <- brm(
  bform,
  family = "gaussian",
  data = imp5,
  prior = c(
    set_prior('normal(0, 1)', class = 'b'),
    set_prior(
      'exponential(1)',
      class = "b",
      dpar = "sigma",
      coef = "As1"
    ),
    set_prior(
      'exponential(1)',
      class = "b",
      dpar = "sigma",
      coef = "As0"
    )
  ),
  sample_prior = "only",
  # only one dataset
  #seed = 1234,
  init = 0,
  warmup = 500,
  iter =  1000,
  chains = 1,
  # backend = "cmdstanr"
  ,
  file = here::here( "mods", "sim_prior_strong.rds")
)


sim_prior_strong_plot <-
  plot(conditional_effects(
    sim_prior_strong,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

sim_prior_strong_plot2 <-
  sim_prior_strong_plot$`Wave:As`  + scale_y_continuous(limits = c(-10.0, 15)) +
  labs(subtitle = "Sampling from prior only: weakly regularised",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()


sim_prior_strong_plot2

ggsave(
  sim_prior_strong_plot2,
  path = here::here(here::here( "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "sim_prior_strong_plot2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

prior_strong_even = c(
  set_prior('normal(0.2, 0.25)', class = 'b', coef = "As1"),
  set_prior('normal(0.05, 0.25)', class = "b", coef = "Wave"),
  set_prior("normal(0, 0.25)",  class = "b", coef = "As1:Wave"),
  set_prior(
    "normal(0,1)",
    class = "b",
    coef = "As0",
    dpar = "sigma"
  ),
  set_prior(
    "normal(0,1)",
    class = "b",
    coef = "As1",
    dpar = "sigma"
  ),
  set_prior("student_t(3, 4.1, 1)", class = "Intercept"),
  set_prior(
    "student_t(3, 0, 2.5)",
    class = "sd",
    coef = "As0",
    group = "Id"
  ),
  set_prior(
    "student_t(3, 0, 2.5)",
    class = "sd",
    coef = "As1",
    group = "Id"
  )
)

system.time(
  sim_prior_stronger_even  <- brms::brm(
    bform,
    prior = prior_strong_even,
    family = gaussian,
    data = imp5,
    #seed = 1234,
    init = 0,
    warmup = 500,
    iter =  1000,
    chains = 1,
    #backend = "cmdstanr",
    sample_prior = "only"
    ,
    file = here::here( "mods", "sim_prior_stronger_even.rds")
  )
)

sim_prior_stronger_evenP <-
  plot(conditional_effects(
    sim_prior_stronger_even,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

sim_prior_stronger_evenPLOT <-
  sim_prior_stronger_evenP$`Wave:As`   + scale_y_continuous(limits = c(-10.0, 15)) +
  labs(subtitle = "Sampling from prior only: moderately regularised",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()


sim_prior_stronger_evenPLOT
ggsave(
  sim_prior_stronger_evenPLOT,
  path = here::here(here::here( "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "sim_prior_stronger_evenPLOT.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

library(patchwork)
compare_priors <-
  sim_prior_strong_plot2 + sim_prior_stronger_evenPLOT +
  plot_annotation(tag_levels  = "i", title = "Comparision of wealkly regularised and strongly regularised priors")

compare_priors


ggsave(
  compare_priors,
  path = here::here(here::here( "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "compare_priors.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


prior_strong_even2 = c(
  set_prior("normal(0, 0.25)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior(
    "student_t(3, 4, 1)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")
)


system.time(
  sim_prior_stronger_even2  <- brms::brm(
    bform,
    prior = prior_strong_even2,
    family = gaussian,
    data = imp5,
    #seed = 1234,
    init = 0,
    warmup = 500,
    iter =  1000,
    chains = 1,
    #backend = "cmdstanr",
    sample_prior = "only"
    ,
    file = here::here( "mods", "sim_prior_stronger_even2.rds")
  )
)

sim_prior_stronger_evenP2 <-
  plot(conditional_effects(
    sim_prior_stronger_even2,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

sim_prior_stronger_evenPLOT2 <-
  sim_prior_stronger_evenP2$`Wave:As`   + scale_y_continuous(limits = c(-10.0, 15)) +
  labs(subtitle = "Sampling from prior only: strongly regularised",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()


sim_prior_stronger_evenPLOT2
ggsave(
  sim_prior_stronger_evenPLOT2,
  path = here::here(here::here( "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "sim_prior_stronger_evenPLOT.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

library(patchwork)
compare_priors <-
  sim_prior_strong_plot2 + sim_prior_stronger_evenPLOT + sim_prior_stronger_evenPLOT2 +
  plot_annotation(tag_levels  = "i",
                  title = "Comparision of regularised priors:",
                  subtitle = "sampled from the posterior only")

compare_priors

ggsave(
  compare_priors,
  path = here::here(here::here( "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "compare_priors.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# model with strongly regularised priors ----------------------------------
prior_strong_even2 = c(
  set_prior("normal(0, 0.25)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior(
    "student_t(3, 4, 1)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")
)




# bayesian model that we used ---------------------------------------------


system.time(
  m_cluster_st  <- brms::brm_multiple(
    bform,
    family = gaussian,
    data = ameliadata,
    prior = prior_strong_even2,
    #seed = 1234,
    init = 0,
    warmup = 1000,
    iter =  2000,
    chains = 2,
    future = TRUE
    ,
    file = here::here("mods", "m_cluster_st.rds")
  )
)

# get priors for summaries in manuscript

prior_summary(m_cluster_st)

# alternatively
stancode(m_cluster_st)


# Table
tab_cluster_st <-
  lazerhawk::brms_SummaryTable(m_cluster_st, panderize = F)
tab_cluster_st

# table in latex
tab_cluster_st
tab_cluster_st %>%
  kable(booktabs = T,
        "latex",
        caption =  "Marginal effect of attack on warmth to Muslims",
        digits = 2) %>%
  print()

# graph  ------------------------------------------------------------------
pl_m_cluster_st <-
  plot(conditional_effects(
    m_cluster_st,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

library(ggsci)
plot_m_cluster_st <-
  pl_m_cluster_st$`Wave:As`  + scale_y_continuous(limits = c(4.1, 4.5)) +
  labs(subtitle = "Predicted marginal means for prejudice by attack condition",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()


plot_m_cluster_st


ggsave(
  plot_m_cluster_st,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "plot_m_cluster_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# make graphs for the stronger prior  --------------------------------

# bayesplot scheme
color_scheme_set("brightblue")

# graph for areas


## graph of expectation

m1_test_model_plot_st <-
  plot(conditional_effects(
    m_cluster_st,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

# save graph
#saveRDS(m1_test_model_plot_st, here::here( "mods", "m1_test_model_plot_st"))

m1_test_model_plot_st <-
  readRDS(here::here("mods", "m1_test_model_plot_st"))


# mage graph
cluster_plot_st <-
  m1_test_model_plot_st$`Wave:As`  + scale_y_continuous(limits = c(4.0, 4.5)) +
  labs(subtitle = "",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  theme_classic()


cluster_plot_st <-
  cluster_plot_st + labs(title = "Predicted marginal means for prejudice by attack condition")

### used graph
cluster_plot_st

# sim compare graph
cluster_plot_st_sim <-
  cluster_plot_st +  scale_y_continuous(limits = c(4.05, 4.55)) +
  labs(subtitle = "",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()

cluster_plot_st_sim


ggsave(
  cluster_plot_st,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "cluster_plot_title_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


## compare graph to more weakly regulated priors
cluster_plot_st <-
  m1_test_model_plot_st$`Wave:As`  + scale_y_continuous(limits = c(4.0, 4.5)) +
  labs(subtitle = "Strongly regularising priors",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()

library(patchwork)

weak_strong_priors_prediction <-
  cluster_plot_st + cluster_plot_title + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "i",
                  title = "Comparison of weakly and strongly regularising priors shows little effect in choice of prior")

weak_strong_priors_prediction

ggsave(
  weak_strong_priors_prediction,
  path = here::here(here::here( "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "weak_strong_priors_prediction.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# strong prior density overlay and combo plots ----------------------------
# density overlay strong prior
dens_overlay_strong_prior <-
  mcmc_dens_overlay(m_cluster_st, pars  = c("b_As1", "b_Wave", "b_As1:Wave"))

# save graph
saveRDS(
  dens_overlay_strong_prior,
  here::here( "mods", "dens_overlay_strong_prior")
)

dens_overlay_strong_prior <-
  readRDS(here::here( "mods", "dens_overlay_strong_prior"))

# used
dens_overlay_strong_prior

# areas plot
color_scheme_set("brightblue")

areas_plot_st <- mcmc_plot(
  m_cluster_st,
  variable = c("b_As1", "b_Wave", "b_As1:Wave"),
  #regex_pars = "beta",
  type = 'areas',
  prob = 0.95
)

areas_plot_st

ggsave(
  areas_plot_st,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "areas_plot_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


ggsave(
  dens_overlay_strong_prior,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "dens_overlay_strong_prior.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)






# strong prior combo plots ------------------------------------------------


plot_post_1 <- plot(
  m_cluster_st,
  variable = c("b_Intercept", "b_As1", "b_Wave", "b_As1:Wave"),
  plot = F,
  ask = T
)

plot_post_2 <- plot(
  m_cluster_st,
  variable = c("b_sigma_As0", "b_sigma_As1", "sd_Id__As0", "sd_Id__As1"),
  plot = F,
  ask = T
)

p1 <- plot_post_1[[1]]
p2 <- plot_post_2[[1]]



pp1 <- p1$bayesplots[[1]] + p1$bayesplots[[2]]
pp2 <- p2$bayesplots[[1]] + p2$bayesplots[[2]]

plot_post_8_st <- plot(
  m_cluster_st,
  variable = c(
    "b_Intercept",
    "b_As1",
    "b_Wave",
    "b_As1:Wave",
    "b_sigma_As0",
    "b_sigma_As1",
    "sd_Id__As0",
    "sd_Id__As1"
  ),
  plot = F,
  newpage = F,
  N = 8
)
plot_post_8_st

# trace plots
trace_plotsS <- plot_post_8_st[[1]]
trace_plotsS


p_trace_plotsS <-
  trace_plotsS$bayesplots[[1]] + trace_plotsS$bayesplots[[2]]

u_trace_plotsS <-
  p_trace_plotsS + labs(title = "MCMC posterior locations + chains")
u_trace_plotsS



# bayesian hypothesis tests

hyp0 <- hypothesis(m_cluster_st, "As1 + As1:Wave > As1 - Wave")
p0 <- plot(hyp0, plot = F)
out_h0 <- p0[[1]] + labs(subtitle = "Post trajectory > 0") +
  #scale_fill_colorblind() +
  theme_classic()
out_h0

hyp1 <- hypothesis(m_cluster_st, "Wave + As1:Wave  =  Wave")
p1 <- plot(hyp1, plot = F)
out_h1 <- p1[[1]] + labs(subtitle = "Post < Pre trajectory") +
  #scale_fill_colorblind() +
  theme_classic()
out_h1



hyp2 <-
  hypothesis(m_cluster_st, "sigma_As0 = sigma_As1", class = "b")
p2 <- plot(hyp2, plot = F)


hyp3 <- hypothesis(m_cluster_st, "As1 >0")

p3 <- plot(hyp3, plot = F)
out_h3 <-
  p3[[1]] + labs(subtitle = "Attack causes acceptance > 0") +
  #scale_fill_colorblind() +
  theme_classic()
out_h3

# combine graph
bayes_hypothesis <- out_h3 +  out_h0 + out_h1
bayes_hypothesis <-
  bayes_hypothesis +   plot_layout(guides = 'collect')

ggsave(
  bayes_hypothesis,
  path = here::here(here::here( "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "bayes_hypothesis.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


#### fig 2
dev.off()

#  (out_h3/ out_h0 / out_h1  )
fig2 <-
  (dens_overlay_strong_prior) /  (areas_plot_st + cluster_plot_st) + plot_annotation(tag_levels = "i")
fig2

ggsave(
  fig2,
  path = here::here(here::here("figs")),
  width = 16,
  height = 09,
  units = "in",
  filename = "fig2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# used this

library(ggpubr)
fig3 <-
  ggarrange(
    areas_plot_st,
    dens_overlay_strong_prior,
    cluster_plot_st,
    bayes_hypothesis,
    labels = c("A", "B", "C", "D"),
    heights = c(2, 2),
    # widths = c(1),
    ncol = 2,
    nrow = 2
  )

fig3

ggsave(
  fig3,
  path = here::here(here::here("figs")),
  width = 15,
  height = 10,
  units = "in",
  filename = "fig3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

# show plot
results_main <-
  cluster_plot_st + (out_h3 / out_h0 / out_h1) + plot_annotation(tag_levels = "i") +
  plot_layout(ncol = 2, widths = c(2, 1))
results_main



ggsave(
  results_main,
  path = here::here(here::here( "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results_main.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)



results_use <- areas_plot_st + dens_overlay_strong_prior  +
  plot_annotation(tag_levels = "i")

results_use

ggsave(
  results_use,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results_use.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

# ppchecks # assumptions not met
pp_check(m_cluster_st, nsamples = 20)


## table
lazerhawk::brms_SummaryTable(m_cluster)

# mcmc chains
color_scheme_set("brightblue")
#color_scheme_set("brewer-Spectral")
#color_scheme_set("mix-blue-red")
#plot(m_cluster)

plot_post_1 <- plot(
  m_cluster_st,
  variable = c("b_Intercept", "b_As1", "b_Wave", "b_As1:Wave"),
  plot = F,
  ask = T
)

plot_post_2 <- plot(
  m_cluster_st,
  variable = c("b_sigma_As0", "b_sigma_As1", "sd_Id__As0", "sd_Id__As1"),
  plot = F,
  ask = T
)

p1 <- plot_post_1[[1]]
p2 <- plot_post_2[[1]]

<- plot_post_1[[1]]$bayesplots

pp1 <- p1$bayesplots[[1]] + p1$bayesplots[[2]]
pp2 <- p2$bayesplots[[1]] + p2$bayesplots[[2]]
pp2


plot_post_8 <- plot(
  m_cluster_st,
  variable = c(
    "b_Intercept",
    "b_As1",
    "b_Wave",
    "b_As1:Wave",
    "b_sigma_As0",
    "b_sigma_As1",
    "sd_Id__As0",
    "sd_Id__As1"
  ),
  plot = F,
  newpage = F,
  N = 8
)
plot_post_8


library(patchwork)
library(ggplot2)
trace_plots <- plot_post_8[[1]]
p_trace_plots <-
  trace_plots$bayesplots[[1]] + trace_plots$bayesplots[[2]]
u_trace_plots <-
  p_trace_plots + labs(title = "MCMC posterior locations + chains")
u_trace_plots

saveRDS(p_trace_plots,
        here::here( "mods", "trace_plots"))

ggsave(
  u_trace_plots,
  path = here::here(here::here( "figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "u_trace_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# ppchecks # assumptions not met
pp_check(m_cluster_st, nsamples = 20)


# sensitivity anlaysis ----------------------------------------------------
# imagine strong time effect


priorS = c(
  prior(normal(0.2, 0.25), class = b, coef = "As1"),
  set_prior("constant(0.12)", class = "b", coef = "Wave"),
  prior(normal(0, 0.25),  class = b, coef = "As1:Wave"),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As0",
    dpar = "sigma"
  ),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As1",
    dpar = "sigma"
  ),
  prior(student_t(3, 4.1, 1), class = Intercept),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As0",
    group = "Id"
  ),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As1",
    group = "Id"
  )
)

b_sens_cluster <- brms::brm(
  bform,
  data = ameliadata,
  prior = priorS,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 1,
  future = TRUE,
  # backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("mods", "b_sens_cluster")
)

prior_summary(b_sens)

summary_b_sens_cluster <- summary(b_sens_cluster)
summary(b_sens_cluster)



tab_bsense_cluster <-
  lazerhawk::brms_SummaryTable(b_sens_cluster, panderize = F)

tab_bsense_cluster %>%
  kable(booktabs = T,
        "latex",
        caption =  "Sensitivity for effect of attack on warmth to Muslims: baseline trajectory fixed 2 $\times$ stronger than estimate",
        digits = 2) %>%
  print()


b_sens_cluster <-
  plot(conditional_effects(
    b_sens_cluster,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))
#saveRDS(b_sens_cluster, here::here( "mods", "b_sens_cluster"))
b_sens_cluster <- readRDS(here::here("mods", "b_sens_cluster"))



sens_plot <-
  b_sens_cluster$`Wave:As`  + scale_y_continuous(limits = c(4.0, 4.5)) +
  labs(subtitle = "",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  theme_classic()



# plot sensitivity --------------------------------------------------------


sens_plot_title <-
  cluster_plot_st + sens_plot + labs(title = "Sensitivity analysis: predicted marginal means for prejudice by attack condition",
                                     subtitle = "Assuming 2 x baseline acceptance growth rate does not diminish post-attack growth rate")
sens_plot_title

dev.off()

ggsave(
  sens_plot_title,
  path = here::here(here::here("mods")),
  width = 16,
  height = 9,
  units = "in",
  filename = "sens_plot_title.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

# comparative graph bayesian with REML ------------------------------------


plot_bayes_3  <-
  m1_test_model_plot$`Wave:As`  + scale_y_continuous(limits = c(4.0, 4.5)) +
  labs(subtitle = "Bayesian MI from six previous waves + full attack wave",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .8) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()



# impute 4 waves ----------------------------------------------------------








# df 4 for estimating time trajectory in attack sample ---------------------------
km_all4 <- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    EthnicCats,
    Employed,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    Religious,
    GenCohort,
    Urban,
    TSCORE,
    Partner,
    Parent,
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(Wave == 2017 |
                  Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2017 =  ifelse(Wave == 2017 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2017, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2017", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b) %>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave == 2018,
    TSCORE_b + 365,
    ifelse(
      YearMeasured == 0 & Wave == 2019,
      TSCORE_b + 730,
      ifelse(YearMeasured == 0 &
               Wave == 2020, TSCORE_b + 1095, TSCORE)
    )
  )) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 &
       Wave == 2018) |
      (Wave == 2019 |
         Wave == 2020), 1, 0
  )))) %>% # All 2019s even if NA need to be 1
  ungroup() %>%  # Create TSCORE for when people would have been observed
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i))) %>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
  dplyr::mutate(pol_bz = if_else(Wave == "2017", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2017", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2017", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2017", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2017", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2017", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2017", (as.numeric(Male)) / 2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2017", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2017", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2017", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2017", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>%
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup() %>%
  arrange(Id, Wave)

levels(km_all4$Wave) <- c("Time9", "Time10", "Time11", "Time12")


# tw<-km_all4%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check
sum(is.na(km_all4$TSCORE_i))



# correct
table1::table1( ~ TSCORE_i | Wave, data = km_all4, overall = FALSE)

# latex summary
kable(x, format = "latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1( ~ Warm.Muslims |
                  Wave * as.factor(Attack),
                data = km_all4,
                overall = FALSE)

obtl <-
  table1::table1( ~ Warm.Muslims |
                    as.factor(Attack) * Wave,
                  data = km_all4,
                  overall = FALSE)


kable(obtl, format = "latex", booktabs = TRUE)

x <- table1::table1(
  ~  Age + Edu + Employed + EthnicCats  +
    Male + NZdep + Parent + Partner +
    Religious + Pol.Orient  + Urban |
    Wave,
  data = km_all4,
  overall = FALSE
)
x

t1kable(x, format = "latex")

# create new data set
library(tidyverse)
kf4  <- km_all4 %>%
  group_by(Id) %>%  # All ZEROS
  # filter(Wave != "Time9")%>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (
    ifelse(
      Wave == "Time10" & Attack == 1 | Wave == "Time11" |
        Wave == "Time12",
      0,
      ifelse(Wave == "Time10" &
               Attack == 0 | Wave == "Time9", 1, Attack)
    )
  )) %>%
  mutate(Ys = ifelse(
    Wave == "Time10" & Attack == 1 | Wave == "Time11" |
      Wave == "Time12",
    NA,
    ifelse(Wave == "Time10" &
             Attack == 0 | Wave == "Time9", NA, Warm.Muslims)
  )) %>%
  ungroup() %>%
  arrange(Id, Wave)

# dat_all N
length(unique(kf4$Id))
str(kf4$As)
# correct


# Test NAs = Correct
table1::table1( ~ Ys |
                  Wave * as.factor(As), data = kf4, overall = F)



## Bind - double dataset to creat missing values
ka4 <- km_all4 %>%
  bind_rows(kf4) %>%
  arrange(Id, Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka4$Ys)
# Check

# Missing data problem
t2 <- table1::table1(~ Ys | Wave * As, data = ka4, overall = F)
t2

t1kable(t2, format = "latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link 4 dfs for zero estimate -----------------------------------------

km_zero4 <- ka4 %>%
  filter((As == 0)) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    wave
  ) %>%
  arrange(Wave, Id)

str(km_zero4$As)
head(km_zero4)
dim(km_zero4)
summary(km_zero4$wave)
summary(km_zero4$Wave)

summary(km_zero4$dys)

# works
table1::table1( ~ Ys | Wave * As, data = km_zero4, overall = FALSE)

# correct
table1::table1( ~ Ys | Wave * As, data = km_zero4, overall = FALSE)


# Impute 4 0s ------------------------------------------------------------


bind_zero4 <- as.data.frame(km_zero4)
head(bind_zero4)
library(Amelia)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.
# assume Y^0|A=2018 = Y^0 2019


# limit Ys to range 1 to 7
# find col number
(bind_zero4)

# create bounds
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_4 <- amelia(
  set.seed = 1234,
  bind_zero4,
  cs = c("Id"),
  ts = c("wave"),
  # use days
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero4)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_4, here::here( "mods", "imputed0_4"))


# check means, looks good!

table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_4$imputations$imp10,
                overall = FALSE)


# impute 4 1s ---------------------------------------------------------------

km_one4 <- ka4 %>%
  filter((As == 1 & Wave != "Time9")) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    wave # include wave
  ) %>%
  mutate(wave = wave - min(wave),
         dys = dys - min(dys)) %>%
  arrange(Wave, Id)

# check
length(unique(km_one4$Id))
summary(km_one4$wave)
summary(km_one4$dys)
hist(km_one4)


#check looks good
table1::table1( ~ Ys | Wave * As, data = km_one4, overall = FALSE)

# make data frame
km_one4 <- as.data.frame(km_one4)

# create bounds for Ys
head(km_one4)

bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1_4 <- amelia(
  set.seed = 1234,
  km_one4,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(km_one4)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1_4, here::here( "mods", "imputed1_4"))

# check means, looks good

table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_4$imputations$imp10,
                overall = FALSE)




# 4 make frames compatible --------------------------------------------------
imputed0_4 <-
  readRDS(here::here( "mods", "imputed0_4"))
imputed1_4 <-
  readRDS(here::here( "mods", "imputed1_4"))




# bind data frames --------------------------------------------------------
# levels_old <- c("Time4","Time5","Time6","Time7","Time8","Time9",
#                 "Time10","Time11","Time12")
# newlevels = c("Time10","Time11","Time12")

m <- 10
zero4 <- NULL
for (i in 1:m) {
  zero4$imputations$imp[[i]] <- imputed0_4$imputations[[i]] %>%
    dplyr::filter(Wave != "Time9") %>%
    droplevels() %>%
    arrange(Wave, Id)
}

one4 <- NULL

for (i in 1:m) {
  one4$imputations$imp[[i]] <- imputed1_4$imputations[[i]] %>%
    dplyr::filter(Wave != "Time9") %>%
    droplevels() %>%
    arrange(Wave, Id)
}

zero4
m <- 10
imps_bind4 <- NULL
for (i in 1:m) {
  imps_bind4$imputations$imp[[i]] <-
    dplyr::bind_rows(zero4$imputations$imp[[i]],
                     one4$imputations$imp[[i]]) %>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    dplyr::mutate(Wave = as.numeric(Wave) - 1) %>%
    dplyr::mutate(days_n = dys - min(dys)) %>% # new zero
    dplyr::mutate(yrs =  days_n / 365) %>% # new
    droplevels() %>%
    arrange(Wave, Id)
}



# Works
hist(imps_bind4$imputations$imp[[1]]$yrs)
table(imps_bind4$imputations$imp[[1]]$Wave)


# save
saveRDS(imps_bind4, here::here( "mods", "imps_bind4"))
#imps_bind4 <- readRDS(here::here( "mods", "imps_bind4"))

# read
imps_bind4 <-
  readRDS(here::here( "mods", "imps_bind4"))

# make list for bayesian models
listbayes4 <- imps_bind4$imputations$imp

# save list for bayesian models
saveRDS(listbayes4, here::here( "mods", "listbayes4"))

#readRDS
#listbayes4<- readRDS(here::here( "mods", "listbayes4"))


# ML model 4 ----------------------------------------------------------------


# model
m <- 10
model_all4 <- NULL
for (i in 1:m) {
  model_all4$model[[i]] <- lmer(Ys ~ As * Wave + (1 | Id),
                                data = imps_bind4$imputations$imp[[i]])
}

# table
tab <- pool_parameters(model_all4$model)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)



# Kurz model

fitted_lines4 <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all4$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()


==  ==  ==  =


  # Kurz model

  fitted_lines4 <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all4$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()


>  >  >  >  >  >  > 9f5a16fc41e9927370908b86e724d0204590a0ed

plot_4 <- fitted_lines4 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.5)) +<  <  <  <  <  <  < HEAD
labs(subtitle = "Multiple imputation: sample from previous 1 wave prior to attack + 2 waves post-attack",
     y = "Muslim Warmth",
     x = "Years: 2017-2020/21; N = 17014") + scale_colour_okabe_ito(alpha =
                                                                      .5)
==  ==  ==  =
  labs(subtitle = "MI from one previous wave",
       y = "Muslim Warmth",
       x = "Years: 2017-2020/21; N = 17014") +   scale_colour_npg(alpha =
                                                                    .5) +  theme_classic() #+
#scale_colour_okabe_ito(alpha =.5)
>  >  >  >  >  >  > 9f5a16fc41e9927370908b86e724d0204590a0ed
plot_4


ggsave(
  plot_4,
  path = here::here(here::here( "figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_4.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)

# impute 5 wave -----------------------------------------------------------



# df 5 for estimating time trajectory in attack sample ---------------------------
km_all5 <- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    EthnicCats,
    Employed,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    Religious,
    GenCohort,
    Urban,
    TSCORE,
    Partner,
    Parent,
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(Wave == 2016 |
                  Wave == 2017 |
                  Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2016", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b) %>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave == 2017,
    TSCORE_b + 365,
    ifelse(
      YearMeasured == 0 & Wave == 2018,
      TSCORE_b + 730,
      ifelse(
        YearMeasured == 0 & Wave == 2019,
        TSCORE_b + 1094,
        # leap
        ifelse(YearMeasured == 0 &
                 Wave == 2020, TSCORE_b + 1459, TSCORE)
      )
    )
  )) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 &
       Wave == 2018) |
      (Wave == 2019 |
         Wave == 2020), 1, 0
  )))) %>% # All 2019s even if NA need to be 1
  ungroup() %>%  # Create TSCORE for when people would have been observed
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i))) %>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
  dplyr::mutate(pol_bz = if_else(Wave == "2016", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2016", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2016", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2016", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2016", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2016", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2016", (as.numeric(Male)) / 2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2016", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2016", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2016", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2016", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>%
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup() %>%
  arrange(Id, Wave)

levels(km_all5$Wave) <-
  c("Time8", "Time9", "Time10", "Time11", "Time12")


# tw<-km_all5%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check



# correct
table1::table1( ~ Ys | Wave*As, data = km_all5, overall = FALSE)

# latex summary
kable(x, format = "latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1( ~ Warm.Muslims |
                  Wave * as.factor(Attack),
                data = km_all5,
                overall = FALSE)

obtl <-
  table1::table1( ~ Warm.Muslims |
                    as.factor(Attack) * Wave,
                  data = km_all5,
                  overall = FALSE)


kable(obtl, format = "latex", booktabs = TRUE)

x <- table1::table1(
  ~  Age + Edu + Employed +
    EthnicCats  + Male + NZdep +
    Parent + Partner + Religious +
    Pol.Orient  + Urban | Wave,
  data = km_all5,
  overall = FALSE
)
x

t1kable(x, format = "latex")

# create new data set
library(tidyverse)
kf5  <- km_all5 %>%
  group_by(Id) %>%  # All ZEROS
  # filter(Wave != "Time9")%>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (
    ifelse(
      Wave == "Time10" & Attack == 1 | Wave == "Time11" |
        Wave == "Time12",
      0,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8",
        1,
        Attack
      )
    )
  )) %>%
  mutate(Ys = ifelse(
    Wave == "Time10" & Attack == 1 |
      Wave == "Time11" |
      Wave == "Time12",
    NA,
    ifelse(
      Wave == "Time10" & Attack == 0 |
        Wave == "Time9" |
        Wave == "Time8",
      NA,
      Warm.Muslims
    )
  )) %>%
  ungroup() %>%
  arrange(Id, Wave)

# dat_all N
length(unique(kf5$Id))
str(kf5$As)
# correct


# Test NAs = Correct
table1::table1( ~ Ys |
                  Wave * as.factor(As), data = kf5, overall = F)



## Bind - double dataset to creat missing values
ka5 <- km_all5 %>%
  bind_rows(kf5) %>%
  arrange(Id, Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka5$Ys)
# Check

# Missing data problem
t2 <- table1::table1(~ Ys | Wave * As, data = ka5, overall = F)
t2

t1kable(t2, format = "latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link 5 dfs for zero estimate -----------------------------------------

km_zero5 <- ka5 %>%
  filter((As == 0)) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    wave
  ) %>%
  arrange(Wave, Id)

str(km_zero5$As)
head(km_zero5)
dim(km_zero5)
summary(km_zero5$wave) # Correct
# works
table1::table1( ~ Ys | Wave * As, data = km_zero5, overall = FALSE)

# correct
table1::table1( ~ Ys | Wave * As, data = km_zero5, overall = FALSE)


# Impute 5 0s ------------------------------------------------------------


bind_zero5 <- as.data.frame(km_zero5)
library(Amelia)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.
# assume Y^0|A=2018 = Y^0 2019


# limit Ys to range 1 to 7
# find col number
head(bind_zero5)

# create bounds
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_5 <- amelia(
  set.seed = 1234,
  bind_zero5,
  cs = c("Id"),
  ts = c("wave"),
  # use days
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero5)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_5, here::here( "mods", "imputed0_5"))


# check means, looks good

table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_5$imputations$imp10,
                overall = FALSE)


# impute 5 1s ---------------------------------------------------------------

km_one5 <- ka5 %>%
  filter((As == 1 & Wave == "Time10") |
           (As == 1 & Wave == "Time11") |
           (As == 1 & Wave == "Time12")) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    wave # include wave
  ) %>%
  mutate(dys = dys - min(dys),
         wave = wave - min(wave)) %>%
  arrange(Wave, Id)

summary(km_one5$wave)
summary(km_one5$dys)

length(unique(km_one5$Id))

#check looks good
table1::table1( ~ Ys | Wave * As, data = km_one5, overall = FALSE)

# make data frame
km_one5 <- as.data.frame(km_one5)

# create bounds for Ys
head(km_one5)
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1_5 <- amelia(
  set.seed = 1234,
  km_one5,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(km_one5)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1_5, here::here( "mods", "imputed1_5"))

# check means, looks good

table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_5$imputations$imp10,
                overall = FALSE)




# 5 make frames compatible --------------------------------------------------
# imputed0_5 <-
#   readRDS(here::here( "mods", "imputed0_5"))
# imputed1_5 <-
#   readRDS(here::here( "mods", "imputed1_5"))




# bind data frames --------------------------------------------------------
# levels_old <- c("Time4","Time5","Time6","Time7","Time8","Time9",
#                 "Time10","Time11","Time12")
# newlevels = c("Time10","Time11","Time12")

m <- 10
zero5 <- NULL
for (i in 1:m) {
  zero5$imputations$imp[[i]] <- imputed0_5$imputations[[i]] %>%
    dplyr::filter(Wave != "Time9" | Wave != "Time8") %>%
    droplevels() %>%
    arrange(Wave, Id)
}

one5 <- NULL

for (i in 1:m) {
  one5$imputations$imp[[i]] <- imputed1_5$imputations[[i]] %>%
    dplyr::filter(Wave != "Time9" | Wave != "Time8") %>%
    droplevels() %>%
    arrange(Wave, Id)
}


m <- 10
imps_bind5 <- NULL
for (i in 1:m) {
  imps_bind5$imputations$imp[[i]] <-
    dplyr::bind_rows(zero5$imputations$imp[[i]],
                     one5$imputations$imp[[i]]) %>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    dplyr::mutate(Wave = as.numeric(Wave) - 3) %>%
    dplyr::mutate(days_n = dys - min(dys)) %>% # new zero
    dplyr::mutate(yrs =  days_n / 365) %>% # new yzer
    droplevels() %>%
    arrange(Wave, Id)
}



# Works
hist(imps_bind5$imputations$imp[[1]]$yrs)
summary(imps_bind5$imputations$imp[[1]]$Wave)
# save
saveRDS(imps_bind5, here::here( "mods", "imps_bind5"))

# read
#imps_bind5 <- readRDS(here::here( "mods", "imps_bind5"))

imps_bind5 <-
  readRDS(here::here( "mods", "imps_bind5"))

# make list for bayesian models
listbayes5 <- imps_bind5$imputations$imp

# save list for bayesian models
#saveRDS(listbayes5, here::here( "mods", "listbayes5"))

#readRDS
#listbayes5<- readRDS(here::here( "mods", "listbayes5"))

table((imps_bind5$imputations$imp[[1]]$Ys) ==head(imps_bind5$imputations$imp[[2]]$Ys))

# ML model 5 ----------------------------------------------------------------

# model
m <- 10
model_all5 <- NULL
for (i in 1:m) {
  model_all5$model[[i]] <- lmer(Ys ~ As * Wave + (1 | Id),
                                data = imps_bind5$imputations$imp[[i]])
}

# table
tab <- pool_parameters(model_all5$model)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)


# Kurz model

fitted_lines5 <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all5$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()



plot_5 <- fitted_lines5 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.5))

plot_5


ggsave(
  plot_5,
  path = here::here(here::here( "mods")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_5.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



# bayesian model 2016-2021 ----------------------------------------------------------

## put data in list
imps_bind5$imputations$imp[[1]]%>%
  select(Id, As, Ys, Wave) %>%
  mutate(post_1 = lead(Ys, n = 1)) %>%
  mutate(post_2 = lead(Ys, n= 2)) %>%
  dplyr::filter(Wave == 0) %>%
  rename(baseline = Ys) %>%
  dplyr::select(-Wave) %>%
  head()

  mutate(Wave = factor(Wave, labels = c("baseline","post1","post2")))%>%
  pivot_wider(id_cols = "Id", values_from = c("Ys"), names_from= c("As","Wave")) %>%
  head()

imps_bind5$imputations$imp[[1]]%>%


# make list
imp1w <- as.data.frame(imps_bind5$imputations$imp[[1]])
imp2w <- as.data.frame(imps_bind5$imputations$imp[[2]])
imp3w <- as.data.frame(imps_bind5$imputations$imp[[3]])
imp4w <- as.data.frame(imps_bind5$imputations$imp[[4]])
imp5w <- as.data.frame(imps_bind5$imputations$imp[[5]])
imp6w <- as.data.frame(imps_bind5$imputations$imp[[6]])
imp7w <- as.data.frame(imps_bind5$imputations$imp[[7]])
imp8w <- as.data.frame(imps_bind5$imputations$imp[[8]])
imp9w <- as.data.frame(imps_bind5$imputations$imp[[9]])
imp10w <- as.data.frame(imps_bind5$imputations$imp[[10]])




ameliadataw <-
  list(imp1w, imp2w, imp3w, imp4w, imp5w, imp6w, imp7w, imp8w, imp9w, imp10w)




bform =   bf(Ys ~ As  *  Wave + (0 + As || Id),
             sigma ~ 0 + As, set_rescor(rescor = FALSE))

prior_strong_even2 = c(
  set_prior("normal(0, 0.25)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior(
    "student_t(3, 4, 1)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")
)


system.time(
  m_fivewaves  <- brms::brm_multiple(
    bform,
    family = gaussian,
    data = ameliadataw,
    prior = prior_strong_even2,
    #seed = 1234,
    init = 0,
    warmup = 1000,
    iter =  2000,
    chains = 2,
    backend = "cmdstan",
    file = here::here("mods", "m_fivewaves.rds")
  )
)





#
#
# bayes_5 <- conditional5$`Wave:As` +   scale_y_continuous(limits=c(4.0,4.48)) +
#   labs(subtitle="Multiple imputation: sample from previous 2 waves prior to attack + 2 waves post-attack",
#        y= "Muslim Warmth",
#        x = "Years: 2016-2020/21; N=21796") + scale_colour_okabe_ito(alpha =.5)
#
# #scale_color_viridis_d(option = "cividis")
# bayes_5


#
#
#
# ggsave(
#   bayes_5,
#   path = here::here(here::here( "figs")),
#   width = 12,
#   height =9,
#   units = "in",
#   filename = "bayes_5x.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1000
# )
#
#


# IMPUTE 9wave ---------------------------------------------------------------
# df 9 for estimating time trajectory in attack sample ---------------------------
df$Warm.Muslims

km_all9 <- df %>%
  dplyr::select(
    Id,
    Age,
    Wave,
    EthnicCats,
    Employed,
    Urban,
    Edu,
    Male,
    Pol.Orient,
    NZdep,
    Religious,
    GenCohort,
    Urban,
    TSCORE,
    Partner,
    Parent,
    # Warm.Overweight,
    # Warm.Elderly,
    # Warm.MentalIllness,
    Warm.Muslims,
    # Warm.Immigrants,
    # Warm.Asians,
    # Warm.Refugees,
    # Wave,
    # Warm.Maori,
    # Warm.NZEuro,
    # Warm.Indians,
    # Warm.Chinese,
    # Warm.Refugees,
    # Warm.Pacific,
    YearMeasured
  ) %>%
  dplyr::filter(
    Wave == 2012 |
      Wave == 2013 |
      Wave == 2014 |
      Wave == 2015 |
      Wave == 2016 |
      Wave == 2017 |
      Wave == 2018 |
      Wave == 2019 |
      Wave == 2020
  ) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2012", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b) %>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave == 2013,
    TSCORE_b + 365,
    ifelse(
      YearMeasured == 0 & Wave == 2014,
      TSCORE_b + 730,
      ifelse(
        YearMeasured == 0 & Wave == 2015,
        TSCORE_b + 1094,
        # leap
        ifelse(
          YearMeasured == 0 & Wave == 2016,
          TSCORE_b + 1459,
          ifelse(
            YearMeasured == 0 & Wave == 2017,
            TSCORE_b + 1824,
            ifelse(
              YearMeasured == 0 & Wave == 2018,
              TSCORE_b + 3248,
              ifelse(
                YearMeasured == 0 & Wave == 2019,
                TSCORE_b + 3512,
                ifelse(YearMeasured == 0 &
                         Wave == 2020, TSCORE_b + 3877, TSCORE)
              )
            )
          )
        )
      )
    )
  )) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 &
       Wave == 2018) |
      (Wave == 2019 |
         Wave == 2020), 1, 0
  )))) %>% # All 2019s even if NA need to be 1
  ungroup() %>%  # Create TSCORE for when people would have been observed
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i))) %>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
  dplyr::mutate(pol_bz = if_else(Wave == "2012", (Pol.Orient), NA_real_)) %>%
  fill(pol_bz) %>%
  dplyr::mutate(rel_bz = if_else(Wave == "2012", (as.numeric(Religious)), NA_real_)) %>%
  fill(rel_bz) %>%
  dplyr::mutate(partner_bz = if_else(Wave == "2012", (as.numeric(Partner)), NA_real_)) %>%
  fill(partner_bz) %>%
  dplyr::mutate(parent_bz = if_else(Wave == "2012", (as.numeric(Parent)), NA_real_)) %>%
  fill(parent_bz) %>%
  dplyr::mutate(age_bz = if_else(Wave == "2012", (Age), NA_real_)) %>%
  fill(age_bz) %>%
  dplyr::mutate(nzdep_bz = if_else(Wave == "2012", (NZdep), NA_real_)) %>%
  fill(nzdep_bz) %>%
  dplyr::mutate(male_2z = if_else(Wave == "2012", (as.numeric(Male)) / 2, NA_real_)) %>%
  fill(male_2z) %>%
  dplyr::mutate(employed_bz = if_else(Wave == "2012", (as.numeric(Employed)), NA_real_)) %>%
  fill(employed_bz) %>%
  dplyr::mutate(edu_bz = if_else(Wave == "2012", (Edu), NA_real_)) %>%
  fill(edu_bz) %>%
  dplyr::mutate(ubran_bz = if_else(Wave == "2012", (as.numeric(Urban)), NA_real_)) %>%
  fill(ubran_bz) %>%
  dplyr::mutate(EthnicCats_b = if_else(Wave == "2012", as.numeric(EthnicCats), NA_real_)) %>%
  fill(EthnicCats_b) %>%
  dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
  ungroup() %>%
  arrange(Wave, Id)
str(km_all9$Wave)
levels(km_all9$Wave) <-
  c("Time4",
    "Time5",
    "Time6",
    "Time7",
    "Time8",
    "Time9",
    "Time10",
    "Time11",
    "Time12")


# tw<-km_all4%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check
sum(is.na(km_all9$TSCORE_i))



# correct
table1::table1( ~ Ys | Wave, data = km_all9, overall = FALSE)

# latex summary
kable(x, format = "latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1( ~ Warm.Muslims |
                  Wave * as.factor(Attack),
                data = km_all9,
                overall = FALSE)

table1::table1( ~ Warm.Muslims |
                  Wave * as.factor(As),
                data = km_all9,
                overall = FALSE)

kable(obtl, format = "latex", booktabs = TRUE)


x <-
  table1::table1(
    ~  Age + Edu + Employed + EthnicCats  + Male + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban |
      Wave,
    data = km_all9,
    overall = FALSE
  )
x

t1kable(x, format = "latex")

table1::table1( ~  as.factor(Warm.Muslims) |
                  Wave * as.factor(As) , data = km_all9)
km_all9
# create new data set
library(tidyverse)
kf9  <- km_all9 %>%
  group_by(Id) %>%  # All ZEROS
  # filter(Wave != "Time9")%>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = ifelse(
    Wave == "Time10" & Attack == 1 |
      Wave == "Time11" |
      Wave == "Time12",
    0,
    ifelse(
      Wave == "Time10" & Attack == 0 |
        Wave == "Time9" & Attack == 0 |
        Wave == "Time8" & Attack == 0 |
        Wave == "Time7" & Attack == 0 |
        Wave == "Time6" & Attack == 0 |
        Wave == "Time5" & Attack == 0 |
        Wave == "Time4" & Attack == 0,
      1,
      Attack
    )
  )) %>%
  mutate(
    Ys = ifelse(
      Wave == "Time9" & Attack == 1 |
        Wave == "Time10" & Attack == 1 |
        Wave == "Time11" | Wave == "Time12",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4" ,
        NA,
        Warm.Muslims
      )
    )
  ) %>%
  ungroup() %>%
  arrange(Wave, Id)

# dat_all N
length(unique(kf9$Id))
str(kf9$As)
# correct


# Test NAs = Correct
table1::table1( ~ (Ys) | Wave * as.factor(As),
                data = kf9,
                overall = F)



## Bind - double dataset to creat missing values
ka9 <- km_all9 %>%
  bind_rows(kf9) %>%
  arrange(Wave, Id) %>%
  mutate(As = as.factor(As)) %>%
  arrange(Wave, Id)
# select(-c(Warm.Muslims,Attack))

head(ka9$Ys)
# Check

# Missing data problem
t2 <-
  table1::table1(~ Ys |
                   Wave * as.factor(As), data = ka9, overall = F)
t2

t1kable(t2, format = "latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link 9 dfs for zero estimate -----------------------------------------

km_zero9 <- ka9 %>%
  filter((As == 0)) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    wave
  ) %>%
  arrange(Wave, Id)

str(km_zero9$As)
head(km_zero9)
dim(km_zero9)
summary(km_zero9$wave)
# works
table1::table1( ~ Ys | Wave * As, data = km_zero9, overall = FALSE)

# correct
table1::table1( ~ Ys | Wave * As, data = km_zero9, overall = FALSE)


# Impute 9 0s ------------------------------------------------------------


bind_zero9 <- as.data.frame(km_zero9)

library(Amelia)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.
# assume Y^0|A=2018 = Y^0 2019


# limit Ys to range 1 to 7
# find col number
head(bind_zero9)

# create bounds
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_9 <- amelia(
  set.seed = 1234,
  bind_zero9,
  cs = c("Id"),
  ts = c("wave"),
  # use days
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial, if 3, reverts to pre w10 mean!
  intercs = F,
  # too many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero9)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_9, here::here( "mods", "imputed0_9"))

# check means, looks good

table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_9$imputations$imp10,
                overall = FALSE)


# impute 9 1s ---------------------------------------------------------------
km_one9 <- ka9 %>%
  dplyr::filter(# (As == 1 & Wave =="Time4")|
    #  (As == 1 & Wave =="Time5")|
    # (As == 1 & Wave =="Time6")|
    # (As == 1 & Wave =="Time7")|
    # (As == 1 & Wave =="Time8")|
    #  (As == 1 & Wave =="Time9")|
    (As == 1 & Wave == "Time10") |
      (As == 1 & Wave == "Time11") |
      (As == 1 & Wave == "Time12")) %>%
  droplevels() %>%
  mutate(wave = wave - 6) %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    wave # include wave
  ) %>%
  arrange(Wave, Id)
str(km_one9)
table(ka9$Wave)
summary(km_one9$wave)


#check looks good
table1::table1( ~ Ys | Wave * As, data = km_one9, overall = FALSE)

# make data frame
km_one9 <- as.data.frame(km_one9)
km_one9
# create bounds for Ys
head(km_one9)
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1_9 <- amelia(
  set.seed = 1234,
  km_one9,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial, if 3, reverts to pre w10 mean!
  intercs = F,
  # too many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero9)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1_9, here::here( "mods", "imputed1_9"))


# check means, looks good

table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed1_9$imputations$imp10,
                overall = FALSE)




# 9 make frames compatible --------------------------------------------------
imputed0_9 <-
  readRDS(here::here( "mods", "imputed0_9"))
imputed1_9 <-
  readRDS(here::here( "mods", "imputed1_9"))




# bind data frames --------------------------------------------------------
# levels_old <- c("Time4","Time5","Time6","Time7","Time8","Time9",
#                 "Time10","Time11","Time12")
# newlevels = c("Time10","Time11","Time12")

m <- 10
zero9 <- NULL
for (i in 1:m) {
  zero9$imputations$imp[[i]] <- imputed0_9$imputations[[i]] %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    droplevels() %>%
    arrange(Wave, Id)
}

one9 <- NULL

for (i in 1:m) {
  one9$imputations$imp[[i]] <- imputed1_9$imputations[[i]] %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    droplevels() %>%
    arrange(Wave, Id)
}


m <- 10
imps_bind9 <- NULL
for (i in 1:m) {
  imps_bind9$imputations$imp[[i]] <-
    dplyr::bind_rows(zero9$imputations$imp[[i]],
                     one9$imputations$imp[[i]]) %>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    dplyr::mutate(Wave = as.numeric(Wave) - 1) %>%
    dplyr::mutate(days_n = dys - min(dys)) %>% # new zero
    dplyr::mutate(yrs =  days_n / 365) %>% # new yzer
    droplevels() %>%
    arrange(Wave, Id)
}



# Works
hist(imps_bind9$imputations$imp[[1]]$Wave)

# save
saveRDS(imps_bind9, here::here( "mods", "imps_bind9"))

# read
imps_bind9 <-
  readRDS(here::here( "mods", "imps_bind9"))

# make list for bayesian models
listbayes9 <- imps_bind9$imputations$imp

# save list for bayesian models
saveRDS(listbayes9, here::here( "mods", "listbayes9"))

#readRDS
#listbayes9<- readRDS(here::here( "mods", "listbayes9"))


# ML model 9 ----------------------------------------------------------------
# model
m <- 10
model_all9 <- NULL
for (i in 1:m) {
  model_all9$model[[i]] <- lmer(Ys ~ As * Wave + (1 | Id),
                                data = imps_bind9$imputations$imp[[i]])
}

# table
tab <- pool_parameters(model_all9$model)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)

# Uncertainty graph

fitted_lines9 <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all9$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()


plot_9 <- fitted_lines9 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +
  scale_y_continuous(limits =  c(4.0, 4.5)) +
  labs(subtitle = "MI from previous six previous waves",
       y = "Muslim Warmth",
       x = "Waves: 2012-2020/21, N = 11799") +   scale_colour_npg(alpha = .5) + theme_classic() # +
scale_colour_okabe_ito(alpha = .5)


plot_9


ggsave(
  plot_9,
  path = here::here(here::here( "figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_9.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



# SENS sensitivity analysis delete known cases and recover with imputation method ---------------------------------

# link dataframe
library(tidyverse)
km_pre
nw_9 <- nw_9 %>%
  filter(As ==0) %>%
  select(-As)
km_pre

bind_zero_s <- full_join(km_pre, bind_zero5) %>%
  arrange(Wave, Id)

table(bind_zero_s$Wave)
# relevel
levels = c("Time4",
           "Time5",
           "Time6",
           "Time7",
           "Time8",
           "Time9",
           "Time10",
           "Time11",
           "Time12")

bind_zero_s$Wave <- fct_relevel(bind_zero_s$Wave, levels)
levels(bind_zero_s$Wave)
str(bind_zero_s$Wave)
head(bind_zero_s)


bind_zero1_s <- bind_zero_s %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  dplyr::select(-c(dys, yrs)) %>%  # not working
  arrange(Wave, Id) %>%
  group_by(Id) %>%
  mutate(dys = TSCORE_i - min(TSCORE_i)) %>% # Not used
  mutate(yrs = dys / 365) %>% # note used
  ungroup() %>%
  dplyr::filter(Wave == "Time4"|
           Wave == "Time5"|
           Wave == "Time6"|
           Wave == "Time7"|
           Wave == "Time8"|
           Wave == "Time9"|
           Wave == "Time10") %>%
  droplevels()


summary(bind_zero1_s$Wave)

table1::table1(~ Ys |Wave, data = bind_zero_s)

## Make numeric var

# correct


# now create df with zeros
# check
table1::table1(~ Ys | Wave, data = bind_zero1_s)

out <- lm(Ys ~ wave, bind_zero1_s)
summary(out)


new_s <- bind_zero1_s


# Replace obeserved values with NA
new_s$Ys[new_s$Wave == "Time9"] <- NA
new_s$Ys[new_s$Wave == "Time10"] <- NA


# check
table1::table1(~ Ys | Wave, data = bind_zero1_s)
table1::table1(~ Ys | Wave, data = new_s)

# make data frame'
nw_s <- new_s %>%
  dplyr::select(
    Id,
    Wave,
    wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs) %>%
  arrange(Id, Wave)
nw_s <- as.data.frame(nw_s)

head(nw_s)
match("Ys",names(nw_s))

# create bounds for Ys
bds <- matrix(c(5, 1, 7), nrow = 1, ncol = 3)

imputed_new_s <- amelia(
  set.seed = 1234,
  nw_s,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial, if 3,
  intercs = F,
  # too many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(nw_s)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed_new_s, here::here( "mods", "imputed_new_s.rds"))
imputed_new_s <- readRDS( here::here( "mods", "imputed_new_s.rds"))


# easier for brms
nine_dat_s<- miceadds::datlist2mids(imputed_new_s$imputations)
rm(nine_dat_s)

table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave ,
                data = imputed_new_s$imputations$imp10,
                overall = FALSE)

# original -- VERY CLOSE!!
table1::table1( ~ Ys |
                  Wave ,
                data = bind_zero1_s,
                overall = FALSE)



# prepare data
bind_zero1_sz <-
  bind_zero1_s %>% mutate(
    pol_bz = as.numeric(scale(pol_bz)),
    rel_bz = as.numeric(scale(rel_bz)),
    partner_bz = as.numeric(scale(partner_bz)),
    parent_bz = as.numeric(scale(parent_bz)),
    nzdep_bz = as.numeric(scale(nzdep_bz)),
    male_2z = as.numeric(scale(male_2z)),
    employed_bz = as.numeric(scale(employed_bz)),
    edu_bz = as.numeric(scale(edu_bz)) ,
    ubran_bz = as.numeric(scale(ubran_bz)) ,
    EthnicCats_b = as.numeric(factor(EthnicCats_b)),
    GenCohort = as.numeric(factor(GenCohort))
  )



imp_r <-
  brm(
    Ys ~  wave + EthnicCats_b + edu_bz + employed_bz +  pol_bz + male_2z +  nzdep_bz +
      parent_bz + partner_bz +  rel_bz + ubran_bz + GenCohort + (1 | Id),
    data = bind_zero1_sz,
    backend = "cmdstanr",
    file = here::here("mods", "imp_r.rds")
  )


imp_s <- brm_multiple(Ys ~  wave + (1|Id),  data = nine_dat_s, backend = "cmdstanr",
                      file = here::here("mods","imp_s.rds"))


summary(imp_r)
summary(imp_s)

tab_imp_r <-
  lazerhawk::brms_SummaryTable(imp_r, panderize = F)


# table in latex
tab_imp_r [1:2, ] %>%
  kable(booktabs = T,
        "latex",
        caption =  "Sensitivity anlysis: Marginal effect of attack on warmth to Muslims, observed",
        digits = 2) %>%
  print()


tab_imp_s <-
  lazerhawk::brms_SummaryTable(imp_s, panderize = F)


# table in latex
tab_imp_s %>%
  kable(booktabs = T,
        "latex",
        caption =  "Sensitivity anlysis: Marginal effect of attack on warmth to Muslims, imputed",
        digits = 2) %>%
  print()


sens_origA  <- conditional_effects(
    imp_r,
    "wave",
    ndraws = 100,
    spaghetti = T)#,

plot(sens_origA)[[1]] + scale_color_grey() +
  scale_fill_grey()

#points = T, alpha = .01,
#point_args = list(alpha = .005, width = .1))

saveRDS(sens_orig, here::here("mods", "sens_orig.rds"))
sens_orig<- readRDS(sens_orig, here::here("mods", "sens_orig.rds"))




sens_imp  <- plot(conditional_effects(imp_s, "wave",ndraws = 100,  spaghetti = T))




saveRDS(sens_imp, here::here("mods", "sens_imp.rds"))
sens_imp.rds<- readRDS(sens_orig, here::here("mods", "sens_imp.rds"))

#points = T, alpha = .01,
#point_args = list(alpha = .005, width = .1))
sessionInfo()
saveRDS(sens_imp, here::here("mods", "sens_imp.rds"))

# graph
sens_i_9  <- sens_imp$`wave` +   scale_y_continuous(limits=c(3.5,4.5)) + #scale_fill_manual(values=c("#CC6666")) +
  labs(title="Deleted and fully imputed model: b = .06",
       subtitle = "Years 2012-2018, N = 33,735 (2012 & 2016 Cohorts)",
       y= "Muslim Warmth",
       x = "Years: 2012-2018 (pre-attack)") + theme_classic() + scale_colour_okabe_ito(alpha =.5) +
  annotate(
    "rect",
    xmin = 4,
    xmax = 6,
    ymin = 3.5,
    ymax = 4.25,
    alpha = .1,
    fill = "red") +
  annotate("text",
           x = 5,
           y = 4.35,
           label = "Deleted & then\nmultiply-imputed")

sens_i_9
# Graph
library(scales)
sens_i_9a  <- sens_orig$`wave` +   scale_y_continuous(limits=c(3.5,4.5)) + #scale_fill_manual(values=c("#CC6666")) +
  labs(title="No imputation model: b = .06",
       subtitle = "Years 2012-2018, N = 33,735 (2012 & 2016 Cohorts)",
       y= "Muslim Warmth",
       x = "Years: 2012-2018 (pre-attack)") + theme_classic() + scale_colour_okabe_ito(alpha =.5)

sens_i_9a

comb_sens_graph<- sens_i_9a  + sens_i_9 + plot_annotation(tag_levels = "A")





comb_sens_graph

comb_sens_graph
ggsave(
  comb_sens_graph,
  path = here::here(here::here( "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "comb_sens_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


## additional graphs
posterior_r <- as.matrix(imp_r)
posterior <- as.matrix(imp_s)

p1 <- mcmc_areas(imp_r,
           pars = c("b_wave"),
           prob = 0.99)




saveRDS(p2, here::here("mods", "p2"))

p1 <- mcmc_intervals(imp_r, pars = c("b_wave", "sigma"))
p1

p1a <- mcmc_hist(posterior_r, pars = c("intercept",  "wave", "sigma"))

p2<- mcmc_intervals(posterior,
                pars = c("b_wave", "sigma"))


(sens_i_9a  + sens_i_9) + plot_annotation(tag_levels = "A")


# rdd graph ---------------------------------------------------------------

### Wave 10 Only
library(dplyr)
library()
sadi <- df %>%
  dplyr::filter((Wave == 2012 &  YearMeasured==1 )|
                  (Wave == 2013 &  YearMeasured==1 )|
                  (Wave == 2014 &  YearMeasured==1 )|
                  (Wave == 2015 &  YearMeasured==1 )|
                  (Wave == 2016 &  YearMeasured==1 )|
                  (Wave == 2017 &  YearMeasured==1 )|
                (Wave == 2018 &  YearMeasured==1 )|
                (Wave = 2019 & YearMeasured==1))%>%
  droplevels() %>%
  select(Warm.Muslims, TSCORE, Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr::filter(timeline > "2012-06-06") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(Attack_Condition = factor(
    ifelse(timeline < "2019-03-15", 0, 1),
    labels= c("Pre-Attack","Post-attack")))%>%
  arrange(timeline,Attack_Condition)

sadi <-as.data.frame(sadi)
head(sadi)
min(sadi$day)

ids <- df %>%
  dplyr::filter((Wave == 2012 &  YearMeasured==1 )|
                  (Wave == 2013 &  YearMeasured==1 )|
                  (Wave == 2014 &  YearMeasured==1 )|
                  (Wave == 2015 &  YearMeasured==1 )|
                  (Wave == 2016 &  YearMeasured==1 )|
                  (Wave == 2017 &  YearMeasured==1 )|
                  (Wave == 2018 &  YearMeasured==1 )|
                  (Wave = 2019 & YearMeasured==1))%>%
  group_by(Id,Wave) %>%
  select(Id, Wave)

length(unique(ids$Id))

# Overall
# (N=228406)
# Attack_Condition
# Baseline	140793 (61.6%)
# Post-attack	87613 (38.4%)

table1::table1( ~ Attack_Condition, data = sadi )

library(ggsci)
rdd <- ggplot(sadi, aes(x = timeline, y = Warm.Muslims, color = Attack_Condition)) +
  geom_jitter(alpha = .01, width = 1) +
  stat_smooth(method = "gam") +
  theme(legend.position = "bottom") +
  labs(title = "Discontinuity at attacks (GAM)",
         subtitle = "Strong & apparently growing increase in acceptace after attacks",
       y = "Muslim Warmth",
       x = "NZAVS Time 4 - 12 (2012-2021)") +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic()

# graph
rdd
# combine graph
library(patchwork)
comb_sens_graph3<- ldsA / rdd  + plot_annotation(tag_levels = "A")

# check
comb_sens_graph3

comb_sens_graph3


ggsave(
  comb_sens_graph3,
  path = here::here(here::here( "figs")),
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
  mutate(timeline = as.numeric(timeline) - min( as.numeric(timeline)))
table(sadi3$Attack_Condition)

# find lowest post-attack case
sadi3 %>% dplyr::filter(Attack_Condition == "Post-attack") %>%
  arrange(timeline, Id) %>%
  head()

# rdd
outr <- rdrobust(y = sadi3$Warm.Muslims, x= sadi3$timeline, c= 2468, all = TRUE)

# summary
summary(outr)


rdr_export <- function(rdr_out, prec = 3){
  outrows = c("coef", "se", "z", "ci", "bws", "N_h", "N_b")
  out = rdr_out[outrows]
  # conventional
  ids = 1
  CI = paste0("(", as.numeric(round(out$ci[ids, ][1], prec)),
              ",", as.numeric(round(out$ci[ids, ][2], prec)), ")")
  conventional = rbind(Coef = round(out$coef[ids], prec), SE = round(out$se[ids],
                                                                     prec), `t-stat` = round(out$z[ids], prec), CI)
  # robust
  ids = 3
  CI = paste0("(", as.numeric(round(out$ci[ids, ][1], prec)),
              ",", as.numeric(round(out$ci[ids, ][2], prec)), ")")
  robust = rbind(Coef = round(out$coef[ids], prec), SE = round(out$se[ids],
                                                               prec), `t-stat` = round(out$z[ids], prec), CI)
  common_rows = rbind(bw = paste0("(", round(out$bws[1, 1],
                                             prec), ",", round(out$bws[1, 2], prec), ")"), Nobs = paste0("(",
                                                                                                         out$N_h[1], ",", out$N_h[2], ")"), poly_order = rdr_out$p)
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
  select(-c(`Polynomial Order`, Bandwidth, Nobs))%>%
  slice(1:2)


# graph
rdplot(y = sadi3$Warm.Muslims, x= sadi3$timeline, c= 2468,
       title = "Robust Regression Discontinuity",
       x.label = "Days from start of Time 4 (2012)", y.label = "Warmth to Muslims (1-7)", x.lim = NULL, y.lim = NULL,
       col.dots = "dodgerblue",
       col.lines = NULL)




# another approach
yola
outr_simple <- gam( Warm.Muslims ~ Attack_Condition + s(timeline, bs = "cs"), data = sadi3)
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
  table1::table1( ~ Ys |
                    As * Wave,
                  data = bind_zero1_s,
                  overall = F,
                  transpose = F)

t1kable(zt_s, format = "latex")

zt_s






# impute stationary  ---------------------------------------------------------------

km_zero_st18 <- ka3 %>%
  filter((As == 0 &  Wave == "Time10")) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs,
    TSCORE_i
  ) %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  arrange(Wave, Id)


summary(km_zero_st18$Wave)



#check looks good
table1::table1(~ Ys |
                 Wave * As, data = km_zero_st18, overall = FALSE)
head(km_zero_st18)
dim(km_zero_st18)
# make data frame
km_zero_st18 <- as.data.frame(km_zero_st18)

# create bounds for Ys
head(km_zero_st18)
# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_st18 <- amelia(
  set.seed = 1234,
  km_zero_st18,
  # cs= c("Id"),
  # ts= c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys", "yrs", "TSCORE_i", "Id", "wave"),
  intercs = F,
  bounds = bds,
  empri = .05 * nrow(km_zero_st18)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_st18,
        here::here( "mods", "imputed0_st18"))

table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st18$imputations$imp10,
               overall = FALSE)



km_zero_st19 <- ka3 %>%
  filter((As == 0 &
            Wave == "Time10") |
           (As == 0 & Wave == "Time11")) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs,
    TSCORE_i
  ) %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  arrange(Wave, Id)



summary(km_zero_st19$wave)
#check looks good
table1::table1(~ Ys |
                 Wave * As, data = km_zero_st19, overall = FALSE)
head(km_zero_st19)
dim(km_zero_st19)


# make data frame
km_zero_st19 <- as.data.frame(km_zero_st19)

# create bounds for Ys
head(km_zero_st19)

# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_st19 <- amelia(
  set.seed = 1234,
  km_zero_st19,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys", "yrs", "TSCORE_i"),
  lags = "Ys",
  #leads="Ys",
  intercs = F,
  bounds = bds,
  empri = .05 * nrow(km_zero_st19)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_st19,
        here::here( "mods", "imputed0_st19"))

table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st19$imputations$imp10,
               overall = FALSE)



km_zero_st20 <- ka3 %>%
  filter((As == 0 &
            Wave == "Time10") |
           (As == 0 & Wave == "Time12")) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs,
    TSCORE_i
  ) %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  arrange(Wave, Id)





summary(km_zero_st20$wave)
#check looks good
table1::table1( ~ Ys |
                  Wave * As, data = km_zero_st20, overall = FALSE)
head(km_zero_st20)
dim(km_zero_st20)
# make data frame
km_zero_st20 <- as.data.frame(km_zero_st20)

# create bounds for Ys
head(km_zero_st20)
# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_st20 <- amelia(
  set.seed = 1234,
  km_zero_st20,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys", "yrs", "TSCORE_i"),
  lags = "Ys",
  #leads="Ys",
  intercs = F,
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(km_zero_st20)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_st20,
        here::here( "mods", "imputed0_st20"))


table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_st20$imputations$imp10,
               overall = FALSE)


## Get ones from above

imp1 <- transform(imputed1, Wave = as.character(Wave))
imp18z <- transform(imputed0_st18, Wave = as.character(Wave))
imp19z <- transform(imputed0_st19, Wave = as.character(Wave))
imp20z <- transform(imputed0_st20, Wave = as.character(Wave))

str(imp20z$imputations$imp1$Wave)

# bind data frames --------------------------------------------------------


newlevels = c("Time10", "Time11", "Time12")


m <- 10
one_st <- NULL
for (i in 1:m) {
  one_st$imputations$imp[[i]] <- imp1$imputations[[i]] %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, newlevels)) %>%
    droplevels() %>%
    dplyr::group_by(Id) %>%
    arrange(Wave, Id)
}

zero_st <- NULL

for (i in 1:m) {
  zero_st$imputations$imp[[i]] <-
    dplyr::bind_rows(imp18z$imputations[[i]],
                     imp19z$imputations[[i]],
                     imp20z$imputations[[i]]) %>%
    dplyr::mutate(Wave = as.factor(Wave)) %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, newlevels)) %>%
    arrange(Wave, Id)

}

m <- 10
imps_bind_st <- NULL
for (i in 1:m) {
  imps_bind_st$imputations$imp[[i]] <-
    dplyr::bind_rows(zero_st$imputations$imp[[i]],
                     one_st$imputations$imp[[i]]) %>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    droplevels() %>%
    dplyr::mutate(Wave = as.numeric(Wave) - 1) %>%
    dplyr::arrange(Wave, Id)


}

imps_bind_st

# Works
summary(imps_bind_st$imputations$imp[[1]]$Wave)

# save
saveRDS(imps_bind_st,
        here::here( "mods", "imps_bind_st"))


# read
imps_bind_st <-
  readRDS(here::here( "mods", "imps_bind_st"))

# make list for bayesian models
listbayesST <- imps_bind_st$imputations$imp

# save list for bayesian models
saveRDS(listbayesST,
        here::here( "mods", "listbayesST"))

#readRDS
listbayesST <-
  readRDS(here::here( "mods", "listbayesST"))


# ML model  stationary ----------------------------------------------------------------

# model
m <- 10
model_all_st <- NULL
for (i in 1:m) {
  model_all_st$model[[i]] <-
    lmer(Ys ~ As * Wave + (1 |Id), data = imps_bind_st$imputations$imp[[i]])
}

# table
tab <- pool_parameters(model_all_st)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)


# Average over uncertainty

fitted_linesST <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all_st$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()

plot_st <- fitted_linesST %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.5)) +
  labs(subtitle = "Multiple imputation: no previous waves",
       y = "Muslim Warmth",
       x = "Years: 2012-2020/21, N = 47948") + scale_colour_okabe_ito(alpha = .5)


plot_st


fitted_linesST <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all_st$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()


plot_st <- fitted_linesST %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.5)) +
  labs(subtitle = "MI: no previous waves",
       y = "Muslim Warmth",
       x = "Years: 2012-2020/21, N = 47948") +   scale_colour_npg(alpha =
                                                                    .5) +  theme_classic()#+
scale_colour_okabe_ito(alpha = .5)


plot_st



ggsave(
  plot_st,
  path = here::here(here::here( "mods")),
  # too large
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

# COMBINED GRAPH ----------------------------------------------------------


robust_waves <-
  ((plot_9 + plot_5) / (plot_4 +  plot_st)) + plot_annotation(title = "Postive trajectory in post-attack acceptance is robust to multiple imputation strategies",
                                                              subtitle = "Recovery of postive trajectory in counterfactual no-attack condition requires at least two waves prior to baseline",
                                                              tag_levels = "i") + plot_layout(guides = 'collect')


robust_waves

robust_waves2 <-
  plot_time + ((plot_9 + plot_5) / (plot_4 +  plot_st)) + plot_annotation(title = "Postive trajectory in post-attack acceptance is robust to multiple imputation strategies",
                                                                          subtitle = "Recovery of postive trajectory in counterfactual no-attack condition requires at least two waves prior to baseline",
                                                                          tag_levels = "i") + plot_layout(guides = 'collect')



ggsave(
  robust_waves,
  path = here::here(here::here( "mods")),
  width = 10,
  height = 9,
  units = "in",
  filename = "robust_waves.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)
ggsave(
  robust_waves2,
  path = here::here(here::here( "mods")),
  width = 16,
  height = 9,
  units = "in",
  filename = "robust_waves2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# z plot method --------------------------------------------------------
#https://solomonkurz.netlify.app/post/2021-10-21-if-you-fit-a-model-with-multiply-imputed-data-you-can-still-plot-the-line/




fitted_lines <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()



fitted_lines %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) +
  ylab("Muslim Warmth") + theme_m


# z plot method --------------------------------------------------------

#https://solomonkurz.netlify.app/post/2021-10-21-if-you-fit-a-model-with-multiply-imputed-data-you-can-still-plot-the-line/



fitted_lines <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()



fitted_lines %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) +
  ylab("Muslim Warmth") + theme_m

# DAG GRAPH ---------------------------------------------------------------


library(ggplot2)
library(magick)
library(patchwork)
ggsave


dag1 <-
  image_ggplot(image_read(here::here( "mods", "missing.tiff")),
               interpolate = T)
dag2 <-
  image_ggplot(image_read(here::here( "mods", "selection.tiff")),
               interpolate = T)
dag3 <-
  image_ggplot(image_read(here::here( "mods", "impute.tiff")),
               interpolate = T)


dag4 <-
  image_ggplot(image_read(here::here(
     "mods", "imputation-graph.tiff"
  )),
  interpolate = T)

dag4


dag5 <-
  image_ggplot(image_read(here::here(
     "mods", "swig-graph.tiff"
  )),
  interpolate = T)

dag5 <-
  image_ggplot(image_read(here::here(
     "mods", "swig-graph.tiff"
  )),
  interpolate = T)
dag5

imp0plot <-
  image_ggplot(image_read(here::here( "mods", "amelia-00.jpeg")),
               interpolate = T)

imp1plot <-
  image_ggplot(image_read(here::here(
     "mods", "amelia-01.jpg.jpeg"
  )),
  interpolate = T)


library(patchwork)

fig1 <-
  (lds2 + dag5) / (dag4 + (imp1plot + imp0plot)) + plot_annotation(tag_levels = "i")

fig1 <-
  lds2 + (dag5  / dag4)  + (imp1plot / imp0plot) + plot_annotation(tag_levels = "i")

fig1 <- (lds2 / dag5)  | (dag4  / imp1plot / imp0plot) +
  plot_annotation(tag_levels = "i") #+  plot_layout(nrow = 2,byrow = FALSE)

fig1

library(ggpubr)
fig1 <-
  ggarrange(
    lds2,
    dag5,
    dag4,
    ggarrange(imp1plot , imp0plot,
              labels = c("  (i)", "(ii)")),
    labels = c("A", "B", "C", "D"),
    widths = c(1, 1),
    heights = c(2, 1),
    ncol = 2,
    nrow = 2
  )

fig1
ggsave(
  fig1,
  path = here::here( "figs"),
  width = 12,
  height = 6,
  units = "in",
  filename = "fig1.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)



fig1 <- ggarrange(
  lds2,
  dag5,
  labels = c("A", "B"),
  #  widths = c(1, 1),
  heights = c(1.5, 2),
  # ncol = 2,
  nrow = 2
)

fig1
ggsave(
  fig1,
  path = here::here( "figs"),
  width = 10,
  height = 10,
  units = "in",
  filename = "fig1.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)


#USED

fig2 <- ggarrange(
  dag4,
  ggarrange(imp1plot, imp0plot, labels = c("  (i)", "(ii)")),
  heights = c(2, 1.5),
  # ncol = 2,
  nrow = 2,
  labels = c("A", "B", "C")
)

fig2


ggsave(
  fig2,
  path = here::here( "figs"),
  width = 10,
  height = 6.3,
  units = "in",
  filename = "fig2.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)


## Test
fig2 <- ggarrange(
  dag4,
  imp1plot,
  imp0plot,
  heights = c(1, 1.5, 1.5),
  # ncol = 2,
  nrow = 3,
  labels = c("A", "B", "C")
)

fig2


ggsave(
  fig2,
  path = here::here( "figs"),
  width = 8,
  height = 8,
  units = "in",
  filename = "fig2.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)


#graph for dags

daggraph <- (dag5)  +
  plot_annotation(title = "Missing potential outcomes and missing responses are both missing data problems")
daggraph

missgraph <- (dag4)  +
  plot_annotation(title = "Separate multiple imputation by condition for missing responses,\nleveraging six years of pre-attack data to recover no-exposure trajectory")

missgraph

# daggraph <- (dag2 /dag1) + dag3  +
#   plot_annotation(title = "Causal identification of missing potential outcomes\nand missing responses",
#                   tag_levels = "i")
# daggraph



dag5

ggsave(
  dag5,
  path = here::here( "mods"),
  width = 14,
  height = 7,
  units = "in",
  filename = "daggraph.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 1200
)


dag5

ggsave(
  dag4,
  path = here::here( "mods"),
  width = 14,
  height = 7,
  units = "in",
  filename = "missgraph.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 1200
)

# model graphs ------------------------------------------------------------

# SIMULATE data using simstudy --------------------------------------------


# how many from year 4 in year 10
in10 <- df %>%
  dplyr::select(Id,
                Wave,
                TSCORE,
                Warm.Muslims,
                YearMeasured, ) %>%
  dplyr::filter(
    Wave == 2012 & YearMeasured == 1 |
      Wave == 2013 & YearMeasured != -1 |
      Wave == 2014 & YearMeasured != -1 |
      Wave == 2015 & YearMeasured != -1 |
      Wave == 2016 & YearMeasured != -1 |
      Wave == 2017 & YearMeasured != -1 |
      Wave == 2018 & YearMeasured != -1
  ) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2012", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b) %>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave == 2013,
    TSCORE_b + 365,
    ifelse(
      YearMeasured == 0 & Wave == 2014,
      TSCORE_b + 730,
      ifelse(
        YearMeasured == 0 & Wave == 2015,
        TSCORE_b + 1094,
        # leap
        ifelse(
          YearMeasured == 0 & Wave == 2016,
          TSCORE_b + 1459,
          ifelse(YearMeasured == 0 &
                   Wave == 2017, TSCORE_b + 1824, TSCORE)
        )
      )
    )
  )) %>%
  dplyr::mutate(Attack = as.numeric(ifelse(TSCORE_i >= 3545, 1, 0))) %>% # All 0
  ungroup(Id)

#
table1::table1(~ Warm.Muslims | Wave * Attack, data = in10)
# 5142
#
6382 / (41566 + 1647) # % missing in Wave 10 ~ 15

library(simstudy)
set.seed(281726)
rm(tdat)
tdat <- km_all3 %>% filter(YearMeasured == 1)
# inspect long data
table1::table1(~ Warm.Muslims | Wave , data = all_d)
table1::table1(~ Warm.Muslims | Wave * Attack, data = tdat)


#tdef <- defData(varname = "m", dist = "binary", formula = 0.5)
rm(tdef) # remove in case object exists

tdef <-
  defData(
    varname = "Y0",
    dist = "normal",
    formula = 4.4,
    variance = 2,
    id = "id"
  )
tdef <-
  defData(
    tdef,
    varname = "Y1",
    dist = "normal",
    formula = "Y0 +  (Y0 * 0.01)",
    variance = 0,
    id = "id"
  )
tdef <-
  defData(
    tdef,
    varname = "Y2",
    dist = "normal",
    formula = "Y1 +  (Y1 * 0.01)",
    variance = 0,
    id = "id"
  )


dtTrial <- genData(47948, tdef)
head(dtTrial)

# check
table1::table1(~ Y0 + Y1 + Y2, data = dtTrial)

# make long
dtTime <-
  addPeriods(
    dtTrial,
    nPeriods = 3,
    idvars = "id",
    timevars = c("Y0",
                 "Y1",
                 "Y2"),
    timevarName = "Y"
  )
dtTime



MCAR <-
  defMiss(
    varname = "Y",
    formula = "-1.3",
    # just over 20% attrition
    logit.link = TRUE,
    monotonic = TRUE
  )
dm <-
  genMiss(dtTime, MCAR, "id", repeated = TRUE, periodvar = "period")

dObs <- genObs(dtTime, dm, idvars = "id")

# check
dObs[, .(prop.missing = mean(is.na(Y))), keyby = period]

table1::table1(~ Y | as.factor(period), data = dObs)

dObs
## Test imputation method for 1s
library(Amelia)

imputed_sim1 <- amelia(
  set.seed = 1234,
  dObs,
  cs = c("id"),
  ts = c("period"),
  m = 10,
  # number of imputations
  # idvars=c(""),
  lags = "Y",
  leads = "Y",
  polytime = 2,
  #  polynomials perhaps not sensible given
  intercs = F,
  # to many vars
  # bounds = bds, # lower upper bounds to Mus Prej
  empri = .05 * nrow(dObs)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed_sim1,
        here::here( "mods", "imputed_sim1"))
imputed_sim1 <-
  readRDS(here::here( "mods", "imputed_sim1"))

table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim1$imputations$imp1,
               overall = FALSE)

# test
imputed_sim1$imputations$imp1$Y


# model
m <- 10
model_sim1 <- NULL
for (i in 1:m) {
  model_sim1$model[[i]] <-
    lmer(Y ~ period + (1 |
                         id), data = imputed_sim1$imputations[[i]])
}

# table
tab_sim1 <- pool_parameters(model_sim1$model)
tab_sim1 # recover effect
tab_sim1 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab_sim1, show_labels = TRUE)



fitted_lines_sim1 <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_sim1$model[[.]], terms = c("period[0:2, by =.01]")
  ))) %>%
  data_frame() %>%
  unnest()

library(ggsci)

plot_sim1 <- fitted_lines_sim1 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.6)) +
  labs(subtitle = "MI from previous six previous waves",
       y = "Muslim Warmth",
       x = "Waves: 2012-2020/21, N = 11799") +   scale_colour_npg(alpha =
                                                                    .5) + theme_classic() # +
#scale_colour_okabe_ito(alpha=.5)


plot_sim1




## create data
m <- 10
self1 <- NULL
for (i in 1:m) {
  self1$imputations$imp[[i]] <- imputed_sim1$imputations[[i]] %>%
    dplyr::mutate(a = as.factor(rep(1, nrow(.)))) %>%
    dplyr::arrange(period, id)
}
str(self1$imputations$imp[[1]])
# SIMULATE ZEROS ----------------------------------------------------------

#Generate data with complete missingness for the zeros



set.seed(281726)

# move data (in case there)
rm(ztdef)

# define yearly outcomes
ztdef <-
  defData(
    varname = "Y0",
    dist = "normal",
    formula = 3.7,
    variance = 1
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y1",
    dist = "normal",
    formula = "Y0 +  (Y0 * 0.025)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y2",
    dist = "normal",
    formula = "Y1 +  (Y1 * 0.025)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y3",
    dist = "normal",
    formula = "Y2 +  (Y2 * 0.02)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y4",
    dist = "normal",
    formula = "Y3 +  (Y3 * 0.02)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y5",
    dist = "normal",
    formula = "Y4 +  (Y4 * 0.015)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y6",
    dist = "normal",
    formula = "Y5 +  (Y5 * 0.015)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y7",
    dist = "normal",
    formula = "Y6 +  (Y6 * 0.015)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y8",
    dist = "normal",
    formula = "Y7 +  (Y7 * 0.015)",
    variance = 0
  )



head(ztdef)

# Missing
#defM <- defMiss(varname = "Y7", formula = 1, logit.link = FALSE)
#defM <- defMiss(defM, varname = "Y8", formula = 1, logit.link = FALSE)


#5142/(5142 + 2020) 72 percent

# create data
zdtTrial <- genData(12179, ztdef)

# create NA columens

# zdtTrial$Y7 <- rep(NA, nrow(zdtTrial))
# zdtTrial$Y8 <- rep(NA, nrow(zdtTrial))

# make logical values numeric
zdtTrial <- zdtTrial %>%
  mutate(across(where(is.logical), as.numeric)) # make all numeric

# check
table1::table1(~ Y0 + Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8, data = zdtTrial)

# save
saveRDS(zdtTrial, here::here( "mods", "zdtTrial"))

# check




# zdtTrial$Y8 <- rep(NA, nrow(zdtTrial))




# Next crate create data for T10
id <- seq(12180, 41566 + 6382, by = 1)
n <- length(id)

Y0 <- rep(NA, n)
Y1 <- rep(NA, n)
Y2 <- rep(NA, n)
Y3 <- rep(NA, n)
Y4 <- rep(NA, n)
Y5 <- rep(NA, n)
Y6 <- rnorm(n = n, mean = 4.15, sd = 1.4)
Y7 <- rep(NA, n)
Y8 <- rep(NA, n)

dx0 <- data.frame(id, Y0, Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8)

str(dx0)
# create missing vals
d0 <- dx0 %>%
  mutate(missing = rbinom(n(), size = 1, prob = 0.15)) %>%
  mutate(Y7 = ifelse(missing == 1, NA, Y7)) %>%
  select(-missing) %>%
  mutate(across(where(is.logical), as.numeric)) # make all numeric

d0 <- data.table::as.data.table(d0)
str(d0)

tbig <- addPeriods(
  d0,
  nPeriods = 9,
  idvars = "id",
  timevars = c("Y0",
               "Y1",
               "Y2",
               "Y3",
               "Y4",
               "Y5",
               "Y6",
               "Y7",
               "Y8"),
  timevarName = "Y"
)


# make long
zdtTime <-
  addPeriods(
    zdtTrial,
    nPeriods = 9,
    idvars = "id",
    timevars = c("Y0",
                 "Y1",
                 "Y2",
                 "Y3",
                 "Y4",
                 "Y5",
                 "Y6",
                 "Y7",
                 "Y8"),
    timevarName = "Y"
  )

# merge data
zdtTime



MCAR <-
  defMiss(
    varname = "Y",
    formula = "-1.3",
    # just over 20% attrition
    logit.link = TRUE,
    monotonic = TRUE
  )


zdm <-
  genMiss(zdtTime, MCAR, "id", repeated = TRUE, periodvar = "period")



zdObs[, .(prop.missing = mean(is.na(Y))), keyby = period]

table1::table1(~ Y | as.factor(period), data = zdObs)

tzdObs <- zdObs %>%
  arrange(Y, id) %>%
  group_by(period) %>%
  mutate(Y = ifelse(period == 7 |
                      period == 8, recode(Y, NA), Y))

table1::table1(~ Y | as.factor(period), data = tzdObs)


om <- rbind(tzdObs, tbig)

head(om)
length(unique(om$id))


# Inspect data
table1::table1(~ Y | as.factor(period), data = om)
str(om)
om <- om %>%
  select(id, period, Y)

saveRDS(om, here::here( "mods", "om"))

## Test imputation method for 1s
library(Amelia)
str(om)
om <- as.data.frame(om)
imputed_sim0 <- amelia(
  set.seed = 1234,
  om,
  cs = c("id"),
  ts = c("period"),
  m = 10,
  # number of imputations
  # idvars=c(""),
  lags = "Y",
  leads = "Y",
  polytime = 2,
  #  polynomials otherwise regression to the mean
  intercs = F,
  # to many vars
  # bounds = bds, # lower upper bounds to Mus Prej
  empri = .05 * nrow(om)
) # ridge prior see: Amelia pdf documentation p.23

# save data
saveRDS(imputed_sim0,
        here::here( "mods", "imputed_sim0"))

table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Y |
                 as.factor(period),
               data = imputed_sim0$imputations$imp1,
               overall = FALSE)

# compare
table1::table1(~ Y | as.factor(period), data = zdObs)


# Select only final three waves
m <- 10
self0 <- NULL
for (i in 1:m) {
  self0$imputations$imp[[i]] <- imputed_sim0$imputations[[i]] %>%
    dplyr::filter(period == 6 |
                    period == 7 | period == 8) %>%
    dplyr::mutate(a = as.factor(rep(0, nrow(.)))) %>%
    dplyr::mutate(period = period - 6) %>%
    dplyr::arrange(period, id)
}



# model
m <- 10
model_sim0 <- NULL
for (i in 1:m) {
  model_sim0$model[[i]] <-
    lmer(Y ~ period + (1 |
                         id), data =  self0$imputations$imp[[i]])
}

# table
tab_simzero <- pool_parameters(model_sim0$model)
tab_simzero
tab_simzero [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab_simzero, show_labels = TRUE)


## create full data
m <- 10
model_sim_full <- NULL
for (i in 1:m) {
  model_sim_full$imputations$imp[[i]] <-
    dplyr::bind_rows(self0$imputations$imp[[i]],
                     self1$imputations$imp[[i]]) %>%
    dplyr::rename(wave = period) %>%
    arrange(wave, id)
}

str(model_sim_full$imputations$imp[[1]])
## replicate model

# save data
sim_list <- model_sim_full$imputations$imp
str(sim_list)
str(listbayes)

# saveRDS(self0, here::here("_posts","mus","mods", "self0"))
# saveRDS(self1, here::here("_posts","mus","mods", "self1"))
# saveRDS(model_sim_full, here::here("_posts","mus","mods", "model_sim_full"))
# saveRDS(sim_list, here::here("_posts","mus","mods", "sim_list"))
sim_list <-
  readRDS(here::here( "mods", "sim_list"))
model_sim_full <-
  readRDS(here::here( "mods", "model_sim_full"))

library(lme4)

m <- 10
model_sim_rep <- NULL
for (i in 1:m) {
  model_sim_rep$model[[i]] <-
    lmer(Y ~ a * wave + (1 |
                           id), data =  model_sim_full$imputations$imp[[i]])
}

# table
tab <- parameters::pool_parameters(model_sim_rep$model)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)

ggeffects::ggemmeans(model_sim_rep$model[[2]], terms = c("wave", "a"))

library(dplyr)
library(tidyr)
library(broom)


fitted_lines_simfull <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_sim_rep$model[[.]], terms = c("wave[0:2,by=.01]", "a")
  ))) %>%
  data_frame() %>%
  unnest()


library(ggsci)
plot_simall <- fitted_lines_simfull %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.05, 4.55)) +
  labs(subtitle = "Multiple imputation with simulated data",
       y = "Muslim Warmth",
       x = "Waves: 2012-2020/21, Simulated N = 47,978") +   scale_colour_npg(alpha =
                                                                               .5) + theme_classic()  +
  scale_colour_npg(alpha = .5)



# compare with bayesian model

# cluster plot in "futures.R"
cluster_plot_st_sim2 <-
  cluster_plot_st_sim +   labs(subtitle = "Bayesian model with real data",
                               y = "Muslim Warmth",
                               x = "Waves: 2012-2020/21, N = 47,978")

sim_plot <- cluster_plot_st_sim2 + plot_simall +
  plot_annotation(tag_levels = "i",  title = "Simulated data that assumes even greater missingness nevertheless recovers approximate data model slopes")

sim_plot

ggsave(
  sim_plot,
  path = here::here(here::here( "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "sim_plot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# compare with full data & expected values
table1::table1(~ Y0 + Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8, data = zdtTrial)



ggsave(
  plot_simall,
  path = here::here(here::here( "figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_simall.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


compare_graph <-
  plot_3 + plot_bayes_3 + plot_annotation(tag_levels = "i",
                                          title = "No practical difference between frequentist and Bayesian estimation") + plot_layout(guides = 'collect')

ggsave(
  compare_graph,
  path = here::here(here::here( "figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "compare_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)
