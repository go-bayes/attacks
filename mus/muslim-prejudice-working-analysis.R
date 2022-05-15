# Causal estimate of long term effect of terrorist attacks on muslim prejudice
# joseph.bulbulia@gmail.com
# code for plots


# model <- sjstats::tidy_stan(tab)

#libraries
library("here") # file management
library("equatiomatic") # equations
library("lubridate") # working with dates
library("ggplot2") # graphs
library("ggthemes") #themes
library("Amelia") # missing data imputation
library("ggpubr") # graphs
#library("viridis") # colours
library("patchwork") # combine graphs
library("ggforce") # graphs
library("lme4") # multilevel estimation
#library("geepack") # marginal models
library("easystats") # reporting
library("kableExtra") # tables
library("broom") # working with  models
library("broom.mixed") # mixed effects models
library("brms") # bayesian estimation
library("rstan") # backend brms
library("rstanarm") # graphing
#library("cmdstanr") # backend brms'
#library("ipw") # inverse probability weighting
library("tidybayes") # workign with posterior probability distributions
library("bayesplot") # graphs
library("ggokabeito")   # color palette
#library("gghalves")     #  half geoms
#library("ggbeeswarm")   # Special distribution-shaped point jittering
library("emmeans") # estimate marginal means
library("table1") # tables /now with latex
library("tidyverse") # data wrangling
library("sjstats")
library("magick")
library("simstudy")
library("future")
#library(splines)
# rstan options
library("brms") # bayesian estimation
#library("cmdstanr") # backend brms
rstan_options(auto_write = TRUE) # bayesian estimation
#options(mc.cores = parallel::detectCores ()) # use all core

#options(mc.cores = 2 ) # use all core

#parallel::detectCores ()
# cite packages
cite_packages()


# import data -------------------------------------------------------------

df <- readRDS(here::here("data", "df"))


# timeline ----------------------------------------------------------------

# create timeline
df%>%
  dplyr::filter(Wave==2019) %>%
  select(TSCORE)%>%
  summarise(max(TSCORE, na.rm=TRUE))


# 3665 - min = 13 July 2019
# 4125 max = 15 October 2021

rarep<-df%>%
  dplyr::filter(YearMeasured == 1) %>%
  #dplyr::filter(Wave == 2018 | Wave == 2019) %>%
  # dplyr::group_by(Id) %>% filter(n() == 2) %>%
  dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020 ) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack!
  ungroup(Id)%>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30)+ TSCORE)%>%
  dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(Condition = factor(
    ifelse(day >="2018-06-18" & day < "2019-03-15", 0,
           ifelse( day >="2019-03-15" & day < "2019-06-18",1,
                   ifelse(day >="2019-06-18" & day < "2020-10-15", 2, 3))),
    labels= c("Baseline","Post-attack", "Post-attack one year","Post-attack two years")))%>%
  arrange(day,Condition)


# get data of change of baseline
#dates_vline1<- as.Date("2018-06-18")

# get attack date
dates_vline2<- as.Date("2019-03-15")
dates_vline3<- as.Date("2019-06-18")
dates_vline4<-as.Date("2021-06-18")
#3665 - min = 13 July 2019
# 4125 max = 15 October 2021

# for line in a graph
dates_vline2b<-which(rarep$day %in% dates_vline2)
dates_vline3b<-which(rarep$day %in% dates_vline3)
dates_vline4b<-which(rarep$day %in% dates_vline4)

#rarep$Condition <- ordered(rarep$Condition, c("Baseline","Post-attack", "Post-attack one year","Post-attack two years"))


lds2 <- ggplot(rarep, aes(day, n)) +
  geom_col(aes(fill = Condition)) +
  scale_x_date(date_labels = "%b/%Y", limits = c(as.Date("2018-06-01"), as.Date("2021-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  xlab("NZAVS Waves years 2018 - 2021 daily counts by condition") + ylab("Count of Responses")+
  theme_classic()  +
  # annotate(
  #   "rect",
  #   xmin = as.Date("2017-08-25"),
  #   xmax = dates_vline1,
  #   ymin = 0,
  #   ymax = 900,
  #   alpha = 0
  # ) +
  annotate(
    "rect",
    xmin = dates_vline2,
    xmax = dates_vline3,
    ymin = 0,
    ymax = 2000,
    alpha = .3,
    fill = "darkred"
  ) +
  # annotate(
  #   "rect",
  #   xmin = dates_vline1,
  #   xmax = dates_vline2,
  #   ymin = 0,
  #   ymax = 900,
  #   alpha = .3
  # ) +
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
  annotate(
    "text",
    x = as.Date("2018-10-15"),
    y = 2600,
    label = "Time 10\npre-attacks"
  ) +
  annotate("text",
           x = as.Date("2019-01-01"),
           y = 2950,
           label = "**attack**") +
  annotate(
    "text",
    x = as.Date("2019-06-15"),
    y = 2600,
    label = "Time 10\npost-attacks"
  ) +
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
  scale_y_continuous(limits=c(0,2950))
#theme(legend.position="none")


### USE GRAPH OF TIMELINE
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



# graph for wave 10 & sadi study ------------------------------------------


### Wave 10 Only

sadi <- df %>%
  filter(Wave == 2018 & YearMeasured==1)%>%
  select(Warm.Muslims, TSCORE, Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30)+ TSCORE)%>%
 # dplyr::filter(timeline > "2018-06-06") %>%
 # dplyr:::count(day = floor_date(timeline, "day"))%>%
   dplyr::mutate(Attack_Condition = factor(
    ifelse(timeline < "2019-03-15", 0, 1),
    labels= c("Baseline","Post-attack")))%>%
  arrange(timeline,Attack_Condition)

sadi <-as.data.frame(sadi)
head(sadi)
min(sadi$day)


table1::table1( ~ Attack_Condition, data = sadi )

library(ggsci)
rdd <- ggplot(sadi, aes(x = timeline, y = Warm.Muslims, color = Attack_Condition)) +
  geom_point(alpha = .01) +
  stat_smooth(method = "gam") +
  theme(legend.position = "bottom") +
  labs(subtitle = "RDD: gam estimator",
       y = "Warmth to Muslims",
       x = "NZAVS Time 10 timeline (after June 06) ") +
  scale_colour_npg(alpha = 1) + theme_classic()

rdd

### Graph of Timeline

sadi2 <- df %>%
  filter(Wave == 2018 & YearMeasured == 1) %>%
 # drop_levels() %>%
  select(Warm.Muslims, TSCORE) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr::filter(timeline > "2018-06-06") %>%
  dplyr:::count(day = floor_date(timeline, "day")) %>%
      dplyr::mutate(Attack_Condition = factor(
        ifelse(day < "2019-03-15", 0, 1),
        labels = c("Baseline", "Post-attack")
      )) %>%
      arrange(day, Attack_Condition)

sadi2
dates_vline2<- as.Date("2019-03-15")
dates_vline2b<-which(sadi2$day %in% dates_vline2)

dates_vline3<- as.Date("2019-06-15")
dates_vline3b<-which(sadi2$day %in% dates_vline3)

#dates_vline3<- as.Date("2019-06-18")

lds3 <- ggplot(sadi2, aes(day, n)) +
  geom_col(aes(fill = Attack_Condition)) +
  scale_x_date(date_labels = "%b/%Y") +     #  limits = c(as.Date("2018-06-01"), as.Date("2021-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(sadi2$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  geom_vline(xintercept = as.numeric(sadi2$day[dates_vline3b]),
             col = "red",
             linetype = "dashed") +
  labs(x = "NZAVS Waves years 2018 - 2021 daily counts by condition",
       y = "Count of Responses",
     #  title = "Data Collection for RDD Analysis of Mosque Attacks",
       subtitle = "Data Collection Wave 10 (after June 6)")+
  scale_colour_npg(alpha =.1) +
  annotate(
    geom = "text",
    x = as.Date("2019-05-01"),
    y = 2300,
    label = "90 Days\nafter attacks") +
  theme_classic()


rdd_graph <- lds3/rdd + plot_layout(guides = 'collect') + plot_annotation(tag_levels = "i", "Replication of Regression Discontinuity Analysis")


rdd_graph




# ggsave(
#   rdd_graph,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 12,
#   height =12,
#   units = "in",
#   filename = "rdd_graph.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1000
# )

# time trajectory --------------------------------------------------------------


### Impute all waves of MUS PRED

# table1::table1(~ Partner|Wave, data = df)%>%
#   kbl()%>%
#   kable_paper("hover", full_width = F)

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
  dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Id,Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2012", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b)%>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave==2013, TSCORE_b + 365,
    ifelse(YearMeasured == 0 & Wave==2014, TSCORE_b + 730,
           ifelse(YearMeasured == 0 & Wave==2015, TSCORE_b + 1094,  # leap
                  ifelse(YearMeasured == 0 & Wave==2016, TSCORE_b + 1459,
                         ifelse(YearMeasured ==0 & Wave ==2017, TSCORE_b + 1824, TSCORE))))))%>%
  dplyr::mutate(Attack = as.numeric(ifelse(TSCORE_i >= 3545, 1, 0)))%>% # All 0
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)),
                yrs = dys/365)%>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>%
  dplyr::mutate(wave = as.numeric(Wave)-1) %>%
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Wave,Id) %>%
  droplevels() %>%
  arrange(Wave,Id) %>%
  dplyr::mutate(wave = as.numeric(Wave)-1)%>%
  group_by(Id) %>%
  dplyr::mutate(Ys = Warm.Muslims)%>%
  group_by(Id)%>% # need to fill this way
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
  dplyr::mutate(male_2z = if_else(Wave == "2012", (as.numeric(Male))/2, NA_real_)) %>%
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
  #  dplyr::mutate(As = as.factor(As)) %>%
  ungroup()%>%
  dplyr::mutate(As = replace_na(As, 0)) %>%
  arrange(Wave, Id)
levels(all_d$Wave) <- c("Time4", "Time5", "Time6", "Time7","Time8", "Time9")
table(all_d$Wave)
table(all_d$As)
summary(all_d$dys)


str(all_d$As)


# all_d_time_10 -----------------------------------------------------------


all_di <- df %>%
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
      Wave == 2017 & YearMeasured != -1 |
      Wave == 2018 & YearMeasured != -1
  ) %>%
  dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019
  ungroup() %>%
  arrange(Wave,Id)

arrange(Wave, Id)
levels(all_di$Wave) <- c("Time4", "Time5", "Time6", "Time7","Time8", "Time9","Time10")
table(all_di$Wave)
table1::table1(~ TSCORE|Wave, data = all_di)

# 3500 participants from the TIME 12 wave study also responded in Time 10

# return to analysis ------------------------------------------------------
all_d
all_d_selected <-all_d%>%
  dplyr::select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,nzdep_bz,
                male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b, GenCohort, dys,yrs,TSCORE_i)
all_d_selected

saveRDS(all_d_selected, here::here("mods","all_d_selected"))




tall<- table1::table1(~ Ys|Wave, data = all_d_selected, overall=FALSE)



kable(tall, format ="latex", booktabs = TRUE)


# Amelia will center and scale continuous variables during the MI process.

library(Amelia)

# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.

# assume Y^0|A=2018 = Y^0 2019
all_d_selected <- as.data.frame(all_d_selected)
head(all_d_selected)
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3) # bounds for mus

imputed_m <- amelia(
  set.seed = 1234,
  all_d_selected,
  #dataset to impute
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  lags= "Ys",
  # leads="Ys",
  noms = c("EthnicCats_b","GenCohort"),
  idvars = c("Wave","As", "yrs","dys","TSCORE_i"),
  #  polytime = 2, # Allow polynomial?
  intercs = F, # too many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(all_d_selected)
)
#saveRDS(imputed_m, here::here("_posts","mus", "mods","imputed_m"))
imputed_m <-readRDS( here::here("_posts","mus", "mods","imputed_m"))

max(imputed_m$imputations[[1]]$yrs)

library(splines)
m<-10
model_m<-NULL
for(i in 1:m) {
  model_m$model[[i]] <- lmer(Ys ~ bs(yrs)  + (1|Id), data = imputed_m$imputations[[i]])
}

# to recover linear trajectory
library(lme4)
m<-10
model_ml<-NULL
for(i in 1:m) {
  model_ml$model[[i]] <- lmer(Ys ~ yrs  + (1|Id), data = imputed_m$imputations[[i]])
}
pool_parameters(model_ml$model)
library(parameters)
# spline

tab<-pool_parameters(model_ml$model)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

# graph trajectory -------------------------------------------------------------

# not run
# fitted_lines_yrs_spx<-
#   tibble(.imp = 1:10) %>%
#   mutate( p = map(.imp, ~  ggeffects::ggpredict(model_m$model[[.]],terms = c("yrs[all]"), data = imputed_m$imputations$imp1$yrs)))%>%
#   data_frame() %>%
#   unnest()
# fitted_lines_yrs_spx


fitted_lines_yrs<-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_ml$model[[.]],terms = c("yrs[0:6.15, by=.05]"))))%>%
  data_frame() %>%
  unnest()

fitted_lines_yrs<-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_ml$model[[.]],terms = c("yrs[all]"))))%>%
  data_frame() %>%
  unnest()


plot_time <- fitted_lines_yrs %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.5)) +
  labs(title="National New Zealand trajectory in Muslim acceptance",
       subtitle="years: 2012-2017/18; N = 12179") +
  labs(y="Muslim Warmth",
       x = "Years:2012-2017/19") +
  scale_y_continuous(limits=c(3,4.5)) +
  theme_classic() #coord_flip()

plot_time

ggsave(
  plot_time,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "pplot_time,jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



trajectory_1217 <-plot_time + #  add.data   = TRUE, dot.alpha = 0.005) +
  labs(title="National New Zealand trajectory in Muslim acceptance",
       subtitle="years: 2012-2017/18; N = 12179") +
  labs(y="Muslim Warmth",
       x = "Years:2012-2017/19") +
  scale_y_continuous(limits=c(3,4.5)) +
  theme_classic() #coord_flip()
trajectory_1217

ggsave(
  plot_time,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "pplot_time.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


#
# plot_trajectory_linear <-plot(ma)+labs(title="Growth in Muslim Acceptance",
#                                   subtitle="Linear model") +
#   labs(y="Muslim Warmth",
#        x = "Years") +
#   scale_y_continuous(limits=c(3,4.5)) + theme_classic() #+ scale_colour_okabe_ito(alpha =.3)

#
# trajectory_spline <- plot_trajectory_linear + plot_trajectory_spline +
#   plot_annotation(title = "Increasing trajectory in Muslim Acceptance",
#                   subtitle = "Years 2012-2017/12",tag_levels = "a")

#scale_color_viridis_d(option = "cividis")

trajectory_spline

ggsave(
  trajectory_spline,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "trajectory_spline",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

ggsave(
  trajectory_1217,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "trajectory_1217",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)





# # bayes time trajectory --------------------------------------------------------
# # prepare data
#
# m <- 10
# trajectory <- NULL
#
# for(i in 1:m) {
#   trajectory$imputations$imp[[i]] <-imputed_m$imputations[[i]] %>%
#     arrange(Wave,Id)
# }
#
#
# trajectorylist <- trajectory$imputations$imp[[i]]
# saveRDS(trajectorylist, here::here("_posts","mus","mods","trajectorylist"))
# #Gamm
#
#
# b_time <- brms::brm(
#   bf(Ys ~ yrs  + (1|Id)),
#   family = gaussian,
#   data = trajectorylist,
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here("_posts", "mus", "mods", "b_time")
# )
#
# lazerhawk::brms_SummaryTable(b_time, panderize=F)


#
# # graph bayes time trajectory -----------------------------------------------------------
# plot_smooth <- marginal_smooths(b_time)
# plot_smooth
#
# conditional9 <- plot(marginal_smooths(b9_m0,  "Wave:As",  ndraws = 100, spaghetti = T), points = F)
# saveRDS(conditional9,here::here("_posts","mus","mods","conditional9"))
#
# bayes_9 <- conditional9$`Wave:As`  +  scale_y_continuous(limits=c(4.0,4.48)) +
#   labs(subtitle="Potential outcome trajectory in Muslim acceptance:\nattack vs. no attack",
#        y= "Muslim Warmth",
#        x = "Waves: 2012-2020, N = 11799") + scale_colour_okabe_ito(alpha=.5)
#
# #scale_color_viridis_d(option = "cividis")
#
# bayes_9
# ggsave(
#   bayes_9,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 12,
#   height =9,
#   units = "in",
#   filename = "bayes_9.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1000
# )

# df for estimating time trajectory in attack sample ---------------------------

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
  dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Id,Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2018", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b)%>%
  dplyr::mutate(TSCORE_i = ifelse(YearMeasured == 0 & Wave==2019, TSCORE_b + 364,
                                  ifelse(YearMeasured == 0 & Wave==2020, TSCORE_b + 729, TSCORE)))%>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)),
                yrs = dys/365)%>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack)%>%
  dplyr::mutate(wave = as.numeric(Wave)-1) %>%
  group_by(Id)%>% # need to fill this way
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
  dplyr::mutate(male_2z = if_else(Wave == "2018", (as.numeric(Male))/2, NA_real_)) %>%
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
  ungroup()%>%
  arrange(Id,Wave)
levels(km_all3$Wave) <- c("Time10", "Time11","Time12")


summary(km_all3$Wave)
# check N
km_all3
length(unique(km_all3$Id))
# correct
km_all3
t13<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all3, overall=FALSE)
t13
kable(t13, format ="latex", booktabs = TRUE)



# impliment flawed model that does not adjust for missingness. ------------


bad_mod1 <- lmer(Ys ~ as.factor(As) + Wave + (1|Id), data = km_all3)

model_parameters(bad_mod1)
bad_mod1

pl_drop_bad <- ggeffects::ggemmeans(bad_mod1, terms = c("Wave","As"))

drop__graph <- plot( pl_drop_bad ) + labs(
  x = "Years 2018-2021",
  y = "Warmth to Muslims",
  title = "Pairwise deletion graph: no imputation"
)
drop__graph


tab_drop<-model_parameters(bad_mod1,  summary = FALSE)
tab_drop

tab_drop[,c(1:5)]%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab_drop,  show_labels = TRUE)

ggsave(
  drop__graph,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "drop__graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)

#
contrasts_tab<- estimate_contrasts( bad_mod1,
                    contrast = "As",
                    at = c("Wave","As") )

contrasts_tab%>%
  kbl("latex",booktabs = TRUE,digits=2)


# poor imputation model that does not address for counterfactuals ---------


#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all3, overall=FALSE)

obtl<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all3, overall=FALSE)

kable(obtl, format ="latex", booktabs = TRUE)



# TABLE FOR PUB -----------------------------------------------------------

tab_pub <- km_all3 %>%
  dplyr::mutate(Urban = as.factor(Urban),
                Parent =as.factor(Parent),
                Partner = as.factor(Partner))


x <- table1::table1(~  Age +Edu + Employed + EthnicCats  + Male + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban|Wave, data = tab_pub, overall = FALSE)
x

t1kable(x, format ="latex")


km_all3$As
# create new data set
library(tidyverse)
kf3  <- km_all3 %>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (ifelse(Wave=="Time10" & Attack == 1|Wave == "Time11"|Wave =="Time12", 0,
                      ifelse(Wave=="Time10" & Attack == 0, 1, Attack)))) %>%
  mutate(Ys = ifelse(Wave=="Time10" & Attack == 1|Wave == "Time11"|Wave =="Time12", NA,
                     ifelse(Wave=="Time10" & Attack == 0, NA, Warm.Muslims)))%>%
  ungroup() %>%
  arrange(Wave,Id)


# dat_all N
# correct

# Test NAs = Correct
table1::table1(~Ys|Wave *as.factor(As), data =kf3, overall=F)



## Bind - double dataset to creat missing values
ka3 <- km_all3%>%
  bind_rows(kf3)%>%
  arrange(Id,Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

saveRDS(ka3, here::here("_posts","mus","mods","ka3"))

#head(ka3$Ys)
# Check

# Missing data problem
t2<- table1::table1( ~Ys | Wave*As, data = ka3, overall=F)
t2
t2
t1kable(t2, format ="latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link dfs for zero estimate -----------------------------------------
km_zero <- ka3 %>%
  filter((As == 0))%>%
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
  )%>%
  arrange(Wave,Id)

str(km_zero$As)
summary(km_zero$yrs)
summary(km_zero$TSCORE_i)


head(km_zero)
dim(km_zero)
# works


# remove wave 10 from the pre-attack sample (above)

km_pre<- all_d_selected %>%
  # dplyr::filter(Wave != "Time10")%>%
  # droplevels() %>%
  dplyr::select(c(-wave))%>% # wrong
  dplyr::mutate(As = as.factor(As))%>%
  group_by(Id) %>%
  arrange(Wave,Id)

dim(km_pre)
head(km_pre)
table1::table1(~ Ys|Wave*As, data = km_pre, overall=FALSE)


str(km_pre$As)
length(unique(km_pre$Id))
str(km_pre$As)
summary(km_pre$dys)
# bind rows and arrange
bind_zero <-full_join(km_pre,km_zero)%>%
  arrange(Wave,Id)

table(bind_zero$Wave)
# relevel
levels = c("Time4", "Time5","Time6","Time7","Time8","Time9",
           "Time10","Time11","Time12")

bind_zero$Wave <- fct_relevel(bind_zero$Wave, levels)
levels(bind_zero$Wave)
str(bind_zero$Wave)
head(bind_zero)
## Make numeric var

bind_zero1 <-bind_zero %>%
  mutate(wave = as.numeric(Wave) -1) %>%
  dplyr::select(-c(dys,yrs)) %>%  # not working
  arrange(Wave, Id) %>%
  group_by(Id) %>%
  mutate(dys = TSCORE_i - min(TSCORE_i) )%>% # Not used
  mutate(yrs = dys/365) %>% # note used
  ungroup()

summary(bind_zero1$wave)
# correct


# zero table --------------------------------------------------------------


zt <- table1::table1(~ Ys|As*Wave, data = bind_zero1, overall=F, transpose = F )

t1kable(zt, format ="latex")

# Impute 0s ------------------------------------------------------------


bind_zero1 <- as.data.frame(bind_zero1)

library(Amelia)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.
# assume Y^0|A=2018 = Y^0 2019


# limit Ys to range 1 to 7
# find col number
head(bind_zero1)
# create bounds
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

# cannot get dys/years easily, use wave
imputed0<- amelia(
  set.seed=1234,
  bind_zero1,
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  ords = "Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","TSCORE_i","dys","yrs"), # yrs not working
  lags="Ys",
  #leads="Ys",
  polytime = 2, # Allow polynomial
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(bind_zero1)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0, here::here("_posts","mus","mods", "imputed0"))


length(unique(imputed0$imputations$imp1$Id))
head(imputed0$imputations$imp1$Id)




table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0$imputations$imp10, overall=FALSE)


# impute 1s ---------------------------------------------------------------

km_one <- ka3 %>%
  filter((As == 1))%>%
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
  )%>%
  mutate(wave = as.numeric(Wave) -1) %>%
  arrange(Wave,Id)

summary(km_one$wave)
#check looks good
table1::table1(~ Ys|Wave * As, data = km_one, overall=FALSE)



head(km_one)
dim(km_one)
# make data frame
km_one<-as.data.frame(km_one)


at <- table1::table1(~ Ys|As*Wave, data = km_one, overall=F, transpose = F )

t1kable(at, format ="latex")

# create bounds for Ys
head(km_one)
# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1<- amelia(
  set.seed=1234,
  km_one,
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  ords="Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys","yrs","TSCORE_i"),
  lags="Ys",
  leads="Ys",
  polytime = 2, #  polynomials perhaps not sensible given
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(km_one)) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1, here::here("_posts","mus","mods", "imputed1"))



# traceplot 1s ------------------------------------------------------------

imputed1<- readRDS(here::here("_posts","mus","mods","imputed1"))
dev.off()
set.seed(0)
out1<- as.character(sample(imputed1$imputations$imp1$Id, 12, replace = FALSE))
out1


imputed0<- readRDS(here::here("_posts","mus","mods","imputed0"))
dev.off()
set.seed(0)
out0<- as.character(sample(imputed0$imputations$imp1$Id, 12, replace = FALSE))
out0


# traceplot 00's
tscsPlot(imputed0, cs = c(out0), main = "no-attack exposure",
         var = "Ys", nr = 3,ylim = c(0,8))

# traceplot 01
tscsPlot(imputed1, cs = c(out1), main = "attack exposure",
         var = "Ys", nr = 3,ylim = c(0,8))



# check imputation of 1's -------------------------------------------------


table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1$imputations$imp10, overall=FALSE)

# make frames compatible --------------------------------------------------
imputed0<-readRDS(here::here("_posts","mus","mods", "imputed0"))
imputed1<-readRDS(here::here("_posts","mus","mods", "imputed1"))


imp0 <- transform(imputed0, Wave = as.character(Wave))
imp1 <- transform(imputed1, Wave = as.character(Wave))


# bind data frames --------------------------------------------------------
levels_old <- c("Time4", "Time5","Time6","Time7","Time8","Time9",
                "Time10","Time11","Time12")
newlevels = c("Time10","Time11","Time12")

m <- 10
zero <- NULL
for (i in 1:m) {
  zero$imputations$imp[[i]] <- imp0$imputations[[i]] %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, levels_old)) %>%
    droplevels()%>%
    dplyr::group_by(Id) %>%
    arrange(Wave,Id)
}

one <- NULL

for(i in 1:m) {
  one$imputations$imp[[i]] <- imp1$imputations[[i]] %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, newlevels)) %>%
    droplevels()%>%
    arrange(Id)
}


m <- 10
imps_bind <- NULL
for (i in 1:m) {
  imps_bind$imputations$imp[[i]] <-
    dplyr::bind_rows(zero$imputations$imp[[i]],
                     one$imputations$imp[[i]])%>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave =="Time10"| Wave =="Time11"| Wave == "Time12") %>%
    droplevels()%>%
    dplyr::mutate(Wave = as.numeric(Wave)-1)%>%
    dplyr::arrange(Wave,Id)


}



# Works!
summary(imps_bind$imputations$imp[[1]]$Wave)


# save
#saveRDS(imps_bind, here::here("_posts", "mus", "mods", "imps_bind"))

# read
imps_bind <- readRDS(here::here("_posts", "mus", "mods", "imps_bind"))

# make list for bayesian models
#listbayes <-imps_bind$imputations$imp

# save list for bayesian models
#saveRDS(listbayes, here::here("_posts", "mus", "mods", "listbayes"))

#readRDS
#listbayes<- readRDS(here::here("_posts", "mus", "mods", "listbayes"))


# ML model ----------------------------------------------------------------

# model
library(lme4)
m<-10
model_all<-NULL
for(i in 1:m) {
  model_all$model[[i]] <- lmer(Ys ~ As * Wave + (1|Id), data = imps_bind$imputations$imp[[i]])
}

# table
tab<-pool_parameters(model_all$model)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

# pl_ml <- ggeffects::ggemmeans(model_all[[2]], terms = c("Wave","As"))
# pl_ml


<<<<<<< HEAD



fitted_lines_all<-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()



plot_3 <- fitted_lines_all %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="Multiple imputation: sample from previous 6 waves prior to attack + full attack wave sample + 2 post-attack waves",
       y= "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.5) + theme_classic()
plot_3


ggsave(
  plot_3,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "plot_3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)

=======
library(ggsci)

fitted_lines_all<-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()



plot_3 <- fitted_lines_all %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="MI from six previous waves + full attack wave",
       y= "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_colour_npg(alpha =.5) + theme_classic()
# scale_colour_okabe_ito(alpha =.5) +
theme_classic()
plot_3




fitted_lines_all<-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest() %>%
  rename(As = group)


library(ggsci)
plot_3 <- fitted_lines_all %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = As, colour = As),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =As),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="Frequentist MI from six previous waves + full attack wave",
       y= "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_colour_npg(alpha =.5) + theme_classic()
 # scale_colour_okabe_ito(alpha =.5) +
  theme_classic()
plot_3


ggsave(
  plot_3,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "plot_3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



## Graph this model with Bayesian model

plot_bayes_3  <- m1_test_model_plot$`Wave:As`  + scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="Bayesian MI from six previous waves + full attack wave",
       y= "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha =.8) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()


#yola

compare_graph <- plot_3 + plot_bayes_3 + plot_annotation(tag_levels = "i",
                                                         title = "No practical difference between frequentist and Bayesian estimation") + plot_layout(guides = 'collect')

compare_graph
>>>>>>> 9f5a16fc41e9927370908b86e724d0204590a0ed

ggsave(
  compare_graph,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "compare_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)




#
# #  create -- monster ------------------------------------------------
# #create zero data for multiple imputation with lage
# all_d_selected <- readRDS(here::here("_posts","mus","mods","all_d_selected"))
# ka3 <- readRDS(here::here("_posts","mus","mods","ka3"))
#
#
# m_zero <- ka3 %>%
#   filter((As == 0))%>%
#   droplevels() %>%
#   dplyr::select(
#     Id,
#     Wave,
#     As,
#     Ys,
#     pol_bz,
#     rel_bz,
#     partner_bz,
#     parent_bz,
#     nzdep_bz,
#     male_2z,
#     employed_bz,
#     edu_bz,
#     ubran_bz,
#     EthnicCats_b,
#     GenCohort,
#     TSCORE_i,
#   )%>%
#   arrange(Wave,Id)
#
# str(m_zero$As)
# summary(m_zero$TSCORE_i)
#
#
# head(m_zero)
# dim(m_zero)
# # works
#
#
# # get pre-attack sample (above)
#
# m_pre<- all_d_selected %>%
#   dplyr::select(c(-wave, -dys, -yrs))%>% # wrong
#   dplyr::mutate(As = as.factor(As))%>%
#   group_by(Id) %>%
#   arrange(Wave,Id)
#
# dim(m_pre)
# head(m_pre)
#
#
# str(m_pre$As)
# length(unique(m_pre$Id))
# str(m_pre$As)
#
# # bind rows and arrange
# m_zerof <-full_join(m_pre,m_zero)%>%
#   arrange(Wave,Id) %>%
#   group_by(Id) %>%
#   mutate(y = Ys )%>%
#   rename(y0 = Ys )%>%
#   mutate(wave = Wave) %>%
#   rename(a = As) %>%
#   mutate(y0lag = lag(y0)) %>%
#   mutate(y1 = NA,
#          y1lag = NA) %>%
#   ungroup() %>%
#   mutate(sub0 = rep(TRUE,nrow(.)),
#          sub1 = rep(FALSE, nrow(.)))
#
#
# table1::table1(~sub0 |  wave* a, data = m_zerof, overall=FALSE)
#
# str(m_zerof)
# # Make for 1s -------------------------------------------------------------
#
# m_one <- ka3 %>%
#   filter((As == 1))%>%
#   droplevels() %>%
#   dplyr::select(
#     Id,
#     Wave,
#     As,
#     Ys,
#     pol_bz,
#     rel_bz,
#     partner_bz,
#     parent_bz,
#     nzdep_bz,
#     male_2z,
#     employed_bz,
#     edu_bz,
#     ubran_bz,
#     EthnicCats_b,
#     GenCohort,
#     TSCORE_i,
#   )%>%
#   arrange(Wave,Id)
#
# str(m_one$As)
# summary(m_one$TSCORE_i)
#
# m_preOne <- m_pre %>%
#   mutate(Ys = NA) %>%
#   dplyr::mutate(As = as.factor(As))%>%
#   mutate(As = "1")%>%
#   arrange(Wave,Id)
#
# # check
# table1::table1(~Ys |  Wave* As, data = m_preOne, overall=FALSE)
#
# m_onef <-full_join(m_preOne, m_one)%>%
#   arrange(Wave,Id) %>%
#   group_by(Id) %>%
#   mutate(y = Ys )%>%
#   rename(y1 = Ys )%>%
#   mutate(wave = Wave) %>%
#   rename(a = As) %>%
#   mutate(y1lag = lag(y1)) %>%
#   mutate(y0 = NA,
#          y0lag = NA) %>%
#   ungroup() %>%
#   mutate(sub0 = rep(FALSE,nrow(.)),
#          sub1 = rep(TRUE, nrow(.)))
#
#
# table1::table1(~sub0 |  Wave* a, data = m_onef, overall=FALSE)
#
#
# #combine data
# m_join <-full_join(m_zerof,m_onef)%>%
#   mutate(dys = TSCORE_i - min(TSCORE_i),
#          yrs = dys/365) %>%
#   mutate(wave = as.numeric(wave)-1)
#
#
# table1::table1(~y |  Wave , data = m_join, overall=FALSE)
# table1::table1(~y |  Wave * sub0 , data = m_join, overall=FALSE)
#
# length(unique(m_join$Id))
# m_join <- m_join %>%
#   arrange(Wave,Id)
# saveRDS(m_join, here::here("_posts","mus","mods","m_join"))
#
# # monsterMI model ---------------------------------------------------------
# #https://discourse.mc-stan.org/t/brms-mi-for-discrete-outcomes-in-an-irt-model/22870/12
#
# #https://discourse.mc-stan.org/t/imputing-missing-responses-in-multivariate-ordinal-data/21644
#
# # pol_bz,
# # rel_bz,
# # partner_bz,
# # parent_bz,
# # nzdep_bz,
# # male_2z,
# # employed_bz,
# # edu_bz,
# # ubran_bz,
# # EthnicCats_b,
# # GenCohort,
#
#
# head(m_join)
# bf0 <- bf(y0 | mi()  + subset(sub0) ~ wave +  (1  | Id), sigma ~ 0 + wave)
# bf1 <- bf(y1 | mi()  + subset(sub1) ~ wave +  (1  | Id), sigma ~ 0 + wave)
#
#
# bform <- bf0 + bf1 + set_rescor(FALSE)
#
# fit_monster <-brms::brm(
#   bform,
#   data = m_join,
#   seed = 1234,
#   prior = c(prior(normal(0, .5), class = b, resp = y0),
#             prior(normal(0, .5), class = b, resp = y1),
#             prior(normal(log(1), 1), class = b, dpar = sigma, resp = y0),
#             prior(normal(log(1), 1), class = b, dpar = sigma, resp = y1)),
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here("_posts", "mus", "mods", "fit_monster"))
# #
# #
# # fit_monster2 <-brms::brm(
# #   bf_mon2,
# #   data = m_join,
# #   seed = 1234,
#   prior = c(prior(normal(0, 1), class = b),
#             prior(normal(log(1), 1), class = b, dpar = sigma)),
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here("_posts", "mus", "mods", "fit_monster2"))

#save_pars=save_pars(grou
# =
# seed = 1234,
# warmup = 1000,
# init=0,
# iter = 2000,
# chains = 4,
# backend = "cmdstanr")
#
# + subset(sub0) + index(Id)



#
# m_zero <- ka3 %>%
#   filter((As == 0))%>%
#   droplevels() %>%
#   dplyr::select(
#     Id,
#     Wave,
#     As,
#     Ys,
#     pol_bz,
#     rel_bz,
#     partner_bz,
#     parent_bz,
#     nzdep_bz,
#     male_2z,
#     employed_bz,
#     edu_bz,
#     ubran_bz,
#     EthnicCats_b,
#     GenCohort,
#     TSCORE_i,
#   )%>%
#   arrange(Wave,Id)
#
# str(m_zero$As)
# summary(m_zero$TSCORE_i)


# table(bind_zero$Wave)
# # relevel
# levels = c("Time4", "Time5","Time6","Time7","Time8","Time9",
#            "Time10","Time11","Time12")
#
# bind_zero$Wave <- fct_relevel(bind_zero$Wave, levels)
# levels(bind_zero$Wave)
# str(bind_zero$Wave)
# head(bind_zero)
## Make numeric var
#
# bind_zero1 <-bind_zero %>%
#   mutate(wave = as.numeric(Wave) -1) %>%
#   dplyr::select(-c(dys,yrs)) %>%  # not working
#   arrange(Wave, Id) %>%
#   group_by(Id) %>%
#   mutate(dys = TSCORE_i - min(TSCORE_i) )%>% # Not used
#   mutate(yrs = dys/365) %>% # note used
#   ungroup()
#
# summary(bind_zero1$wave)
# # correct
# table1::table1(~ Ys|Wave * As, data = bind_zero1, overall=FALSE)
#
#
# # create lag
#
#
#
# ## BAYESIAN MODEL THAT IS USED IS UNDER THE FUTURES SCRIPT
#
# #MODEL USED IS UNDER FUTURES SCRIPT
#


#
#
# # bayesian 3  -------------------------------------------------------------
# # us this
# listbayes<- readRDS(here::here("_posts", "mus", "mods", "listbayes"))
#
# # prob way to go.
#
# test<-brms::brm(
#   bf(Ys ~ As  *  Wave)
#   data = listbayes,
#   seed = 1234,
#   warmup = 100,
#   iter = 200,
#   init=0,
#   chains = 1,
#   backend = "cmdstanr")
# prior_summary(test)
#
# #save_pars=save_pars(group=FALSE),
# # file = here::here("_posts", "mus", "mods", "b_m_dfCw"))
#
# b_m0 <- brms::brm(
#   bf(Ys ~ As  *  Wave + (1|Id)),
#   family = gaussian,
#   data = listbayes,
#   # c(prior(lognormal(0,.25), nl= "As"),
#   #   prior(lognormal(0,.2), nl= "Wave"),
#   #   prior(normal(0,.5), class = b, coef = "As"),
#   #   prior(normal(0,.5), class = b, coef = "Wave"),
#   #   prior(normal(0,.25),  class= b, coef = "As1:Wave")),
#   seed = 1234,
#   warmup = 1000,
#   init=0,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr")
#
#
# tab_b_m0 <- model_parameters(b_m0, test = c("pd"))
#
# plot(tab_b_m0, show_labels = TRUE)
#
#
#
# #conditions <- data.frame(Wave = seq(0, 1, by = 0.05)) +
# # plot(conditional_effects(fit, effects = "Wave:As",
# #                          conditions = conditions))
#
#
# # graph bayes 3 -----------------------------------------------------------


#Used
conditional3 <- conditional_effects(m_cluster,  "Wave:As",  ndraws = 100, spaghetti = T)
conditional3

saveRDS(conditional3,here::here("_posts","mus","mods","conditional3"))
library(ggplot2)
library(ggplot2)
library(patchwork)


bayes_3 <- conditional3$`Wave:As`  +  scale_y_continuous(limits=c(4.0,4.48)) +
  labs(subtitle="Multiple imputation: sample from previous 6 waves prior to attack + full attack wave sample + 2 post-attack waves",
       y= "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.5)




#scale_color_viridis_d(option = "cividis")

bayes_3
bayes_3main

ggsave(
  bayes_3,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =12,
  units = "in",
  filename = "bayes_3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

bayes_3main

ggsave(
  bayes_3main,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =12,
  units = "in",
  filename = "bayes_3main.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)




# BAYES USE 3 str prior # 2 -----------------------------------------------------------------
listbayes<- readRDS(here::here("_posts", "mus", "mods", "listbayes"))
#saveRDS(listbayes, here::here("_posts", "mus",  "listbayes"))

# from other script = proper list and also test model
ameliadata<- readRDS(here::here("_posts", "mus", "mods", "ameliadata"))
testdata<- readRDS(here::here("_posts", "mus", "mods", "testdata"))


# default priors

system.time( def_fit  <- brms::brm_multiple(
  bf(Ys ~ As  *  Wave + (0 + As||Id),
     sigma ~ 0 + As , set_rescor(rescor = FALSE)),
  family = gaussian,
  data = ameliadata,
  # c(
  #   prior(normal(.2,.25), class = b, coef = "As1"),
  #   prior(normal(0.05,.25), class = b, coef = "Wave"),
  #   prior(normal(0,.25),  class= b, coef = "As1:Wave"),
  #   prior(normal(log(1),.5),  class= b, coef = "As0", dpar = "sigma"),
  #   prior(normal(log(1),.5),  class= b, coef = "As1", dpar = "sigma"),
  #   prior(student_t(3,4.1,.1), class = Intercept),
  #   prior(student_t(3,0,.1), class = "sd"),
  #   prior(student_t(3,0,1), class = "sd", coef = "As0", group = "Id"),
  #   prior(student_t(3,0,1), class = "sd", coef = "As1", group = "Id")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "def_fit")
) )



b_m1g <- brms::brm_multiple(
    bf(Ys ~ As  *  Wave + (1|Id),
       sigma ~ 0 + As, set_rescor(rescor = FALSE)),
    family = gaussian,
    data = listbayes,
    c(prior(normal(0,0.5), class = b, coef = "As1"),
      prior(normal(0,0.5), class = b, coef = "Wave"),
      prior(normal(0,0.5),  class= b, coef = "As1:Wave"),
      prior(normal(0,0.5),  class= b, coef = "As1", dpar = "sigma")),
    seed = 1234,
    warmup = 1000,
    iter = 2000,
    chains = 4,
   # backend = "cmdstanr",
    file = here::here("_posts", "mus", "mods", "b_m1g")
  )





b_fit <- brms::brm_multiple(
  bf(Ys ~ As  *  Wave + (1|Id),
     sigma ~ As, set_rescor(rescor = FALSE)),
  family = gaussian,
  data = listbayes,
  c(prior(normal(0,0.5), class = b, coef = "As1"),
    prior(normal(0,0.5), class = b, coef = "Wave"),
    prior(normal(0,0.5),  class= b, coef = "As1:Wave"),
    prior(normal(0,0.5),  class= b, dpar = "sigma")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "b_fit")
)


b_m1 <- brms::brm_multiple(
  bf(Ys ~ As  *  Wave + (1|Id),
     sigma ~ As, set_rescor(rescor = FALSE)),
  family = gaussian,
  data = listbayes,
  c(prior(normal(0,.5), class = b, coef = "As1"),
    prior(normal(0,.5), class = b, coef = "Wave"),
    prior(normal(0,.5),  class= b, coef = "As1:Wave"),
    prior(normal(log(1),0.5),  class= b, coef = "As1", dpar = "sigma")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "b_m1")
)

summary_b_m1 <- summary(b_m1)
#saveRDS(summary_b_m1, here::here("_posts", "mus", "mods","summary_b_m1"))
summary_b_m1<- readRDS(here::here("_posts", "mus", "mods","summary_b_m1"))

desc_prior <- bayestestR::describe_posterior(b_m1,  test = NULL)
#saveRDS(desc_prior, here::here("_posts", "mus", "mods","desc_prior"))

lazerhawk::brms_SummaryTable(b_m1, panderize=F)
tab
tab %>%
  kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims", digits = 2) %>%
  print()


stancode(b_m1)
prior_summary(b_m1)

plot(b_m1)
traceps <- plot(b_m1, N = 7, plot=F)
traceps

traceps[[1]]
g1 + theme_classic()

# emtrends for main model -------------------------------------------------


# bit run
# em_trends_bm1 <- b_m1 %>%
#   emtrends(~ Wave,
#            var = "Wave",
#            at = list(As = c("0","1"),
#                      Wave = seq(0, 2, by = 0.05)),
#            epred = TRUE,
#            re_formula = NA)%>%
#   gather_emmeans_draws()
#



# Time intensive
# contrast_bm1 <- estimate_contrasts( b_m1,
#                     contrast = "As",
#                     at = c("Wave") )

trend_bm1 <- estimate_slopes(eb_m1,
                             trend = "Wave",
                             at =c("Wave", "As"), length = 3,
                             ci = 0.95)


%>%
  kbl("latex",booktabs = TRUE,digits=2)

# graph emtrends ----------------------------------------------------------



# // generated with brms 2.16.8
# functions {
# }
# data {
#   int<lower=1> N;  // total number of observations
#   vector[N] Y;  // response variable
#   int<lower=1> K;  // number of population-level effects
#   matrix[N, K] X;  // population-level design matrix
#   int<lower=1> K_sigma;  // number of population-level effects
#   matrix[N, K_sigma] X_sigma;  // population-level design matrix
#   // data for group-level effects of ID 1
#   int<lower=1> N_1;  // number of grouping levels
#   int<lower=1> M_1;  // number of coefficients per level
#   int<lower=1> J_1[N];  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_1_1;
#   int prior_only;  // should the likelihood be ignored?
# }
# transformed data {
#   int Kc = K - 1;
#   matrix[N, Kc] Xc;  // centered version of X without an intercept
#   vector[Kc] means_X;  // column means of X before centering
#   int Kc_sigma = K_sigma - 1;
#   matrix[N, Kc_sigma] Xc_sigma;  // centered version of X_sigma without an intercept
#   vector[Kc_sigma] means_X_sigma;  // column means of X_sigma before centering
#   for (i in 2:K) {
#     means_X[i - 1] = mean(X[, i]);
#     Xc[, i - 1] = X[, i] - means_X[i - 1];
#   }
#   for (i in 2:K_sigma) {
#     means_X_sigma[i - 1] = mean(X_sigma[, i]);
#     Xc_sigma[, i - 1] = X_sigma[, i] - means_X_sigma[i - 1];
#   }
# }
# parameters {
#   vector[Kc] b;  // population-level effects
#   real Intercept;  // temporary intercept for centered predictors
#   vector[Kc_sigma] b_sigma;  // population-level effects
#   real Intercept_sigma;  // temporary intercept for centered predictors
#   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
#   vector[N_1] z_1[M_1];  // standardized group-level effects
# }
# transformed parameters {
#   vector[N_1] r_1_1;  // actual group-level effects
#   real lprior = 0;  // prior contributions to the log posterior
#   r_1_1 = (sd_1[1] * (z_1[1]));
#   lprior += normal_lpdf(b[1] | 0.2, 0.5);
#   lprior += normal_lpdf(b[2] | 0.06, 0.5);
#   lprior += normal_lpdf(b[3] | 0, 0.5);
#   lprior += student_t_lpdf(Intercept | 3, 4, 2.5);
#   lprior += student_t_lpdf(Intercept_sigma | 3, 0, 2.5);
#   lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
#   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
# }
# model {
#   // likelihood including constants
#   if (!prior_only) {
#     // initialize linear predictor term
#     vector[N] mu = Intercept + Xc * b;
#     // initialize linear predictor term
#     vector[N] sigma = Intercept_sigma + Xc_sigma * b_sigma;
#     for (n in 1:N) {
#       // add more terms to the linear predictor
#       mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
#     }
#     for (n in 1:N) {
#       // apply the inverse link function
#       sigma[n] = exp(sigma[n]);
#     }
#     target += normal_lpdf(Y | mu, sigma);
#   }
#   // priors including constants
#   target += lprior;
#   target += std_normal_lpdf(z_1[1]);
# }
# generated quantities {
#   // actual population-level intercept
#   real b_Intercept = Intercept - dot_product(means_X, b);
#   // actual population-level intercept
#   real b_sigma_Intercept = Intercept_sigma - dot_product(means_X_sigma, b_sigma);
# }
# > prior_summary(b_m1)
# prior     class      coef group resp  dpar nlpar bound       source
# (flat)         b                                             default
# normal(0.2, 0.5)         b       As1                                      user
# normal(0, 0.5)         b  As1:Wave                                      user
# normal(0.06, 0.5)         b      Wave                                      user
# (flat)         b                      sigma             (vectorized)
# (flat)         b       As1            sigma             (vectorized)
# student_t(3, 4, 2.5) Intercept                                             default
# student_t(3, 0, 2.5) Intercept                      sigma                  default
# student_t(3, 0, 2.5)        sd                                             default
# student_t(3, 0, 2.5)        sd              Id                        (vectorized)
# student_t(3, 0, 2.5)        sd Intercept    Id                        (vectorized)

# test rand intercept  ----------------------------------------------------
# see This on why we might not want
# this: https://discourse.mc-stan.org/t/clarification-on-the-meaning-of-in-brms-syntax/8716/3

b_m1_b <- brms::brm(
  bf(Ys ~ As  *  Wave + (1 + As||Id),
     sigma ~ As, set_rescor(rescor = FALSE)),
  family = gaussian,
  data = listbayes,
  c(prior(normal(.2, .5), class = b, coef = "As1"),
    prior(normal(.06,.5), class = b, coef = "Wave"),
    prior(normal(0,.5),  class= b, coef = "As1:Wave")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "b_m1_b")
)
# stancode(b_m1_b)
# prior_summary(b_m1_b)
# // generated with brms 2.16.8
# functions {
# }
# data {
#   int<lower=1> N;  // total number of observations
#   vector[N] Y;  // response variable
#   int<lower=1> K;  // number of population-level effects
#   matrix[N, K] X;  // population-level design matrix
#   int<lower=1> K_sigma;  // number of population-level effects
#   matrix[N, K_sigma] X_sigma;  // population-level design matrix
#   // data for group-level effects of ID 1
#   int<lower=1> N_1;  // number of grouping levels
#   int<lower=1> M_1;  // number of coefficients per level
#   int<lower=1> J_1[N];  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_1_1;
#   vector[N] Z_1_2;
#   int prior_only;  // should the likelihood be ignored?
# }
# transformed data {
#   int Kc = K - 1;
#   matrix[N, Kc] Xc;  // centered version of X without an intercept
#   vector[Kc] means_X;  // column means of X before centering
#   int Kc_sigma = K_sigma - 1;
#   matrix[N, Kc_sigma] Xc_sigma;  // centered version of X_sigma without an intercept
#   vector[Kc_sigma] means_X_sigma;  // column means of X_sigma before centering
#   for (i in 2:K) {
#     means_X[i - 1] = mean(X[, i]);
#     Xc[, i - 1] = X[, i] - means_X[i - 1];
#   }
#   for (i in 2:K_sigma) {
#     means_X_sigma[i - 1] = mean(X_sigma[, i]);
#     Xc_sigma[, i - 1] = X_sigma[, i] - means_X_sigma[i - 1];
#   }
# }
# parameters {
#   vector[Kc] b;  // population-level effects
#   real Intercept;  // temporary intercept for centered predictors
#   vector[Kc_sigma] b_sigma;  // population-level effects
#   real Intercept_sigma;  // temporary intercept for centered predictors
#   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
#   vector[N_1] z_1[M_1];  // standardized group-level effects
# }
# transformed parameters {
#   vector[N_1] r_1_1;  // actual group-level effects
#   vector[N_1] r_1_2;  // actual group-level effects
#   real lprior = 0;  // prior contributions to the log posterior
#   r_1_1 = (sd_1[1] * (z_1[1]));
#   r_1_2 = (sd_1[2] * (z_1[2]));
#   lprior += normal_lpdf(b[1] | 0.2, 0.5);
#   lprior += normal_lpdf(b[2] | 0.06, 0.5);
#   lprior += normal_lpdf(b[3] | 0, 0.5);
#   lprior += student_t_lpdf(Intercept | 3, 4, 2.5);
#   lprior += student_t_lpdf(Intercept_sigma | 3, 0, 2.5);
#   lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
#   - 2 * student_t_lccdf(0 | 3, 0, 2.5);
# }
# model {
#   // likelihood including constants
#   if (!prior_only) {
#     // initialize linear predictor term
#     vector[N] mu = Intercept + Xc * b;
#     // initialize linear predictor term
#     vector[N] sigma = Intercept_sigma + Xc_sigma * b_sigma;
#     for (n in 1:N) {
#       // add more terms to the linear predictor
#       mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n];
#     }
#     for (n in 1:N) {
#       // apply the inverse link function
#       sigma[n] = exp(sigma[n]);
#     }
#     target += normal_lpdf(Y | mu, sigma);
#   }
#   // priors including constants
#   target += lprior;
#   target += std_normal_lpdf(z_1[1]);
#   target += std_normal_lpdf(z_1[2]);
# }
# generated quantities {
#   // actual population-level intercept
#   real b_Intercept = Intercept - dot_product(means_X, b);
#   // actual population-level intercept
#   real b_sigma_Intercept = Intercept_sigma - dot_product(means_X_sigma, b_sigma);
# }
# > prior_summary(b_m1_b)
# prior     class      coef group resp  dpar nlpar bound       source
# (flat)         b                                             default
# normal(0.2, 0.5)         b       As1                                      user
# normal(0, 0.5)         b  As1:Wave                                      user
# normal(0.06, 0.5)         b      Wave                                      user
# (flat)         b                      sigma             (vectorized)
# (flat)         b       As1            sigma             (vectorized)
# student_t(3, 4, 2.5) Intercept                                             default
# student_t(3, 0, 2.5) Intercept                      sigma                  default
# student_t(3, 0, 2.5)        sd                                             default
# student_t(3, 0, 2.5)        sd              Id                        (vectorized)
# student_t(3, 0, 2.5)        sd       As1    Id                        (vectorized)
# student_t(3, 0, 2.5)        sd Intercept    Id                        (vectorized)

tab_m1_b <- lazerhawk::brms_SummaryTable(b_m1_b, panderize=F)
tab_m1_b


tab_m1_b %>%
  kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims", digits = 2) %>%
  print()


# graph test --------------------------------------------------------------



conditional_b_m1_b <- plot(conditional_effects(b_m1_b,  "Wave:As",  ndraws = 200, spaghetti = T), points = F)
saveRDS(conditional_b_m1_b,here::here("_posts","mus","mods","conditional_b_m1_b"))


bayes_b_m1_b <-
  conditional_b_m1_b$`Wave:As`  +  scale_y_continuous(limits = c(4.1, 4.48)) +
  labs(
    title = "Bayesian estimate of potential outcome trajectories for Muslim acceptance Years 2018-2021",
    # labs(subtitle = "Multiple imputation: sample from previous 6 waves prior to attack + full attack wave sample + 2 post-attack waves",
    y = "Muslim Warmth",
    x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.3)
)

bayes_b_m1_c <- conditional_b_m1$`Wave:As`  +  scale_y_continuous(limits=c(4.1,4.48)) +
  labs(subtitle = "Distinct & uncorrelated sigmas for attack vs. no-attack",
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.3)

# compare sensitivity analysis
bayes_b_m1_c2 <- conditional_b_m1$`Wave:As`  +  scale_y_continuous(limits=c(4.04,4.48)) +
  labs(title = "Bayesian estimate of potential outcome trajectories for Muslim acceptance Time10-Time12",
       y= "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.3)



#scale_color_viridis_d(option = "cividis")

bayes_b_m1
bayes_b_m1_c

ggsave(
  bayes_b_m1,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "bayes_b_m1.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

ggsave(
  bayes_b_m1,
  path = here::here(here::here("_posts", "mus", "mods")),
  width = 12,
  height =12,
  units = "in",
  filename = "bayes_b_m1.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)



# graph strong prior ------------------------------------------------------


conditional_b_m1 <- plot(conditional_effects(b_m1,  "Wave:As",  ndraws = 200, spaghetti = T), points = F)
#saveRDS(conditional_b_m1,here::here("_posts","mus","mods","conditional_b_m1"))
conditional_b_m1 <- readRDS(here::here("_posts","mus","mods","conditional_b_m1"))


bayes_b_m1 <-
  conditional_b_m1$`Wave:As`  +  scale_y_continuous(limits = c(4.1, 4.48)) +
  labs(
    title = "Bayesian estimate of potential outcome trajectories for Muslim acceptance Time10-Time12",
    # labs(subtitle = "Multiple imputation: sample from previous 6 waves prior to attack + full attack wave sample + 2 post-attack waves",
    y = "Muslim Warmth",
    x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.3)
)

bayes_b_m1_c <- conditional_b_m1$`Wave:As`  +  scale_y_continuous(limits=c(4.1,4.48)) +
  labs(subtitle = "Distinct & uncorrelated sigmas for attack vs. no-attack",
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.3)

# compare sensitivity analysis
bayes_b_m1_c2 <- conditional_b_m1$`Wave:As`  +  scale_y_continuous(limits=c(4.04,4.48)) +
  labs(title = "Bayesian estimate of potential outcome trajectories for Muslim acceptance Time10-Time12",
       y= "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") + scale_colour_okabe_ito(alpha =.3)

bayes_b_m1_c2

#scale_color_viridis_d(option = "cividis")

bayes_b_m1
bayes_b_m1_c

ggsave(
  bayes_b_m1,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "bayes_b_m1.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

ggsave(
  bayes_b_m1,
  path = here::here(here::here("_posts", "mus", "mods")),
  width = 12,
  height =12,
  units = "in",
  filename = "bayes_b_m1.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


bayes_b_m1

# compare

compare <- bayes_b_m1_c + bayes_3main + plot_annotation(tag_levels = "i",
                                                        title = "Distinct and uncorrelated sigmas improves variance estimates of trajectories")
compare


# save to mods
ggsave(
  compare,
  path = here::here(here::here("_posts", "mus", "mods")),
  width = 16,
  height =9,
  units = "in",
  filename = "compare.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


# Priors  ----------------------------------------------------------

b_sample_prior_strong <- brms::brm(
  bf(Ys ~ As  *  Wave + (1 + As||Id),
     sigma ~ As, set_rescor(rescor = FALSE)),
  family = gaussian,
  data = listbayes,
  c(prior(normal(.2, .5), class = b, coef = "As1"),
    prior(normal(.06,.5), class = b, coef = "Wave"),
    prior(normal(0,.5),  class= b, coef = "As1:Wave"),
    prior(normal(0,.5),  class= b, dpar = "sigma")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  sample_prior = "only",
  chains = 4,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "b_sample_prior_strong"))

# info
stancode(b_sample_prior_strong)
prior_summary(b_sample_prior_strong)

# plot
conditional_b_sample_prior_strong <- plot(conditional_effects(b_sample_prior_strong,  "Wave:As",  ndraws = 200, spaghetti = T), points = F)

saveRDS(conditional_b_sample_prior_strong,here::here("_posts","mus","mods","conditional_b_sample_prior_strong"))

lazerhawk::brms_SummaryTable(b_sample_prior_strong, panderize=F)




# weak priors -------------------------------------------------------------
b_sample_prior_weak <- brms::brm(
  bf(Ys ~ As  *  Wave + (1 + As||Id),
     sigma ~ As, set_rescor(rescor = FALSE)),
  family = gaussian,
  data = listbayes,
  # c(prior(normal(.2, .5), class = b, coef = "As1"),
  #   prior(normal(.06,.5), class = b, coef = "Wave"),
  #   prior(normal(0,.5),  class= b, coef = "As1:Wave"),
  prior = c(prior(normal(0,1),  class= b, coef = "As1", dpar = "sigma"),
            prior(student_t(3,0,2.5), class = b, dpar = "sigma" )),
  seed = 1234,
  warmup = 500,
  iter = 1000,
  sample_prior = "only",
  chains = 5,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "b_sample_prior_weak"))


conditional_b_sample_prior_strong <- plot(conditional_effects(b_sample_prior_strong,  "Wave:As",  ndraws = 200, spaghetti = T), points = F)

saveRDS(conditional_b_sample_prior_strong,here::here("_posts","mus","mods","conditional_b_sample_prior_strong"))

lazerhawk::brms_SummaryTable(b_sample_prior_strong, panderize=F)



bayes_sconditional_b_sample_prior_strong <- conditional_b_sample_prior_strong$`Wave:As` + #scale_y_continuous(limits=c(4.,4.48)) +
  labs(
    title = "Strong prior allows greater scope for posterior exploration than is needed.",
    y = "Muslim Warmth",
    x = "Years: 2018-2020/21; N = 47948"
  ) + scale_colour_okabe_ito(alpha = .5)

bayes_sconditional_b_sample_prior_strong
#scale_color_viridis_d(option = "cividis")


ggsave(
  bayes_sconditional_b_sample_prior_strong,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =12,
  units = "in",
  filename = "bayes_sconditional_b_sample_prior_strong.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


# ANOTHER METHOD ----------------------------------------------------------
b_sample_prior_strong <- brms::brm(
  bf(Ys ~ As  *  Wave + (1 + As|Id),
     sigma ~ As, set_rescor(rescor = FALSE)),
  family = gaussian,
  data = listbayes,
  c(prior(normal(.2, .5), class = b, coef = "As1"),
    prior(normal(.06,.5), class = b, coef = "Wave"),
    prior(normal(0,.5),  class= b, coef = "As1:Wave"),
    prior(normal(0,.5),  class= b, coef = "As1", dpar = "sigma")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  sample_prior = "only",
  chains = 4,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "b_sample_prior_strong"))

#
# fit1 <- brm(
#   bf(Ys ~ 0 + As * Wave + (0 + As | Id),
#      sigma ~ 0 + As,
#      set_rescor(FALSE)),
#   prior = c(
#     prior(normal(0, 1), class = b),
#     prior(
#       normal(0, 1),
#       class = b,
#       dpar = "sigma"
#     )),
#   family = gaussian,
#   data = listbayes,
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here("_posts", "mus", "mods", "fit1")
# )
#
#
#
#
#
# c(prior(normal(.2, .5), class = b, coef = "As1"),
#   prior(normal(.06,.5), class = b, coef = "Wave"),
#   prior(normal(0,.5),  class= b, coef = "As1:Wave"),
#   prior(normal(0,.5),  class= b, coef = "As1", dpar = "sigma")),
# seed = 1234,
# warmup = 1000,
# iter = 2000,
# sample_prior = "only",
# chains = 4,
# backend = "cmdstanr",
# file = here::here("_posts", "mus", "mods", "b_sample_prior_strong"))
#

# sensitivity anlaysis ----------------------------------------------------
# imagine strong time effect
listbayes <- readRDS(  file = here::here("_posts", "mus", "mods", "listbayes"))
b_sens <- brms::brm(
  bf(Ys ~  As * Wave + (1 | Id)),
  data = listbayes,
  prior = c(set_prior("constant(0.12)", class = "b", coef = "Wave")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "b_sens")
)
prior_summary(b_sens)


lazerhawk::brms_SummaryTable(b_sens, panderize=F)


b_sens2 <- brms::brm(
  bf(Ys ~ As  *  Wave + (1 | Id),
     sigma ~ As, set_rescor(rescor = FALSE)),
  family = gaussian,
  data = listbayes,
  prior = c(set_prior("constant(0.12)", class = "b", coef = "Wave")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("_posts", "mus", "mods", "b_sens2")
)

prior_summary(b_sens2)


tab <- lazerhawk::brms_SummaryTable(b_sens2, panderize=F)


tab
tab %>%
  kable(booktabs = T, "latex", caption =  "Sensitivity for effect of attack on warmth to Muslims: baseline trajectory fixed 2 $\times$ stronger than estimate", digits = 2) %>%
  print()


# graph sense analysis ----------------------------------------------------


conditional_sens2 <- plot(conditional_effects(b_sens2,  "Wave:As",  ndraws = 200, spaghetti = T), points = F)
saveRDS(conditional_sens2,here::here("_posts","mus","mods","conditional_sens2"))

conditional_sens2 <- readRDS(here::here("_posts","mus","mods","conditional_sens2"))


# save graph
cluster_plot_st <- readRDS( here::here("_posts", "mus", "mods", "cluster_plot_st"))



bayes_sense2 <- conditional_sens2$`Wave:As` + scale_y_continuous(limits=c(4.,4.48)) +
  labs(
    title = "Sensitivity analysis: Bayesian estimate of potential outcome trajectories for Muslim acceptance Time10-Time12",
    y = "Muslim Warmth",
    x = "Years: 2018-2020/21; N = 47948"
  ) + scale_colour_okabe_ito(alpha = .5)

#scale_color_viridis_d(option = "cividis")
bayes_sense2

ggsave(
  bayes_sense2,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =12,
  units = "in",
  filename = "bayes_sense2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

compare_traj <- cluster_plot_st + bayes_sense2 + plot_annotation(tag_levels = "i",
                                                            title = "Sensitivity analysis: assuming twice the baseline growth rate does not affect post-attack inference")  +
  plot_layout(guides = 'collect')
compare_traj

ggsave(
  compare_traj,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height =9,
  units = "in",
  filename = "compare_traj.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)
summary_b_m1 <- summary(b_m1)
saveRDS(summary_b_m1, here::here("_posts","mus","mods","summary_b_m1"))

summary_b_m1

mp_b_m1 <-model_parameters(b_m1, test = "pd")
saveRDS(mp_b_m1, here::here("_posts","mus","mods","mp_b_m1"))

#
# summary(b_m1)
# Family: gaussian
# Links: mu = identity; sigma = log
# Formula: Ys ~ As * Wave + (1 | Id)
# sigma ~ As
# Data: listbayes (Number of observations: 286870)
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
#
# Group-Level Effects:
#   ~Id (Number of levels: 47948)
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.69      0.00     0.68     0.70 1.00     1356     2342
#
# Population-Level Effects:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept           4.15      0.01     4.14     4.17 1.00     5560     3309
# sigma_Intercept     0.44      0.00     0.44     0.45 1.00     3891     3460
# As1                 0.21      0.01     0.19     0.22 1.00     6568     3179
# Wave                0.06      0.01     0.05     0.07 1.00     6561     3420
# As1:Wave           -0.03      0.01    -0.04    -0.02 1.00     6924     2956
# sigma_As1          -0.14      0.00    -0.14    -0.13 1.00     4352     3489
#
# Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).



# hypothesis test ---------------------------------------------------------
prior_summary(b_m1)
#h <- c("As1:Wave = Wave", "As1:Wave = 0")
#hyp <- hypothesis(b_m1, h)


hyp0 <- hypothesis(b_m1, "As1 + As1:Wave > As1 - Wave")
p0 <-plot(hyp0, plot=F)
out_h0<-p0[[1]] + labs(title = "Post-attack warmth trajectory remains positive") +
  scale_fill_colorblind() + theme_classic()
out_h0

hyp1 <- hypothesis(b_m1, "Wave + As1:Wave  =  Wave")
p1 <-plot(hyp1, plot=F)
out_h1<-p1[[1]] + labs(title = "Post-attack warmth trajectory is shallower than no-attack") +
  scale_fill_colorblind() + theme_classic()
out_h1



hyp2<- hypothesis(b_m1, "sigma_As1 = sigma_Intercept", class = "b")
p2 <-plot(hyp2, plot=F)

out_h2<- p2[[1]]+ labs(title = "Sigma of attack condition is lower than sigma of no-attack") + scale_fill_colorblind() + theme_classic()
out_h2

hyp3 <- hypothesis(b_m1, "As1 >0")
p3 <-plot(hyp3, plot=F)
out_h3<-p3[[1]] + labs(title = "Strong main positive effect of attack on warmth") +
  scale_fill_colorblind() + theme_classic()
out_h3


# combine graph
bayes_hypothesis <- out_h3/  out_h0 / out_h1 / out_h2 + plot_annotation(
  title = "Bayesian hypothesis tests",
  tag_levels = "a")
bayes_hypothesis
# show plot
results <- bayes_b_m1 + (out_h3/ out_h0 / out_h1 / out_h2 ) + plot_annotation(tag_levels = "i") +
  plot_layout(ncol = 2, widths = c(2, 1))
results
# save plot
ggsave(
  bayes_hypothesis,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height =9,
  units = "in",
  filename = "bayes_hypothesis.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

ggsave(
  results,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height =9,
  units = "in",
  filename = "results.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)



# posterior predictive checks ---------------------------------------------

pp <-pp_check(b_m1 ) + ylab("Muslim Warmth ")
pp

brms::stancode(b_m1)
tidys <- tidy_stan(b_m1, effects = "fixed")

tab_model(b_m1, show.ci = .89)

color_scheme_set("red")
pp_plot <- ppc_dens_overlay(y = b_m1$Ys,
                            yrep = posterior_predict(b_m1, draws = 50))


color_scheme_set("mix-blue-pink")
#color_scheme_set("mix-brightblue-gray")

p <- mcmc_trace(b_m1,  pars = c("mu", "tau"), n_warmup = 1000,
                facet_args = list(nrow = 2, labeller = label_parsed))
p + facet_text(size = 15)

color_scheme_set("darkgray")
mc_test <- mcmc_parcoord(b_m1, np = np_cp)

mc_test

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
  dplyr::filter(Wave == 2017 | Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2017 =  ifelse(Wave == 2017 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2017, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Id,Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2017", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b)%>%
  dplyr::mutate(TSCORE_i = ifelse(YearMeasured == 0 & Wave==2018, TSCORE_b + 365,
                                  ifelse(YearMeasured == 0 & Wave==2019, TSCORE_b + 730,
                                         ifelse(YearMeasured == 0 & Wave==2020, TSCORE_b + 1095, TSCORE))))%>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
  ungroup() %>%  # Create TSCORE for when people would have been observed
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)))%>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>%
  dplyr::mutate(wave = as.numeric(Wave)-1) %>%
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
  dplyr::mutate(male_2z = if_else(Wave == "2017", (as.numeric(Male))/2, NA_real_)) %>%
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
  ungroup()%>%
  arrange(Id,Wave)

levels(km_all4$Wave) <- c("Time9", "Time10", "Time11","Time12")


# tw<-km_all4%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check
sum(is.na(km_all4$TSCORE_i))



# correct
table1::table1(~ TSCORE_i|Wave, data = km_all4, overall=FALSE)

# latex summary
kable(x, format ="latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all4, overall=FALSE)

obtl<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all4, overall=FALSE)


kable(obtl, format ="latex", booktabs = TRUE)

x <- table1::table1(~  Age +Edu + Employed + EthnicCats  +
                      Male + NZdep + Parent + Partner +
                      Religious + Pol.Orient  + Urban|Wave, data = km_all4, overall = FALSE)
x

t1kable(x, format ="latex")

# create new data set
library(tidyverse)
kf4  <- km_all4 %>%
  group_by(Id) %>%  # All ZEROS
  # filter(Wave != "Time9")%>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (ifelse(Wave=="Time10" & Attack == 1|Wave == "Time11"|Wave =="Time12", 0,
                      ifelse(Wave=="Time10" & Attack == 0| Wave=="Time9", 1, Attack)))) %>%
  mutate(Ys = ifelse(Wave=="Time10" & Attack == 1|Wave == "Time11"|Wave =="Time12", NA,
                     ifelse(Wave=="Time10" & Attack == 0 | Wave == "Time9", NA, Warm.Muslims)))%>%
  ungroup() %>%
  arrange(Id,Wave)

# dat_all N
length(unique(kf4$Id))
str(kf4$As)
# correct


# Test NAs = Correct
table1::table1(~Ys|Wave *as.factor(As), data =kf4, overall=F)



## Bind - double dataset to creat missing values
ka4 <- km_all4%>%
  bind_rows(kf4)%>%
  arrange(Id,Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka4$Ys)
# Check

# Missing data problem
t2<- table1::table1( ~Ys | Wave*As, data = ka4, overall=F)
t2

t1kable(t2, format ="latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link 4 dfs for zero estimate -----------------------------------------

km_zero4 <- ka4 %>%
  filter((As == 0))%>%
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
  )%>%
  arrange(Wave,Id)

str(km_zero4$As)
head(km_zero4)
dim(km_zero4)
summary(km_zero4$wave)
summary(km_zero4$Wave)

summary(km_zero4$dys)

# works
table1::table1(~ Ys|Wave * As, data = km_zero4, overall=FALSE)

# correct
table1::table1(~ Ys|Wave * As, data = km_zero4, overall=FALSE)


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

imputed0_4<- amelia(
  set.seed=1234,
  bind_zero4,
  cs= c("Id"),
  ts= c("wave"), # use days
  m = 10, # number of imputations
  ords = "Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys"),
  lags="Ys",
  leads="Ys",
  polytime = 2, # Allow polynomial
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(bind_zero4)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_4, here::here("_posts","mus","mods", "imputed0_4"))


# check means, looks good!

table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_4$imputations$imp10, overall=FALSE)


# impute 4 1s ---------------------------------------------------------------

km_one4 <- ka4 %>%
  filter((As == 1 & Wave !="Time9"))%>%
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
  )%>%
  mutate(wave = wave - min(wave),
         dys = dys - min(dys))%>%
  arrange(Wave,Id)

# check
length(unique(km_one4$Id))
summary(km_one4$wave)
summary(km_one4$dys)
hist(km_one4)


#check looks good
table1::table1(~ Ys|Wave * As, data = km_one4, overall=FALSE)

# make data frame
km_one4<-as.data.frame(km_one4)

# create bounds for Ys
head(km_one4)

bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1_4<- amelia(
  set.seed=1234,
  km_one4,
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  ords = "Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys"),
  lags="Ys",
  leads="Ys",
  polytime = 2, # Allow polynomial
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(km_one4)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1_4, here::here("_posts","mus","mods","imputed1_4"))

# check means, looks good

table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_4$imputations$imp10, overall=FALSE)




# 4 make frames compatible --------------------------------------------------
imputed0_4<-readRDS(here::here("_posts","mus","mods", "imputed0_4"))
imputed1_4<-readRDS(here::here("_posts","mus","mods", "imputed1_4"))




# bind data frames --------------------------------------------------------
# levels_old <- c("Time4","Time5","Time6","Time7","Time8","Time9",
#                 "Time10","Time11","Time12")
# newlevels = c("Time10","Time11","Time12")

m <- 10
zero4 <- NULL
for (i in 1:m) {
  zero4$imputations$imp[[i]] <- imputed0_4$imputations[[i]] %>%
    dplyr::filter(Wave !="Time9") %>%
    droplevels()%>%
    arrange(Wave, Id)
}

one4 <- NULL

for(i in 1:m) {
  one4$imputations$imp[[i]] <- imputed1_4$imputations[[i]] %>%
    dplyr::filter(Wave !="Time9") %>%
    droplevels()%>%
    arrange(Wave, Id)
}

zero4
m <- 10
imps_bind4 <- NULL
for (i in 1:m) {
  imps_bind4$imputations$imp[[i]] <-
    dplyr::bind_rows(zero4$imputations$imp[[i]],
                     one4$imputations$imp[[i]])%>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave =="Time10"| Wave =="Time11"| Wave == "Time12") %>%
    dplyr::mutate(Wave = as.numeric(Wave)-1) %>%
    dplyr::mutate(days_n = dys - min(dys) ) %>% # new zero
    dplyr::mutate(yrs =  days_n/365 ) %>% # new
    droplevels()%>%
    arrange(Wave,Id)
}



# Works
hist(imps_bind4$imputations$imp[[1]]$yrs)
table(imps_bind4$imputations$imp[[1]]$Wave)


# save
saveRDS(imps_bind4, here::here("_posts", "mus", "mods", "imps_bind4"))
#imps_bind4 <- readRDS(here::here("_posts", "mus", "mods", "imps_bind4"))

# read
imps_bind4 <- readRDS(here::here("_posts", "mus", "mods", "imps_bind4"))

# make list for bayesian models
listbayes4 <-imps_bind4$imputations$imp

# save list for bayesian models
saveRDS(listbayes4, here::here("_posts", "mus", "mods", "listbayes4"))

#readRDS
#listbayes4<- readRDS(here::here("_posts", "mus", "mods", "listbayes4"))


# ML model 4 ----------------------------------------------------------------


# model
m<-10
model_all4<-NULL
for(i in 1:m) {
  model_all4$model[[i]] <- lmer(Ys ~ As * Wave + (1|Id),
                          data = imps_bind4$imputations$imp[[i]])
}

# table
tab<-pool_parameters(model_all4$model)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

yola


# Kurz model

fitted_lines4 <-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all4$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()


=======


# Kurz model

fitted_lines4 <-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all4$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()


>>>>>>> 9f5a16fc41e9927370908b86e724d0204590a0ed

plot_4 <- fitted_lines4 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.5)) +
<<<<<<< HEAD
  labs(subtitle="Multiple imputation: sample from previous 1 wave prior to attack + 2 waves post-attack",
       y= "Muslim Warmth",
       x = "Years: 2017-2020/21; N = 17014") + scale_colour_okabe_ito(alpha =.5)
=======
  labs(subtitle="MI from one previous wave",
       y= "Muslim Warmth",
       x = "Years: 2017-2020/21; N = 17014") +   scale_colour_npg(alpha =.5)+  theme_classic() #+
#scale_colour_okabe_ito(alpha =.5)
>>>>>>> 9f5a16fc41e9927370908b86e724d0204590a0ed
plot_4


ggsave(
  plot_4,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
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
  dplyr::filter(Wave == 2016 | Wave == 2017 | Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
  dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Id,Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2016", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b)%>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave==2017, TSCORE_b + 365,
    ifelse(YearMeasured == 0 & Wave==2018, TSCORE_b + 730,
           ifelse(YearMeasured == 0 & Wave==2019, TSCORE_b + 1094, # leap
                  ifelse(YearMeasured == 0 & Wave==2020, TSCORE_b + 1459, TSCORE)))))%>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
  ungroup() %>%  # Create TSCORE for when people would have been observed
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)))%>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>%
  dplyr::mutate(wave = as.numeric(Wave)-1) %>%
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
  dplyr::mutate(male_2z = if_else(Wave == "2016", (as.numeric(Male))/2, NA_real_)) %>%
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
  ungroup()%>%
  arrange(Id,Wave)

levels(km_all5$Wave) <- c("Time8", "Time9", "Time10", "Time11","Time12")


# tw<-km_all5%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check



# correct
table1::table1(~ TSCORE_i|Wave, data = km_all5, overall=FALSE)

# latex summary
kable(x, format ="latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all5, overall=FALSE)

obtl<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all5, overall=FALSE)


kable(obtl, format ="latex", booktabs = TRUE)

x <- table1::table1(~  Age +Edu + Employed +
                      EthnicCats  + Male + NZdep +
                      Parent + Partner + Religious +
                      Pol.Orient  + Urban|Wave,
                    data = km_all5, overall = FALSE)
x

t1kable(x, format ="latex")

# create new data set
library(tidyverse)
kf5  <- km_all5 %>%
  group_by(Id) %>%  # All ZEROS
  # filter(Wave != "Time9")%>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (ifelse(Wave=="Time10" & Attack == 1|Wave == "Time11"|Wave =="Time12", 0,
                      ifelse(Wave=="Time10" & Attack == 0|
                               Wave=="Time9" |
                               Wave =="Time8", 1, Attack)))) %>%
  mutate(Ys = ifelse(
    Wave=="Time10" & Attack == 1|
      Wave == "Time11"|
      Wave =="Time12", NA,
    ifelse(Wave=="Time10" & Attack == 0 |
             Wave == "Time9" |
             Wave=="Time8", NA, Warm.Muslims)))%>%
  ungroup() %>%
  arrange(Id,Wave)

# dat_all N
length(unique(kf5$Id))
str(kf5$As)
# correct


# Test NAs = Correct
table1::table1(~Ys|Wave *as.factor(As), data =kf5, overall=F)



## Bind - double dataset to creat missing values
ka5 <- km_all5%>%
  bind_rows(kf5)%>%
  arrange(Id,Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka5$Ys)
# Check

# Missing data problem
t2<- table1::table1( ~Ys | Wave*As, data = ka5, overall=F)
t2

t1kable(t2, format ="latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link 5 dfs for zero estimate -----------------------------------------

km_zero5 <- ka5 %>%
  filter((As == 0))%>%
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
  )%>%
  arrange(Wave,Id)

str(km_zero5$As)
head(km_zero5)
dim(km_zero5)
summary(km_zero5$wave) # Correct
# works
table1::table1(~ Ys|Wave*As, data = km_zero5, overall=FALSE)

# correct
table1::table1(~ Ys|Wave * As, data = km_zero5, overall=FALSE)


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

imputed0_5<- amelia(
  set.seed=1234,
  bind_zero5,
  cs= c("Id"),
  ts= c("wave"), # use days
  m = 10, # number of imputations
  ords = "Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys"),
  lags="Ys",
  leads="Ys",
  polytime = 2, # Allow polynomial
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(bind_zero5)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_5, here::here("_posts","mus","mods", "imputed0_5"))


# check means, looks good!

table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_5$imputations$imp10, overall=FALSE)


# impute 4 1s ---------------------------------------------------------------

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
  )%>%
  mutate(dys = dys - min(dys),
         wave = wave - min(wave)) %>%
  arrange(Wave,Id)

summary(km_one5$wave)
summary(km_one5$dys)

length(unique(km_one5$Id))

#check looks good
table1::table1(~ Ys|Wave * As, data = km_one5, overall=FALSE)

# make data frame
km_one5<-as.data.frame(km_one5)

# create bounds for Ys
head(km_one5)
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1_5<- amelia(
  set.seed=1234,
  km_one5,
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  ords = "Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys"),
  lags="Ys",
  leads="Ys",
  polytime = 2, # Allow polynomial
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(km_one5)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1_5, here::here("_posts","mus","mods","imputed1_5"))

# check means, looks good

table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_5$imputations$imp10, overall=FALSE)




# 5 make frames compatible --------------------------------------------------
imputed0_5<-readRDS(here::here("_posts","mus","mods", "imputed0_5"))
imputed1_5<-readRDS(here::here("_posts","mus","mods", "imputed1_5"))




# bind data frames --------------------------------------------------------
# levels_old <- c("Time4","Time5","Time6","Time7","Time8","Time9",
#                 "Time10","Time11","Time12")
# newlevels = c("Time10","Time11","Time12")

m <- 10
zero5 <- NULL
for (i in 1:m) {
  zero5$imputations$imp[[i]] <- imputed0_5$imputations[[i]] %>%
    dplyr::filter(Wave !="Time9"| Wave != "Time8") %>%
    droplevels()%>%
    arrange(Wave, Id)
}

one5 <- NULL

for(i in 1:m) {
  one5$imputations$imp[[i]] <- imputed1_5$imputations[[i]] %>%
    dplyr::filter(Wave !="Time9"| Wave != "Time8") %>%
    droplevels()%>%
    arrange(Wave, Id)
}


m <- 10
imps_bind5 <- NULL
for (i in 1:m) {
  imps_bind5$imputations$imp[[i]] <-
    dplyr::bind_rows(zero5$imputations$imp[[i]],
                     one5$imputations$imp[[i]])%>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave =="Time10"| Wave =="Time11"| Wave == "Time12") %>%
    dplyr::mutate(Wave = as.numeric(Wave)-3) %>%
    dplyr::mutate(days_n = dys - min(dys) ) %>% # new zero
    dplyr::mutate(yrs =  days_n/365 ) %>% # new yzer
    droplevels()%>%
    arrange(Wave,Id)
}



# Works
hist(imps_bind5$imputations$imp[[1]]$yrs)
summary(imps_bind5$imputations$imp[[1]]$Wave)
# save
saveRDS(imps_bind5, here::here("_posts", "mus", "mods", "imps_bind5"))

# read
#imps_bind5 <- readRDS(here::here("_posts", "mus", "mods", "imps_bind5"))

imps_bind5 <- readRDS(here::here("_posts", "mus", "mods", "imps_bind5"))

# make list for bayesian models
listbayes5 <-imps_bind5$imputations$imp

# save list for bayesian models
#saveRDS(listbayes5, here::here("_posts", "mus", "mods", "listbayes5"))

#readRDS
#listbayes5<- readRDS(here::here("_posts", "mus", "mods", "listbayes5"))


# ML model 5 ----------------------------------------------------------------

# model
m<-10
model_all5<-NULL
for(i in 1:m) {
  model_all5$model[[i]] <- lmer(Ys ~ As * Wave + (1|Id),
                          data = imps_bind5$imputations$imp[[i]])
}

# table
tab<-pool_parameters(model_all5$model)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)


# Kurz model

fitted_lines5 <-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all5$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()



plot_5 <- fitted_lines5 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.5)) +
<<<<<<< HEAD
  labs(subtitle="Multiple imputation: sample from previous 2 waves prior to attack + 2 waves post-attack",
       y= "Muslim Warmth",
       x = "Years: 2016-2020/21; N=21796") + scale_colour_okabe_ito(alpha =.5)
=======
  labs(subtitle="MI from two previous waves",
       y= "Muslim Warmth",
       x = "Years: 2016-2020/21; N=21796") +  scale_colour_npg(alpha =.5)+  theme_classic()# +
# scale_colour_okabe_ito(alpha =.5)
>>>>>>> 9f5a16fc41e9927370908b86e724d0204590a0ed
plot_5


ggsave(
  plot_5,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "plot_5.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
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
#   path = here::here(here::here("_posts", "mus", "figs")),
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
  dplyr::filter(Wave == 2012 |
                  Wave == 2013 |
                  Wave == 2014 |
                  Wave == 2015 |
                  Wave == 2016 |
                  Wave == 2017 |
                  Wave == 2018 |
                  Wave ==2019 |
                  Wave ==2020) %>%
  dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu))%>%
  arrange(Id,Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2012", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b)%>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave==2013, TSCORE_b + 365,
    ifelse(YearMeasured == 0 & Wave==2014, TSCORE_b + 730,
           ifelse(YearMeasured == 0 & Wave==2015, TSCORE_b + 1094,  # leap
                  ifelse(YearMeasured == 0 & Wave==2016, TSCORE_b + 1459,
                         ifelse(YearMeasured ==0 & Wave ==2017, TSCORE_b + 1824,
                                ifelse(YearMeasured == 0 & Wave == 2018,  TSCORE_b + 3248,
                                       ifelse(YearMeasured == 0 & Wave == 2019, TSCORE_b + 3512,
                                              ifelse(YearMeasured == 0 & Wave == 2020, TSCORE_b + 3877, TSCORE)))))))))%>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
  ungroup() %>%  # Create TSCORE for when people would have been observed
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)))%>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack)%>%
  dplyr::mutate(yrs =  (dys/365))%>%
  dplyr::mutate(wave = as.numeric(Wave)-1) %>%
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
  dplyr::mutate(male_2z = if_else(Wave == "2012", (as.numeric(Male))/2, NA_real_)) %>%
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
  ungroup()%>%
  arrange(Wave,Id)
str(km_all9$Wave)
levels(km_all9$Wave) <- c("Time4","Time5", "Time6", "Time7", "Time8","Time9", "Time10", "Time11", "Time12")


# tw<-km_all4%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check
sum(is.na(km_all9$TSCORE_i))



# correct
table1::table1(~ TSCORE_i|Wave, data = km_all9, overall=FALSE)

# latex summary
kable(x, format ="latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all9, overall=FALSE)

table1::table1(~ Warm.Muslims|Wave*as.factor(As), data = km_all9, overall=FALSE)

kable(obtl, format ="latex", booktabs = TRUE)


x <- table1::table1(~  Age +Edu + Employed + EthnicCats  + Male + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban|Wave, data = km_all9, overall = FALSE)
x

t1kable(x, format ="latex")

table1::table1(~  as.factor(Warm.Muslims) |Wave*as.factor(As) , data = km_all9)
km_all9
# create new data set
library(tidyverse)
kf9  <- km_all9 %>%
  group_by(Id) %>%  # All ZEROS
  # filter(Wave != "Time9")%>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = ifelse(Wave=="Time10" & Attack == 1|
                       Wave == "Time11"|
                       Wave =="Time12", 0,
                     ifelse(Wave=="Time10" & Attack == 0|
                              Wave=="Time9" & Attack == 0|
                              Wave=="Time8" & Attack == 0|
                              Wave=="Time7" & Attack == 0|
                              Wave=="Time6" & Attack == 0|
                              Wave=="Time5" & Attack == 0|
                              Wave=="Time4" & Attack == 0, 1,
                            Attack))) %>%
  mutate(Ys = ifelse(Wave=="Time9" & Attack == 1 |
                       Wave=="Time10" & Attack == 1|
                       Wave == "Time11"|Wave =="Time12", NA,
                     ifelse(Wave=="Time10" & Attack == 0 |
                              Wave == "Time9" |
                              Wave == "Time8" |
                              Wave == "Time7" |
                              Wave == "Time6" |
                              Wave == "Time5" |
                              Wave == "Time4" , NA,
                            Warm.Muslims)))%>%
  ungroup() %>%
  arrange(Wave,Id)

# dat_all N
length(unique(kf9$Id))
str(kf9$As)
# correct


# Test NAs = Correct
table1::table1(~(Ys)|Wave*as.factor(As), data =kf9, overall=F)



## Bind - double dataset to creat missing values
ka9 <- km_all9%>%
  bind_rows(kf9)%>%
  arrange(Wave,Id) %>%
  mutate(As = as.factor(As)) %>%
  arrange(Wave,Id)
# select(-c(Warm.Muslims,Attack))

head(ka9$Ys)
# Check

# Missing data problem
t2<- table1::table1( ~Ys | Wave*as.factor(As), data = ka9, overall=F)
t2

t1kable(t2, format ="latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link 9 dfs for zero estimate -----------------------------------------

km_zero9 <- ka9 %>%
  filter((As == 0))%>%
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
  )%>%
  arrange(Wave,Id)

str(km_zero9$As)
head(km_zero9)
dim(km_zero9)
summary(km_zero9$wave)
# works
table1::table1(~ Ys|Wave*As, data = km_zero9, overall=FALSE)

# correct
table1::table1(~ Ys|Wave * As, data = km_zero9, overall=FALSE)


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

imputed0_9<- amelia(
  set.seed=1234,
  bind_zero9,
  cs= c("Id"),
  ts= c("wave"), # use days
  m = 10, # number of imputations
  ords = "Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys"),
  lags="Ys",
  leads="Ys",
  polytime = 2, # Allow polynomial, if 3, reverts to pre w10 mean!
  intercs = F, # too many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(bind_zero9)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_9, here::here("_posts","mus","mods", "imputed0_9"))

# check means, looks good

table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_9$imputations$imp10, overall=FALSE)


# impute 9 1s ---------------------------------------------------------------
km_one9 <- ka9 %>%
  dplyr::filter(
    # (As == 1 & Wave =="Time4")|
    #  (As == 1 & Wave =="Time5")|
    # (As == 1 & Wave =="Time6")|
    # (As == 1 & Wave =="Time7")|
    # (As == 1 & Wave =="Time8")|
    #  (As == 1 & Wave =="Time9")|
    (As == 1 & Wave =="Time10") |
      (As == 1 & Wave =="Time11")|
      (As == 1 & Wave =="Time12"))%>%
  droplevels()%>%
  mutate(wave = wave -6) %>%
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
  )%>%
  arrange(Wave,Id)
str(km_one9)
table(ka9$Wave)
summary(km_one9$wave)


#check looks good
table1::table1(~ Ys|Wave * As, data = km_one9, overall=FALSE)

# make data frame
km_one9<-as.data.frame(km_one9)
km_one9
# create bounds for Ys
head(km_one9)
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1_9<- amelia(
  set.seed=1234,
  km_one9,
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  ords = "Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys"),
  lags="Ys",
  leads="Ys",
  polytime = 2, # Allow polynomial, if 3, reverts to pre w10 mean!
  intercs = F, # too many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(bind_zero9)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1_9, here::here("_posts","mus","mods","imputed1_9"))


# check means, looks good

table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed1_9$imputations$imp10, overall=FALSE)




# 9 make frames compatible --------------------------------------------------
imputed0_9<-readRDS(here::here("_posts","mus","mods", "imputed0_9"))
imputed1_9<-readRDS(here::here("_posts","mus","mods", "imputed1_9"))




# bind data frames --------------------------------------------------------
# levels_old <- c("Time4","Time5","Time6","Time7","Time8","Time9",
#                 "Time10","Time11","Time12")
# newlevels = c("Time10","Time11","Time12")

m <- 10
zero9<- NULL
for (i in 1:m) {
  zero9$imputations$imp[[i]] <- imputed0_9$imputations[[i]] %>%
    dplyr::filter(Wave =="Time10"|Wave =="Time11"|Wave =="Time12") %>%
    droplevels()%>%
    arrange(Wave, Id)
}

one9 <- NULL

for(i in 1:m) {
  one9$imputations$imp[[i]] <- imputed1_9$imputations[[i]] %>%
    dplyr::filter(Wave =="Time10"|Wave =="Time11"|Wave =="Time12") %>%
    droplevels()%>%
    arrange(Wave, Id)
}


m <- 10
imps_bind9 <- NULL
for (i in 1:m) {
  imps_bind9$imputations$imp[[i]] <-
    dplyr::bind_rows(zero9$imputations$imp[[i]],
                     one9$imputations$imp[[i]])%>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave =="Time10"|Wave =="Time11"|Wave =="Time12") %>%
    dplyr::mutate(Wave = as.numeric(Wave)-1) %>%
    dplyr::mutate(days_n = dys - min(dys) ) %>% # new zero
    dplyr::mutate(yrs =  days_n/365 ) %>% # new yzer
    droplevels()%>%
    arrange(Wave,Id)
}



# Works
hist(imps_bind9$imputations$imp[[1]]$Wave)

# save
saveRDS(imps_bind9, here::here("_posts", "mus", "mods", "imps_bind9"))

# read
imps_bind9 <- readRDS(here::here("_posts", "mus", "mods", "imps_bind9"))

# make list for bayesian models
listbayes9 <-imps_bind9$imputations$imp

# save list for bayesian models
saveRDS(listbayes9, here::here("_posts", "mus", "mods", "listbayes9"))

#readRDS
#listbayes9<- readRDS(here::here("_posts", "mus", "mods", "listbayes9"))


# ML model 9 ----------------------------------------------------------------
# model
m<-10
model_all9<-NULL
for(i in 1:m) {
  model_all9$model[[i]] <- lmer(Ys ~ As * Wave + (1|Id),
                          data = imps_bind9$imputations$imp[[i]])
}

# table
tab<-pool_parameters(model_all9$model)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

# Kurz model

fitted_lines9 <-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all9$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()



plot_9 <- fitted_lines9 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.5)) +
<<<<<<< HEAD
  labs(subtitle="Multiple imputation: sample from previous 6 waves prior to attack + 2 waves post-attack",
=======
  labs(subtitle="MI from previous six previous waves",
>>>>>>> 9f5a16fc41e9927370908b86e724d0204590a0ed
       y= "Muslim Warmth",
       x = "Waves: 2012-2020/21, N = 11799") +   scale_colour_npg(alpha =.5) + theme_classic() # +
scale_colour_okabe_ito(alpha=.5)


plot_9



ggsave(
  plot_9,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "plot_9.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)

# # bayes 9 -----------------------------------------------------------------
#
# bayes_9 <- conditional9$`Wave:As`  +  scale_y_continuous(limits=c(4.0,4.48)) +
#   labs(subtitle="Multiple imputation: sample from previous 6 waves prior to attack + 2 waves post-attack",
#        y= "Muslim Warmth",
#        x = "Waves: 2012-2020/21, N = 11799") + scale_colour_okabe_ito(alpha=.5)
#
# #scale_color_viridis_d(option = "cividis")
#
# bayes_9
#
#
# ggsave(
#   bayes_9,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 12,
#   height =9,
#   units = "in",
#   filename = "bayes_9.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1000
# )




# impute stationary  ---------------------------------------------------------------


km_zero_st18 <- ka3 %>%
  filter((As == 0 &  Wave == "Time10"))%>%
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
  )%>%
  mutate(wave = as.numeric(Wave) -1) %>%
  arrange(Wave,Id)


summary(km_zero_st18$Wave)
#check looks good
table1::table1(~ Ys|Wave * As, data = km_zero_st18, overall=FALSE)
head(km_zero_st18)
dim(km_zero_st18)
# make data frame
km_zero_st18<-as.data.frame(km_zero_st18)

# create bounds for Ys
head(km_zero_st18)
# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_st18<- amelia(
  set.seed=1234,
  km_zero_st18,
  # cs= c("Id"),
  # ts= c("wave"),
  m = 10, # number of imputations
  ords="Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys","yrs","TSCORE_i", "Id", "wave"),
  # lags="Ys",
  # leads="Ys",
  # polytime = 2, #  polynomials perhaps not sensible given
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(km_zero_st18)) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_st18, here::here("_posts","mus","mods", "imputed0_st18"))

table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st18$imputations$imp10, overall=FALSE)



km_zero_st19 <- ka3 %>%
  filter((As == 0 & Wave == "Time10")|(As == 0 & Wave == "Time11"))%>%
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
  )%>%
  mutate(wave = as.numeric(Wave) -1) %>%
  arrange(Wave,Id)



summary(km_zero_st19$wave)
#check looks good
table1::table1(~ Ys|Wave * As, data = km_zero_st19, overall=FALSE)
head(km_zero_st19)
dim(km_zero_st19)
# make data frame
km_zero_st19<-as.data.frame(km_zero_st19)

# create bounds for Ys
head(km_zero_st19)
# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_st19<- amelia(
  set.seed=1234,
  km_zero_st19,
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  ords="Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys","yrs","TSCORE_i"),
  lags="Ys",
  #leads="Ys",
  #polytime = 2, #  polynomials perhaps not sensible given
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(km_zero_st19)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_st19, here::here("_posts","mus","mods", "imputed0_st19"))

table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st19$imputations$imp10, overall=FALSE)



km_zero_st20 <- ka3 %>%
  filter((As == 0 & Wave == "Time10")|(As == 0 & Wave == "Time12"))%>%
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
  )%>%
  mutate(wave = as.numeric(Wave) -1) %>%
  arrange(Wave,Id)



summary(km_zero_st20$wave)
#check looks good
table1::table1(~ Ys|Wave * As, data = km_zero_st20, overall=FALSE)
head(km_zero_st20)
dim(km_zero_st20)
# make data frame
km_zero_st20<-as.data.frame(km_zero_st20)

# create bounds for Ys
head(km_zero_st20)
# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_st20<- amelia(
  set.seed=1234,
  km_zero_st20,
  cs= c("Id"),
  ts= c("wave"),
  m = 10, # number of imputations
  ords="Ys",
  noms = c(
    "EthnicCats_b","GenCohort"
  ),
  idvars=c("Wave","As","dys","yrs","TSCORE_i"),
  lags="Ys",
  #leads="Ys",
  #polytime = 2, #  polynomials perhaps not sensible given
  intercs = F, # to many vars
  bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(km_zero_st20)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_st20, here::here("_posts","mus","mods", "imputed0_st20"))

table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp1, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp2, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp3, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp4, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp5, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp6, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp7, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp8, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp9, overall=FALSE)
table1::table1(~ Ys|Wave * As, data = imputed0_st20$imputations$imp10, overall=FALSE)


## Get ones from above

imp1 <- transform(imputed1, Wave = as.character(Wave))
imp18z <- transform(imputed0_st18, Wave = as.character(Wave))
imp19z <- transform(imputed0_st19, Wave = as.character(Wave))
imp20z <- transform(imputed0_st20, Wave = as.character(Wave))

str(imp20z$imputations$imp1$Wave)

# bind data frames --------------------------------------------------------

newlevels = c("Time10","Time11","Time12")

m <- 10
one_st <- NULL
for (i in 1:m) {
  one_st$imputations$imp[[i]] <- imp1$imputations[[i]] %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, newlevels)) %>%
    droplevels()%>%
    dplyr::group_by(Id) %>%
    arrange(Wave,Id)
}

zero_st<- NULL

for(i in 1:m) {
  zero_st$imputations$imp[[i]] <-  dplyr::bind_rows(imp18z$imputations[[i]],
                                                    imp19z$imputations[[i]],
                                                    imp20z$imputations[[i]])%>%
    dplyr::mutate(Wave = as.factor(Wave)) %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, newlevels)) %>%
    arrange(Wave,Id)

}

m <- 10
imps_bind_st<- NULL
for (i in 1:m) {
  imps_bind_st$imputations$imp[[i]] <-
    dplyr::bind_rows(zero_st$imputations$imp[[i]],
                     one_st$imputations$imp[[i]])%>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave =="Time10"| Wave =="Time11"| Wave == "Time12") %>%
    droplevels()%>%
    dplyr::mutate(Wave = as.numeric(Wave)-1)%>%
    dplyr::arrange(Wave,Id)


}

imps_bind_st





# Works!
summary(imps_bind_st$imputations$imp[[1]]$Wave)

# save
saveRDS(imps_bind_st, here::here("_posts", "mus", "mods", "imps_bind_st"))

# read
imps_bind_st <- readRDS(here::here("_posts", "mus", "mods", "imps_bind_st"))

# make list for bayesian models
listbayesST <-imps_bind_st$imputations$imp

# save list for bayesian models
saveRDS(listbayesST, here::here("_posts", "mus", "mods", "listbayesST"))

#readRDS
listbayesST<- readRDS(here::here("_posts", "mus", "mods", "listbayesST"))


# ML model  stationary ----------------------------------------------------------------

# model
m<-10
model_all_st<-NULL
for(i in 1:m) {
  model_all_st$model[[i]] <- lmer(Ys ~ As * Wave + (1|Id), data = imps_bind_st$imputations$imp[[i]])
}

# table
tab<-pool_parameters(model_all_st)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

<<<<<<< HEAD

# Kurz model

fitted_linesST <-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all_st$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()



plot_st <- fitted_linesST %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="Multiple imputation: no previous waves",
       y= "Muslim Warmth",
       x = "Years: 2012-2020/21, N = 47948") + scale_colour_okabe_ito(alpha=.5)


plot_st

=======

# Kurz model

fitted_linesST <-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all_st$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()



plot_st <- fitted_linesST %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.5)) +
  labs(subtitle="MI: no previous waves",
       y= "Muslim Warmth",
       x = "Years: 2012-2020/21, N = 47948") +   scale_colour_npg(alpha =.5) +  theme_classic()#+
scale_colour_okabe_ito(alpha=.5)


plot_st

>>>>>>> 9f5a16fc41e9927370908b86e724d0204590a0ed
ggsave(
  plot_st,
  path = here::here(here::here("_posts", "mus", "mods")), # too large
  width = 12,
  height =9,
  units = "in",
  filename = "plot_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


# bayesian model stationary -----------------------------------------------




# graph bayes stationary  -----------------------------------------------------------
#
#
# #Used
# conditional_st <- plot(conditional_effects(bst_m0,  "Wave:As",  ndraws = 100, spaghetti = T), points = F)
# saveRDS(conditional_st,here::here("_posts","mus","mods","conditional_st"))
#
# bayes_st <- conditional_st$`Wave:As`  +  scale_y_continuous(limits=c(4.0,4.48)) +
#   labs(subtitle="Multiple imputation: no previous waves",
#        y= "Muslim Warmth",
#        x = "Years: 2012-2020/21, N = 47948") + scale_colour_okabe_ito(alpha=.5)

# #scale_color_viridis_d(option = "cividis")
# bayes_st
# ggsave(
#   bayes_st,
#   path = here::here(here::here("_posts", "mus", "mods")), # too large
#   width = 12,
#   height =9,
#   units = "in",
#   filename = "bayes_st.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 800
# )
#


# COMBINED GRAPH ----------------------------------------------------------

<<<<<<< HEAD
robust_waves <- ((plot_9 + plot_3)/( plot_5 + plot_3) / (plot_4 +  plot_st ))+ plot_annotation(
=======
robust_waves <- (plot_9  + plot_5) / (  plot_4 + plot_st )+ plot_annotation(
>>>>>>> 9f5a16fc41e9927370908b86e724d0204590a0ed
  title = "Postive trajectory in post-attack acceptance is robust to multiple imputation strategies",
  subtitle = "Recovery of postive trajectory in counterfactual no-attack condition requires at least two waves prior to baseline",
  tag_levels = "i")


<<<<<<< HEAD
robust_waves <- ((plot_9 + plot_5 ) / (plot_4 +  plot_st ))+ plot_annotation(
  title = "Postive trajectory in post-attack acceptance is robust to multiple imputation strategies",
  subtitle = "Recovery of postive trajectory in counterfactual no-attack condition requires at least two waves prior to baseline",
  tag_levels = "i") + plot_layout(guides = 'collect')
plot_3

robust_waves

# robust_waves <- trajectory_1217 + ((bayes_9 + bayes_3)/( bayes_5 + bayes_3) / (bayes_4 +  bayes_st ))+ plot_annotation(
#   title = "Postive trajectory in post-attack acceptance is robust to multiple imputation strategies",
#   subtitle = "Recovery of postive trajectory in counterfactual no-attack condition requires at least two waves prior to baseline",
#   tag_levels = "i") +
#   plot_layout(guides = 'collect')
=======
robust_waves2 <- plot_time + ((plot_9 + plot_5 ) / (plot_4 +  plot_st ))+ plot_annotation(
  title = "Postive trajectory in post-attack acceptance is robust to multiple imputation strategies",
  subtitle = "Recovery of postive trajectory in counterfactual no-attack condition requires at least two waves prior to baseline",
  tag_levels = "i") + plot_layout(guides = 'collect')


robust_waves
robust_waves2

# robust_waves2 <- trajectory_1217 + ((bayes_9 + bayes_3)/( bayes_5 + bayes_3) / (bayes_4 +  bayes_st ))+ plot_annotation(
# #   title = "Postive trajectory in post-attack acceptance is robust to multiple imputation strategies",
# #   subtitle = "Recovery of postive trajectory in counterfactual no-attack condition requires at least two waves prior to baseline",
# #   tag_levels = "i") +
# #   plot_layout(guides = 'collect')
>>>>>>> 9f5a16fc41e9927370908b86e724d0204590a0ed


robust_waves

ggsave(
  robust_waves,
  path = here::here(here::here("_posts", "mus", "mods")),
<<<<<<< HEAD
  width = 18,
  height =16,
=======
  width = 10,
  height =9,
>>>>>>> 9f5a16fc41e9927370908b86e724d0204590a0ed
  units = "in",
  filename = "robust_waves.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)
ggsave(
  robust_waves2,
  path = here::here(here::here("_posts", "mus", "mods")),
  width = 16,
  height =9,
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
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()



fitted_lines %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) +
  ylab("Muslim Warmth") + theme_m


# z plot method --------------------------------------------------------

#https://solomonkurz.netlify.app/post/2021-10-21-if-you-fit-a-model-with-multiply-imputed-data-you-can-still-plot-the-line/



fitted_lines <-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_all$model[[.]],terms = c("Wave[0:2,by=.01]","As"))))%>%
  data_frame() %>%
  unnest()



fitted_lines %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) +
  ylab("Muslim Warmth") + theme_m



# DAG GRAPH ---------------------------------------------------------------

library(ggplot2)
library(magick)
library(patchwork)
ggsave


dag1 <- image_ggplot(image_read(here::here("_posts","mus", "mods","missing.tiff")),
                     interpolate = T)
dag2 <- image_ggplot(image_read(here::here("_posts","mus", "mods","selection.tiff")),
                     interpolate = T)
dag3 <- image_ggplot(image_read(here::here("_posts","mus", "mods","impute.tiff")),
                     interpolate = T)


dag4 <- image_ggplot(image_read(here::here("_posts","mus", "mods","imputation-graph.tiff")),
                     interpolate = T)

dag4


dag5 <- image_ggplot(image_read(here::here("_posts","mus", "mods","swig-graph.tiff")),
                     interpolate = T)

dag5
library(patchwork)


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
  path = here::here("_posts","mus", "mods"),
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
  path = here::here("_posts","mus", "mods"),
  width = 14,
  height = 7,
  units = "in",
  filename = "missgraph.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 1200
)



# model graphs ------------------------------------------------------------

library(tidybayes)
library(emmeans)


marg_eff_bfCw <- b_m1 %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time10", "Time11","Time12")),
              re_formula = NA)


saveRDS(marg_eff_bfCw,  here::here("_posts", "mus", "mods", "marg_eff_bfCw"))
#marg_eff_bf_df <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_bf_df"))

marg_eff_sens <- b_sens %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time10", "Time11","Time12")),
              re_formula = NA)
saveRDS(marg_eff_sens,  here::here("_posts", "mus", "mods", "marg_eff_sens"))

# sensitivity
marg_eff_attack_0s <- m_use_pr_sens %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)
#saveRDS(marg_eff_attack_0s,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s"))

marg_eff_attack_0s
# marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))



marg_eff_attack_0s2 <- m_use_pr_sens2 %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)
saveRDS(marg_eff_attack_0s,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s2"))

marg_eff_attack_0s2
# marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))

marg_eff_attack_0s3 <- m_use_pr_sens3 %>%
  epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
              re_formula = NA)

saveRDS(marg_eff_attack_0s3,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s3"))



plot_bf_df <- ggplot(
  marg_eff_bf_df,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) +
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes\nModel estimates"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.6))
plot_bf_df

# marg_eff_m_use_pr <- m_use_pr %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
#               re_formula = NA)
# saveRDS(marg_eff_m_use_pr,  here::here("_posts", "mus", "mods", "marg_eff_m_use_pr"))
#
#  <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))

## plot all
# marg_eff_attack_0
plot_baseline_constr <- ggplot(
  marg_eff_attack_0,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) +
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: constrain baseline to Time 10"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.6))
plot_baseline_constr

#
# plot_baseline_sens <- ggplot(
#   marg_eff_attack_0s,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) +
#   scale_y_discrete(limits=rev) +
#     stat_halfeye() +
#     scale_fill_okabe_ito() +
#     labs(
#       x = "Predicted Warmth Response",
#       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#       fill = "Conterfactual Contrasts",
#       subtitle = "Bayesian posterior locations of potential outcomes: Time 10,11,12.\nSensitivity analysis: strong time effect"
#     ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#     theme_pubclean() +
#     theme(legend.position = "bottom"
#     )+scale_x_continuous(limits=c(4,4.6))
# plot_baseline_sens


# plot_baseline_sens2 <- ggplot(
#   marg_eff_attack_0s2,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) +
#   scale_y_discrete(limits=rev) +
#     stat_halfeye() +
#     scale_fill_okabe_ito() +
#     labs(
#       x = "Predicted Warmth Response",
#       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#       fill = "Conterfactual Contrasts",
#       subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: assume strong time effect"
#     ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#     theme_pubclean() +
#     theme(legend.position = "bottom"
#     )+scale_x_continuous(limits=c(4,4.6))
# plot_baseline_sens2


plot_baseline_sens <- ggplot(
  marg_eff_sens,
  aes(
    x = .epred,
    y = Wave,
    fill = as.factor(As)
  ) ) +
  scale_y_discrete(limits=rev) +
  stat_halfeye() +
  scale_fill_okabe_ito() +
  labs(
    x = "Predicted Warmth Response",
    #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
    fill = "Conterfactual Contrasts",
    subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: assume strong time effect"
  ) +
  #  scale_x_continuous(limits=c(4.0,4.6)) +
  theme_pubclean() +
  theme(legend.position = "bottom"
  )+scale_x_continuous(limits=c(4,4.6))
plot_baseline_sens

# ML Graph

d_ml <- ggeffects::ggemmeans(model_all[[3]], terms = c("Wave","As")) + labs(subtitle="maximu")

pl_ml <-plot(d_ml) +
  scale_y_reverse() +
  coord_flip() +
  scale_y_continuous(limits=c(4,4.6))+
  labs(x = "Predicted Warmth Response",
       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
       title = '',
       fill = "Conterfactual Contrasts",
       subtitle = "Frequentist posterior locations of potential outcomes:\nSensitivity analysis: assume strong time effect") +
  theme_pubclean() + theme(legend.position = "bottom")
pl_ml

plot_baseline_constr
outplot <- (pl_ml + plot_baseline_unconstr ) /
  (plot_baseline_constr + plot_baseline_sens2 )  +
  plot_annotation(tag_levels = 'a', title = "Estimation of  predicted marginal effects of the attack under different assumptions")
outplot
ggsave(
  outplot,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "outplot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

# SIMULATE data using simstudy --------------------------------------------

# how many from year 4 in year 10
in10 <- df %>%
  dplyr::select(
    Id,
    Wave,
    TSCORE,
    Warm.Muslims,
    YearMeasured,
  ) %>%
  dplyr::filter(
    Wave == 2012 & YearMeasured == 1 |
      Wave == 2013 & YearMeasured != -1 |
      Wave == 2014 & YearMeasured != -1 |
      Wave == 2015 & YearMeasured != -1 |
      Wave == 2016 & YearMeasured != -1 |
      Wave == 2017 & YearMeasured != -1 |
      Wave == 2018 & YearMeasured != -1
  ) %>%
  dplyr::filter(YearMeasured != -1)%>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 & YearMeasured ==1,1,0 ))%>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold>0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  arrange(Id,Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2012", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b)%>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave==2013, TSCORE_b + 365,
    ifelse(YearMeasured == 0 & Wave==2014, TSCORE_b + 730,
           ifelse(YearMeasured == 0 & Wave==2015, TSCORE_b + 1094,  # leap
                  ifelse(YearMeasured == 0 & Wave==2016, TSCORE_b + 1459,
                         ifelse(YearMeasured ==0 & Wave ==2017, TSCORE_b + 1824, TSCORE))))))%>%
  dplyr::mutate(Attack = as.numeric(ifelse(TSCORE_i >= 3545, 1, 0)))%>% # All 0
  ungroup(Id)

#
table1::table1(~ Warm.Muslims|Wave * Attack, data = in10)
# 5142
#
6382/(41566 +1647) # % missing in Wave 10 ~ 15

library(simstudy)
set.seed(281726)
rm(tdat)
tdat <- km_all3 %>%filter(YearMeasured ==1)
# inspect long data
table1::table1(~Warm.Muslims |Wave , data = all_d)
table1::table1(~Warm.Muslims |Wave * Attack, data = tdat)


#tdef <- defData(varname = "m", dist = "binary", formula = 0.5)
rm(tdef) # remove in case object exists

tdef <- defData( varname = "Y0", dist = "normal", formula = 4.4, variance = 2, id = "id")
tdef <- defData(tdef, varname = "Y1", dist = "normal", formula = "Y0 +  (Y0 * 0.01)", variance = 0,id = "id")
tdef <- defData(tdef, varname = "Y2", dist = "normal", formula = "Y1 +  (Y1 * 0.01)", variance = 0, id = "id")


dtTrial <- genData(47948, tdef)
head(dtTrial)

# check
table1::table1(~Y0 + Y1 + Y2, data = dtTrial)

# make long
dtTime <- addPeriods(dtTrial, nPeriods = 3, idvars = "id", timevars = c("Y0",
                                                                        "Y1",
                                                                        "Y2"), timevarName = "Y")
dtTime



MCAR <- defMiss(varname = "Y", formula = "-1.3", # just over 20% attrition
                logit.link = TRUE, monotonic = TRUE
)
dm <- genMiss(dtTime, MCAR, "id", repeated = TRUE, periodvar = "period")

dObs <- genObs(dtTime, dm, idvars = "id")

# check
dObs[, .(prop.missing = mean(is.na(Y))), keyby = period]

table1::table1(~Y|as.factor(period), data = dObs)

dObs
## Test imputation method for 1s
library(Amelia)

imputed_sim1<- amelia(
  set.seed=1234,
  dObs,
  cs= c("id"),
  ts= c("period"),
  m = 10, # number of imputations
  # idvars=c(""),
  lags="Y",
  leads="Y",
  polytime = 2, #  polynomials perhaps not sensible given
  intercs = F, # to many vars
  # bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(dObs)) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed_sim1, here::here("_posts","mus","mods", "imputed_sim1"))
imputed_sim1<- readRDS(here::here("_posts","mus","mods", "imputed_sim1"))

table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim1$imputations$imp1, overall=FALSE)

# test
imputed_sim1$imputations$imp1$Y


# model
m<-10
model_sim1<-NULL
for(i in 1:m) {
  model_sim1$model[[i]] <- lmer(Y ~ period + (1|id), data = imputed_sim1$imputations[[i]])
}

# table
tab_sim1<-pool_parameters(model_sim1$model)
tab_sim1 # recover effect
tab_sim1 [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab_sim1, show_labels = TRUE)



fitted_lines_sim1 <-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_sim1$model[[.]],terms = c("period[0:2, by =.01]"))))%>%
  data_frame() %>%
  unnest()

library(ggsci)

plot_sim1 <- fitted_lines_sim1 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.0,4.6)) +
  labs(subtitle="MI from previous six previous waves",
       y= "Muslim Warmth",
       x = "Waves: 2012-2020/21, N = 11799") +   scale_colour_npg(alpha =.5) + theme_classic() # +
#scale_colour_okabe_ito(alpha=.5)


plot_sim1




## create data
m<-10
self1 <-NULL
for(i in 1:m) {
  self1$imputations$imp[[i]] <- imputed_sim1$imputations[[i]] %>%
    dplyr::mutate(a = as.factor(rep(1, nrow(.))))%>%
    dplyr::arrange(period, id)
}
str(self1$imputations$imp[[1]])




# SIMULATE ZEROS ----------------------------------------------------------

#Generate data with complete missingness for the zeros


set.seed(281726)

# move data (in case there)
rm(ztdef)

# define yearly outcomes
ztdef <- defData(varname = "Y0", dist = "normal", formula = 3.7, variance = 1)
ztdef <- defData(ztdef, varname = "Y1", dist = "normal", formula = "Y0 +  (Y0 * 0.025)", variance = 0)
ztdef <- defData(ztdef, varname = "Y2", dist = "normal", formula = "Y1 +  (Y1 * 0.025)", variance = 0)
ztdef <- defData(ztdef, varname = "Y3", dist = "normal", formula = "Y2 +  (Y2 * 0.02)", variance = 0)
ztdef <- defData(ztdef, varname = "Y4", dist = "normal", formula = "Y3 +  (Y3 * 0.02)", variance = 0)
ztdef <- defData(ztdef, varname = "Y5", dist = "normal", formula = "Y4 +  (Y4 * 0.015)", variance = 0)
ztdef <- defData(ztdef, varname = "Y6", dist = "normal", formula = "Y5 +  (Y5 * 0.015)", variance = 0)
ztdef <- defData(ztdef, varname = "Y7", dist = "normal", formula = "Y6 +  (Y6 * 0.015)", variance = 0)
ztdef <- defData(ztdef, varname = "Y8", dist = "normal", formula = "Y7 +  (Y7 * 0.015)", variance = 0)



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
zdtTrial<- zdtTrial %>%
  mutate(across(where(is.logical), as.numeric)) # make all numeric

# check
table1::table1(~Y0 + Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8, data = zdtTrial)

# save
saveRDS(zdtTrial, here::here("_posts", "mus", "mods", "zdtTrial"))

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

dx0 <- data.frame(id, Y0, Y1, Y2, Y3, Y4, Y5, Y6, Y7,Y8)

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
zdtTime <- addPeriods(zdtTrial, nPeriods = 9, idvars = "id", timevars = c("Y0",
                                                                          "Y1",
                                                                          "Y2",
                                                                          "Y3",
                                                                          "Y4",
                                                                          "Y5",
                                                                          "Y6",
                                                                          "Y7",
                                                                          "Y8"), timevarName = "Y")

# merge data
zdtTime




MCAR <- defMiss(varname = "Y", formula = "-1.3", # just over 20% attrition
                logit.link = TRUE, monotonic = TRUE
)


zdm <- genMiss(zdtTime, MCAR, "id", repeated = TRUE, periodvar = "period")



zdObs[, .(prop.missing = mean(is.na(Y))), keyby = period]

table1::table1(~Y|as.factor(period), data = zdObs)

tzdObs <- zdObs%>%
  arrange(Y,id) %>%
  group_by(period) %>%
  mutate(Y = ifelse(period == 7 | period == 8, recode(Y, NA),Y))

table1::table1(~Y|as.factor(period), data = tzdObs)


om <- rbind(tzdObs,tbig)

head(om)
length(unique(om$id))


# Inspect data
table1::table1(~Y|as.factor(period), data = om)
str(om)
om <- om %>%
  select(id, period, Y)

saveRDS(om, here::here("_posts", "mus", "mods", "om"))

## Test imputation method for 1s
library(Amelia)
str(om)
om<- as.data.frame(om)
imputed_sim0<- amelia(
  set.seed=1234,
  om,
  cs= c("id"),
  ts= c("period"),
  m = 10, # number of imputations
  # idvars=c(""),
  lags="Y",
  leads="Y",
  polytime = 2, #  polynomials otherwise regression to the mean
  intercs = F, # to many vars
  # bounds = bds, # lower upper bounds to Mus Prej
  empri = .05*nrow(om)) # ridge prior see: Amelia pdf documentation p.23

# save data
saveRDS(imputed_sim0, here::here("_posts","mus","mods", "imputed_sim0"))

table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)
table1::table1(~ Y|as.factor(period), data = imputed_sim0$imputations$imp1, overall=FALSE)

# compare
table1::table1(~Y|as.factor(period), data = zdObs)


# Select only final three waves
m<-10
self0 <-NULL
for(i in 1:m) {
  self0$imputations$imp[[i]] <- imputed_sim0$imputations[[i]] %>%
    dplyr::filter(period == 6 | period == 7 | period == 8) %>%
    dplyr::mutate(a = as.factor(rep(0, nrow(.))))%>%
    dplyr::mutate(period = period -6)%>%
    dplyr::arrange(period, id)
}



# model
m<-10
model_sim0<-NULL
for(i in 1:m) {
  model_sim0$model[[i]] <- lmer(Y ~ period + (1|id), data =  self0$imputations$imp[[i]])
}

# table
tab_simzero <-pool_parameters(model_sim0$model)
tab_simzero
tab_simzero [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab_simzero, show_labels = TRUE)





## create full data
m<-10
model_sim_full<-NULL
for(i in 1:m) {
  model_sim_full$imputations$imp[[i]] <-
    dplyr::bind_rows(self0$imputations$imp[[i]],
                     self1$imputations$imp[[i]])%>%
    dplyr::rename(wave = period) %>%
    arrange(wave,id)
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
sim_list <- readRDS( here::here("_posts","mus","mods", "sim_list"))
model_sim_full<- readRDS(here::here("_posts","mus","mods", "model_sim_full"))

library(lme4)

m<-10
model_sim_rep <-NULL
for(i in 1:m) {
  model_sim_rep$model[[i]] <- lmer(Y ~ a * wave + (1|id), data =  model_sim_full$imputations$imp[[i]])
}

# table
tab<-parameters::pool_parameters(model_sim_rep$model)
tab
tab [,c(1:5)]%>%
  # print_md()%>%
  kbl("latex",booktabs = TRUE,digits=2)

plot(tab, show_labels = TRUE)

ggeffects::ggemmeans( model_sim_rep$model[[2]], terms = c("wave","a"))

library(dplyr)
library(tidyr)
library(broom)


fitted_lines_simfull <-
  tibble(.imp = 1:10) %>%
  mutate( p = map(.imp, ~  ggeffects::ggpredict(model_sim_rep$model[[.]],terms = c("wave[0:2,by=.01]","a"))))%>%
  data_frame() %>%
  unnest()


library(ggsci)
plot_simall <- fitted_lines_simfull %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group, colour = group),
              alpha = 1/10) +
  geom_line(aes(y = predicted, group =group),
            size = 1/4) + theme_clean() +  scale_y_continuous(limits=c(4.05,4.55)) +
  labs(subtitle="Multiple imputation with simulated data",
       y= "Muslim Warmth",
       x = "Waves: 2012-2020/21, Simulated N = 47,978") +   scale_colour_npg(alpha =.5) + theme_classic()  +
scale_colour_npg(alpha=.5)


plot_simall

# compare with bayesian model

# cluster plot in "futures.R"
cluster_plot_st_sim2 <- cluster_plot_st_sim +   labs(subtitle="Bayesian model with real data",
                             y= "Muslim Warmth",
                             x = "Waves: 2012-2020/21, N = 47,978")

sim_plot <- cluster_plot_st_sim2 + plot_simall +
  plot_annotation(tag_levels = "i",  title = "Simulated data that assumes even greater missingness nevertheless recovers approximate data model slopes")

sim_plot

ggsave(
  sim_plot,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "sim_plot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# compare with full data & expected values
table1::table1(~Y0 + Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8, data = zdtTrial)



ggsave(
  plot_simall,
  path = here::here(here::here("_posts", "mus", "figs")),
  width = 12,
  height =9,
  units = "in",
  filename = "plot_simall.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)




# bayesian simulation model -----------------------------------------------


prior = c(
  prior(normal(0.2, 0.25), class = b, coef = "a1"),
  prior(normal(0.05, 0.25), class = b, coef = "wave"),
  prior(normal(0, 0.25),  class = b, coef = "a1:wave"),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "a0",
    dpar = "sigma"
  ),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "a1",
    dpar = "sigma"
  ),
  prior(student_t(3, 4.1, 1), class = Intercept),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "a0",
    group = "id"
  ),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "a1",
    group = "id"
  )
)

bform =   bf(Y ~ a  *  wave + (0 + a || id),
             sigma ~ 0 + a, set_rescor(rescor = FALSE))




system.time(
  m_simulation <- brms::brm_multiple(
    bform,
    family = gaussian,
    data = sim_list,
    prior = prior,
    warmup = 1000,
    iter =  2000,
    chains = 2,
    future = TRUE
    , file = here::here("_posts", "mus", "mods", "m_simulation.rds")
  )
)


# SIMULATE (BAD) DROP NA METHOD -------------------------------------------


# compare with simulation that has no missingness:

dObs2<- dObs

dObs2$a <-rep(1, nrow(dObs))
dObs2

om$a <-rep(0, nrow(om))

om2 <- om %>%
  group_by(id, period)%>%
  dplyr::filter(period == 6 | period == 7 | period == 8) %>%
  dplyr::mutate(period = period -6)%>%
  dplyr::arrange(id, period)

head(dObs2)
head(om2)
om3 <- bind_rows(om2,dObs2)
om3 <- om3 %>%
  mutate(a = as.factor(a),
         wave = as.factor(period))
table(om3$a)

table1::table1(~Y|a * wave, om3)

# save
#saveRDS(om3, here::here("_posts","mus","mods", "om3"))

# model that drops nas.

drop_sim <- lmer(Y ~ a + wave + (1|id), data =  om3) # just inherits the post-attack trend.
model_parameters(drop_sim)

tab_sim_drop <-model_parameters(drop_sim)
tab_sim_drop
plot(tab_sim_drop)


pl_drop_sims <- ggeffects::ggemmeans(drop_sim, terms = c("wave","a"))
pl_drop_sims

drop_sim_graph <- plot( pl_drop_sims ) + labs(
  x = "Waves T10-T12",
  y = "Warmth to Muslims",
  title = "Simulation for multiple imputation: drop NAs."
)
drop_sim_graph


#
estimate_contrasts( drop_sim,
                    contrast = "a",
                    at = c("wave","a") )


sim_list <- om3

#  simulation bayesian model ----------------------------------------------

# not run
b_sim <- brms::brm(
  bf(Y ~ a  *  wave + (1|id),
     sigma ~ a, set_rescor(rescor = FALSE)),
  family = gaussian,
  data = sim_list,
  c(prior(normal(.2, 1), class = b, coef = "a1"),
    prior(normal(0, 1), class = b, coef = "wave"),
    prior(normal(0, 1),  class= b, coef = "a1:wave")),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("_posts", "mus", "mods", "b_sim")
)

summary_b_sim <- summary(b_sim)
saveRDS(summary_b_sim, here::here("_posts", "mus", "mods","summary_b_sim"))
summary_b_sim<- readRDS(here::here("_posts", "mus", "mods","summary_b_sim"))

#desc_prior <- bayestestR::describe_posterior(b_sim,  test = NULL)
#saveRDS(desc_prior, here::here("_posts", "mus", "mods","desc_prior"))








# # old ---------------------------------------------------------------------
#
#
#
# # filter only 0s and only 1s for the As
# d0 <- ka_amelia %>%
#   filter(As==0 )
#
# d18_only0 <- ka_amelia %>%
#   filter(As==0 & Wave == "Time 10")
#
#
# d18_only1 <- ka_amelia %>%
#   filter(As==1 & Wave =="Time 10")
#
#
#
# # need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
# d19_only1 <- ka_amelia %>%
#   filter(Wave =="Time 10" | (As==1 & Wave =="Time 11" ))
#
# d20_only1 <- ka_amelia %>%
#   filter(Wave =="Time 10"|(As==1 & Wave =="Time 11" )|(As==1 & Wave =="Time 12" ))
#
#
#
#
# # old ---------------------------------------------------------------------
#
#
#
#
# # df for ones -------------------------------------------------------------
#
#
# # create new data set
# # library(tidyverse)
# # kf3  <- km_all3 %>%
# #   group_by(Id) %>%  # All ZEROS
# #   # mutate(Attack == as.numeric(Attack))%>%
# #   mutate(As = (ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", 0,
# #                       ifelse(Wave=="Time 10" & Attack == 0, 1, Attack)))) %>%
# #   mutate(Ys = ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", NA,
# #                      ifelse(Wave=="Time 10" & Attack == 0, NA, Warm.Muslims)))%>%
# #   ungroup() %>%
# #   arrange(Wave,Id)
#
#
#
# # Test NAs = Correct
# table1::table1(~Ys|Wave *as.factor(As), data =dat_all_star, overall=F)
#
#
#
# ## Bind - double dataset to creat missing values
# ka3 <- dat_all%>%
#   bind_rows(dat_all_star)%>%
#   arrange(Id,Wave) %>%
#   mutate(As = as.factor(As)) #%>%
# # select(-c(Warm.Muslims,Attack))
#
# head(ka3$Ys)
# # Check
#
# # Missing data problem
# t2<- table1::table1( ~Ys | Wave*As, data = ka3, overall=F)
# t2
#
# t1kable(t2, format ="latex")
# # impute missing data
# # avoid collineraity
#
# ka2_selected <-ka3%>%
#   dplyr::select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b,GenCohort)
#
# ka_amelia<-as.data.frame(ka2_selected)
#
#
# # filter only 0s and only 1s for the As
# d0 <- ka_amelia %>%
#   filter(As==0 )
#
# d18_only0 <- ka_amelia %>%
#   filter(As==0 & Wave == "Time 10")
#
#
# d18_only1 <- ka_amelia %>%
#   filter(As==1 & Wave =="Time 10")
#
#
#
#
#
#
# # need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
# d19_only1 <- ka_amelia %>%
#   filter(Wave =="Time 10" | (As==1 & Wave =="Time 11" ))
#
# d20_only1 <- ka_amelia %>%
#   filter(Wave =="Time 10"|(As==1 & Wave =="Time 11" )|(As==1 & Wave =="Time 12" ))
#
#
# library(Amelia)
#
# # data needs to be a data frame if passed to Amelia
# d0 <-as.data.frame(d0)
#
#
# # We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.
# # assume Y^0|A=2018 = Y^0 2019
#
#
#
#
#
# imputed_0<- amelia(
#   set.seed=1234,
#   d0, #dataset to impute
#   # cs= c("Id"),
#   #ts= c("wave"),
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b","GenCohort"
#   ),
#   idvars=c("Wave","As", "Id","wave"))
#
# saveRDS(imputed_0, here::here("_posts","mus","mods", "imputed_0"))
#
# imputed_18_only0<- amelia(
#   set.seed=1234,
#   d18_only0, #dataset to impute
#   # cs= c("Id"),
#   #ts= c("wave"),
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b","GenCohort"
#   ),
#   idvars=c("Wave","As", "Id","wave"))
#
# saveRDS(imputed_18_only0, here::here("_posts","mus","mods", "imputed_18_only0"))
#
#
# d18_only1<-data.frame(d18_only1)
#
# ## For 2018 v 2020 contrast
# imputed_18only1<- amelia(
#   set.seed=1234,
#   d18_only1, #dataset to impute
#   # cs= c("Id"),
#   # ts= c("wave"),
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b","GenCohort"
#   ),
#   idvars=c("As", "Wave", "Id","wave"))
#
# saveRDS(imputed_18only1, here::here("_posts","mus","mods", "imputed_18only1"))
#
#
# # we impute all the 1s from 2018 seperately.  Y^1|A=2018 \ne Y^1 2019
# d19_only1<-as.data.frame(d19_only1)
#
#
# # This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.
# # Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later.
#
# imputed_19only1<- amelia(
#   d19_only1, #dataset to impute
#   cs= c("Id"),
#   ts= c("wave"),
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b","GenCohort"
#   ),
#   idvars=c("As", "Wave"),
#   lags="Ys",
#   leads="Ys")
#
# saveRDS(imputed_19only1, here::here("_posts","mus","mods", "imputed_19only1"))
#
#
# # This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.
# # Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later.
#
# d20_only1<-as.data.frame(d20_only1)
# imputed_20only1 <- amelia(
#   set.seed=14321,
#   d20_only1, #dataset to impute
#   m = 10, # number of imputations
#   cs= c("Id"),
#   ts= c("wave"),
#   noms = c(
#     "EthnicCats_b","GenCohort"
#   ),
#   idvars=c("Wave","As"),
#   lags="Ys",
#   leads="Ys")  # correct
#
# saveRDS(imputed_20only1, here::here("_posts","mus","mods", "imputed_20only1"))
#
#
# ## 18 one
# m<-10
# imputed_18one<-NULL
#
# for(i in 1:m) {
#   imputed_18one$imputations$imp[[i]] <- imputed_18only1$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 10" & As == 1) %>%
#     arrange(Wave, Id)
# }
#
# ## 18 Zeros
# m<-10
# imputed_18zero<-NULL
#
# for(i in 1:m) {
#   imputed_18zero$imputations$imp[[i]] <- imputed_18_only0$imputations[[i]] %>% # diff
#     dplyr::filter(Wave == "Time 10" & As == 0) %>%
#     arrange(Wave, Id)
# }
#
#
#
# # Here filter only the 2019 Y^1s
# m<-10
# imputed_19one<-NULL
#
# # use 20 wave: King says leads give better estimates (Amelia documentation)
# for(i in 1:m) {
#   imputed_19one$imputations$imp[[i]] <- imputed_19only1$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 11" & As == 1) %>%
#     arrange(Wave, Id)
# }
#
# # check
#
# imputed_19one$imputations$imp[[1]]
#
# ## 19 Zeros
# m<-10
# imputed_19zero<-NULL
#
# for(i in 1:m) {
#   imputed_19zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 11" & As == 0) %>%
#     arrange(Wave, Id)
# }
#
#
#
# # Here filter only the 2020 Y^1s
# m<-10
# imputed_20one<-NULL
#
# for(i in 1:m) {
#   imputed_20one$imputations$imp[[i]] <- imputed_20only1$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 12" & As == 1) %>%
#     arrange(Wave, Id)
# }
#
# m<-10
# imputed_20zero<-NULL
#
# for(i in 1:m) {
#   imputed_20zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 12" & As == 0) %>%
#     arrange(Wave, Id)
# }
#
#
#
# # check
#
# imputed_20one$imputations$imp[[1]]
#
#
# # combine the data and arrange by wave
#
# m<-10
# imps_18 <-NULL
# for(i in 1:m) {
#   imps_18$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18zero$imputations$imp[[i]],
#                                                    imputed_18one$imputations$imp[[i]])
# }
#
# m<-10
# imps_19 <-NULL
# for(i in 1:m) {
#   imps_19$imputations$imp[[i]] <- dplyr::bind_rows(imputed_19zero$imputations$imp[[i]],
#                                                    imputed_19one$imputations$imp[[i]])
# }
#
# m<-10
# imps_20 <-NULL
# for(i in 1:m) {
#   imps_20$imputations$imp[[i]] <- dplyr::bind_rows(imputed_20zero$imputations$imp[[i]],
#                                                    imputed_20one$imputations$imp[[i]])
# }
#
#
# ## ALL 1s
#
#
# m<-10
# imps_all <-NULL
# for(i in 1:m) {
#   imps_all$imputations$imp[[i]] <- dplyr::bind_rows(
#     imputed_18zero$imputations$imp[[i]],
#     imputed_18one$imputations$imp[[i]],
#     imputed_19zero$imputations$imp[[i]],
#     imputed_19one$imputations$imp[[i]],
#     imputed_20zero$imputations$imp[[i]],
#     imputed_20one$imputations$imp[[i]]
#   ) %>%
#     arrange(Wave, Id)
# }
#
#
# # ## ALL 1s
# # # m<-10
# # # imps_trunc <-NULL
# # # for(i in 1:m) {
# # #   imps_all$imputations$imp[[i]] <- dplyr::bind_rows(
# # #     imputed_18zero$imputations$imp[[i]],
# # #     imputed_18one$imputations$imp[[i]],
# # #    # imputed_19zero$imputations$imp[[i]],
# # #     imputed_19one$imputations$imp[[i]],
# # #   #  imputed_20zero$imputations$imp[[i]],
# # #     imputed_20one$imputations$imp[[i]]
# # #   ) %>%
# # #     arrange(Wave, Id)
# # # }
# #
# # saveRDS(imps_trunc, here::here("_posts","mus","mods", "imps_all"))
# #
# # # Save data
# # #saveRDS(imps_all, here::here("_posts","mus","mods", "imps_all"))
# #
# #
# # #mod 2018
# # m<-10
# # model_18<-NULL
# # for(i in 1:m) {
# #   model_18[[i]] <- lm(Ys ~ As, data = imps_18$imputations$imp[[i]])
# #   }
# #
# # pool_parameters(model_18)
# # pred_18<- ggeffects::ggemmeans(model_18[[6]], terms = c("As"))
# # mus_plot_18 <-plot(pred_18)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# #
# # mus_plot_18
# #
# #
# # # mod 19
# # #mod 2018
# # m<-10
# # model_19<-NULL
# # for(i in 1:m) {
# #   model_19[[i]] <- lm(Ys ~ As, data = imps_19$imputations$imp[[i]])
# #   }
# #
# # pool_parameters(model_19)
# # pred_19<- ggeffects::ggemmeans(model_19[[6]], terms = c("As"))
# # mus_plot_19 <-plot(pred_19)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# #
# # mus_plot_19
# #
# # # mod 20
# # m<-10
# # model_20<-NULL
# # for(i in 1:m) {
# #   model_20[[i]] <- lm(Ys ~ As, data = imps_20$imputations$imp[[i]])
# #   }
# #
# #
# # pool_parameters(model_18)
# # pool_parameters(model_19)
# # pool_parameters(model_20)
# #
# #
# # model_20<- ggeffects::ggemmeans(model_20[[6]], terms = c("As"))
# # mus_plot_20 <-plot(model_20)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# #
# # mus_plot_18 + mus_plot_19 + mus_plot_20
#
#
# # all one
#
#
# imps_all$imputations$imp[[1]]$Wave
# # #for brms model
# dat_l<-imps_all$imputations$imp
# #saveRDS(dat_l, here::here("_posts", "mus", "mods", "dat_l"))
# # aveRDS(imps_all, here::here("_posts", "mus", "mods", "imps_all"))
#
#
# #USE
# m<-10
# model_all<-NULL
# for(i in 1:m) {
#   model_all[[i]] <- lmer(Ys ~  As * Wave + (1|Id), data = imps_all$imputations$imp[[i]])
# }
#
#
#
# # USE
# tab<-pool_parameters(model_all)
# tab
# tab [,c(1:5)]%>%
#   # print_md()%>%
#   kbl("latex",booktabs = TRUE,digits=2)
#
# plot(tab, show_labels = TRUE)
#
# pl_ml <- ggeffects::ggemmeans(model_all[[3]], terms = c("Wave","As"))
#
#
# mus_plot_model_all <-plot(pl_model_all)+ scale_y_continuous(limits=c(4.10,4.5))+labs(subtitle="Effect of attack on acceptance of Muslims") +  coord_flip()
# mus_plot_model_all
#
#
#
#
# ## Another approach a little better for writing code, identical results
# library(merTools)
# #imps_all<- readRDS(here::here("_posts", "mus", "mods","imps_all"))
#
# modList <- lmerModList(Ys ~  Wave*As + (1|Id), data =  imps_all$imputations$imp)
# fixef(modList) # model with dropped missing
#
# modelFixedEff(modList)[,c(1:5)]%>%
#   kbl("latex",booktabs = TRUE,digits=3)
#
#
#
#
# estimate_means(
#   model_all[[1]],
#   contrast = "As",
#   at = c("As","Wave"),
#   # fixed = NULL,
#   # transform = "none",
#   ci = 0.95,
#   adjust = "holm",
#   length = 2
# )
#
# estimate_contrasts( model_all[[1]],
#                     contrast = "As",
#                     at = c("Wave","As") )%>%
#   kbl("latex",booktabs = TRUE,digits=2)
#
#
#
#
#
# mus_plot_18 + mus_plot_19 + mus_plot_20
#
#
#
# # str(dat4)
# # str(dat3)
# #saveRDS(dat_l, here::here("_posts", "mus", "mods", "dat_l"))
# # # saveRDS(imps_all, here::here("_posts", "mus", "mods", "imps_all"))
# #  dat_l <- readRDS(here::here("_posts", "mus", "mods", "dat_l"))
# #
# # tscsPlot(imputed_20only1, var = "Ys", cs =c(2,4,491,564,76,582), draws = 100, conf = 0.9, misscol = "red",
# #   obscol = "black")
# #
#
# ## 2018-2020 contrast
# # m<-10
# # imps_base2020 <-NULL
# #
# # for(i in 1:m) {
# #   imps_base2020$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18b$imputations[[i]],
# #                                       imputed_18_3$imputations[[i]],
# #                                       imputed_20s_3$imputations$imp[[i]]%>%
# #                                        droplevels())
# #   imps_base2020$imputations$imp[[i]] <- imps_base2020$imputations$imp[[i]] %>% arrange(Wave,Id)
# # }
# #
# # m<-10
# # imps_d <-NULL
# # dat_d <- NULL
# #
# # for(i in 1:m) {
# #   imps_d$imputations[[i]]<- as.data.frame(imps_base2020$imputations$imp[[i]])
# # }
# #
# # dat_d<-imps_d$imputations
# # m<-10
# # models_d<-NULL
# # for(i in 1:m) {
# #   models_d[[i]] <- lmer(Ys ~ As * Wave  + (1|Id), data=imps_d$imputations[[i]])
# # }
# #
# # pool_parameters(models_d)
# # pred__base20202<- ggeffects::ggemmeans(models_d[[6]], terms = c("Wave","As"))
# # plot(pred__base20202)
# # mus_plot_base20202 <-plot(pred__base20202)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# #
# # mus_plot
#
#
#
#
#
# # df three waves ----------------------------------------------------------
#
#
# library(tidyverse)
# df_raw<-df%>%
#   dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020 ) %>%
#   droplevels() %>%
#   dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
#   group_by(Id) %>%
#   dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
#   filter(hold>0) %>% # hack!
#   ungroup(Id)%>%
#   # filter(n == 2 ) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(Attack = ifelse(TSCORE >= 3545, 1, 0)) %>%
#   ungroup()
#
# # table
# table1::table1(~ Warm.Muslims |Wave*as.factor(Attack), data = df_raw, overall=FALSE)
#
# raw<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = df_raw, overall=FALSE)
# raw
# kable(raw, format ="latex", booktabs = TRUE)
#
#
# # data frame for three waves imputation -----------------------------------
#
# km_all <- df %>%
#   dplyr::select(
#     Id,
#     Age,
#     Wave,
#     EthnicCats,
#     Employed,
#     Urban,
#     Edu,
#     Male,
#     Pol.Orient,
#     NZdep,
#     Religious,
#     GenCohort,
#     Urban,
#     TSCORE,
#     Partner,
#     Parent,
#     # Warm.Overweight,
#     # Warm.Elderly,
#     # Warm.MentalIllness,
#     Warm.Muslims,
#     # Warm.Immigrants,
#     # Warm.Asians,
#     # Warm.Refugees,
#     # Wave,
#     # Warm.Maori,
#     # Warm.NZEuro,
#     # Warm.Indians,
#     # Warm.Chinese,
#     # Warm.Refugees,
#     # Warm.Pacific,
#     YearMeasured
#   ) %>%
#   dplyr::filter(Wave == 2018 | Wave ==2019) %>%
#   droplevels() %>%
#   dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
#   group_by(Id) %>%
#   dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
#   filter(hold>0) %>% # hack to enable repeate of baseline in 2019
#   ungroup(Id) %>%
#   dplyr::mutate(Edu = as.numeric(Edu))%>%
#   arrange(Wave,Id) %>%
#   dplyr::mutate(Attack = as.numeric((ifelse(
#     (TSCORE >= 3545 & Wave == 2018)|(Wave==2019), 1, 0)))) %>% # All 2019s even if NA need to be 1
#   # dplyr::filter(Attack == 0) %>%
#   dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>%
#   dplyr::mutate(Ys = Warm.Muslims,
#                 As = Attack)%>%
#   dplyr::mutate(yrs =  (dys/365))%>%
#   dplyr::mutate(wave = as.numeric(Wave)-1) %>%
#   select(Id, Wave, Ys, As, Warm.Muslims, Attack, Age, EthnicCats, Edu, Employed, Urban, Male, GenCohort, Religious, Pol.Orient, Parent, Partner, Urban, NZdep,wave) %>%
#   group_by(Id)%>% # need to fill this way
#   dplyr::mutate(pol_bz = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
#   fill(pol_bz) %>%
#   dplyr::mutate(rel_bz = if_else(Wave == "2018", (as.numeric(Religious)), NA_real_)) %>%
#   fill(rel_bz) %>%
#   dplyr::mutate(partner_bz = if_else(Wave == "2018", (as.numeric(Partner)), NA_real_)) %>%
#   fill(partner_bz) %>%
#   dplyr::mutate(parent_bz = if_else(Wave == "2018", (as.numeric(Parent)), NA_real_)) %>%
#   fill(parent_bz) %>%
#   dplyr::mutate(age_bz = if_else(Wave == "2018", (Age), NA_real_)) %>%
#   fill(age_bz) %>%
#   dplyr::mutate(nzdep_bz = if_else(Wave == "2018", (NZdep), NA_real_)) %>%
#   fill(nzdep_bz) %>%
#   dplyr::mutate(male_2z = if_else(Wave == "2018", (as.numeric(Male))/2, NA_real_)) %>%
#   fill(male_2z) %>%
#   dplyr::mutate(employed_bz = if_else(Wave == "2018", (as.numeric(Employed)), NA_real_)) %>%
#   fill(employed_bz) %>%
#   dplyr::mutate(edu_bz = if_else(Wave == "2018", (Edu), NA_real_)) %>%
#   fill(edu_bz) %>%
#   dplyr::mutate(ubran_bz = if_else(Wave == "2018", (as.numeric(Urban)), NA_real_)) %>%
#   fill(ubran_bz) %>%
#   dplyr::mutate(EthnicCats_b = if_else(Wave == "2018", as.numeric(EthnicCats), NA_real_)) %>%
#   fill(EthnicCats_b) %>%
#   dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
#   ungroup()%>%
#   arrange(Id,Wave)
#
# # check N
# length(unique(km_all$Id))
# # correct
#
# km_all_baseline<- km_all%>%
#   filter(Wave==2018)
#
# t1<-table1::table1(~ Warm.Muslims|Wave*as.factor(Attack), data = km_all, overall=FALSE)
# kable(obtl, format ="latex", booktabs = TRUE)
#
# table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all, overall=FALSE)
#
# obtl<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all, overall=FALSE)
#
# kable(obtl, format ="latex", booktabs = TRUE)
#
#
# x <- table1::table1(~ Warm.Muslims + Age + Male + GenCohort + Edu + Employed + EthnicCats  + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban|Wave*Attack, data = km_all, overall=FALSE)
#
# t1kable(x, format ="latex")
#
#
# km_all$As
# # create new data set
# library(tidyverse)
# kf  <- km_all %>%
#   group_by(Id) %>%  # All ZEROS
#   # mutate(Attack == as.numeric(Attack))%>%
#   mutate(As = (ifelse(Wave==2018 & Attack == 1|Wave == 2019, 0,
#                       ifelse(Wave==2018 & Attack == 0, 1, Attack)))) %>%
#   mutate(Ys = ifelse(Wave==2018 & Attack == 1|Wave ==2019, NA,
#                      ifelse(Wave==2018 & Attack == 0, NA, Warm.Muslims)))%>%
#   ungroup() %>%
#   arrange(Wave,Id)
#
#
#
# # Test  = Correct
# table1::table1(~Ys|Wave *as.factor(As), data =kf, overall=F)
#
#
#
# ## Bind - double dataset to creat missing values
# ka <- km_all %>%
#   bind_rows(kf)%>%
#   arrange(Id,Wave) %>%
#   mutate(As = as.factor(As)) %>%
#   select(-c(Warm.Muslims,Attack))
#
# head(ka)
# # Check
#
# # Missing data problem
# t2<- table1::table1( ~Ys | Wave*As, data = ka, overall=F)
#
# # impute missing data
# # avoid collineraity
# ka2<-ka%>%
#   select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,age_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b)
#
# ka2<-as.data.frame(ka2)
#
#
# ### WITHOUT JOINT ASSUMPTION
# # filter only 0s and only 1s for the As
# d0 <- ka2 %>%
#   filter(As==0 )
#
# d18 <- ka2 %>%
#   filter(As==1 & Wave ==2018)
#
# # need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
# d19 <- ka2
#
#
#
# #ka3$Wave<-as.numeric(ka3$Wave)
#
# # Amelia will center and scale continuous variables during the MI process.
#
# library(Amelia)
#
# # data needs to be a data frame if passed to Amelia
# d0 <-as.data.frame(d0)
#
#
# # We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.
#
# # assume Y^0|A=2018 = Y^0 2019
#
# imputed_0<- amelia(
#   set.seed=1234,
#   d0, #dataset to impute
#   cs= c("Id"),
#   ts= c("wave"),
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b"
#   ),
#   idvars=c("Wave","As"))
#
# head(d0)
#
#
#
# # we impute all the 1s from 2018 seperately.  Y^1|A=2018 \ne Y^1 2019
# d18<-as.data.frame(d18)
#
# imputed_18<- amelia(
#   d18, #dataset to impute
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b"
#   ),
#   idvars=c("Wave","As","Id","wave")) #, # do
#
#
# # This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.
# # Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later.
#
# d19<-as.data.frame(d19)
# imputed_19<- amelia(
#   set.seed=14321,
#   d19, #dataset to impute
#   m = 10, # number of imputations
#   cs= c("Id"),
#   ts= c("wave"),
#   noms = c(
#     "EthnicCats_b"
#   ),
#   idvars=c("Wave","As")) # correct
#
#
# # Here filter only the 2019 Y^1s
#
# m<-10
# imputed_19s<-NULL
#
# for(i in 1:m) {
#   imputed_19s$imputations$imp[[i]] <- imputed_19$imputations[[i]] %>%
#     dplyr::filter(Wave == 2019 & As == 1) %>%
#     arrange(Wave, Id)
# }
#
# # check
#
# imputed_19s$imputations$imp[[1]]
#
#
# m<-10
# imps <-NULL
# for(i in 1:m) {
#   imps$imputations$imp[[i]] <- dplyr::bind_rows(imputed_0$imputations[[i]],
#                                                 imputed_18$imputations[[i]],
#                                                 imputed_19s$imputations$imp[[i]])
#   imps$imputations$imp[[i]] <- imps$imputations$imp[[i]] %>% arrange(Wave,Id)
# }
#
# #
# # m<-20
# # imps<-NULL
# # for(i in 1:m) {
# #   imps$imputations$imp[[i]] <- dplyr::bind_rows(imputed_0$imputations[[i]],
# #                                      imputed_1$imputations[[i]])
# #   imps$imputations$imp[[i]] <- imps$imputations$imp[[i]] %>% arrange(Wave,Id)
# #
# # }
#
#
#
# m<-10
# impsL <-NULL
# dat <- NULL
#
# for(i in 1:m) {
#   impsL$imputations[[i]]<- as.data.frame(imps$imputations$imp[[i]])
# }
#
# # dat <- impsL$imputations
# # saveRDS(dat, here::here("_posts", "mus", "mods", "dat"))
# # saveRDS(impsL, here::here("_posts", "mus", "mods", "impsL"))
# # saveRDS(imps, here::here("_posts", "mus", "mods", "imps"))
# impsL <- readRDS(here::here("_posts", "mus", "mods", "impsL"))
#
# tscsPlot(imputed, var = "Ys", cs =c(2,4,491,564,76,582), draws = 100, conf = 0.9, misscol = "red",
#          obscol = "black")
#
#
# m<-10
# models1<-NULL
# for(i in 1:m) {
#   models1[[i]] <- lmer(Ys ~ As * Wave  + (1|Id), data=impsL$imputations[[i]])
# }
#
# pool_parameters(models1)
# pred1<- ggeffects::ggemmeans(models1[[1], terms = c("Wave","As"))
# plot(pred1)
#
#
# ```
#
# Graph
#
#
# ```{r}
#
# #library(Amelia)
# # see: https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/, who writes:
# # unclass() is necessary because bind_rows() will complain when dealing with
# # lists with the "amelia" class, which is what amelia() returns
#
# all_imputations <- bind_rows(unclass(imputed$imputations), .id = "m") %>%
#   group_by(m) %>%
#   nest()
#
# all_imputations <- bind_rows(unclass(imps$imputations), .id = "m") %>%
#   group_by(m) %>%
#   nest()
#
# unnest(all_imputations) %>%
#   select(m, Wave, As,Ys )%>%
#   arrange((m), Wave) %>%
#   group_by(m,Wave,As) %>%
#   rename( Dataset = m,
#           Muslim_Warmth = Ys,
#           Exposure = As) %>%
#   summarise(mean = mean(Muslim_Warmth), sd = sd(Muslim_Warmth) )%>%
#   pivot_wider(names_from = c("Exposure","Wave"), values_from = c(mean,sd))%>%
#   mutate( across(where(is.numeric), round, 3))
# %>%
#   kbl("latex",booktabs =T, caption = "Imputed mean/sd for warmth to Muslims by condition and wave")
#
#
# unnest(all_imputations) %>%
#   select(m, Wave, As,Ys )%>%
#   group_by(Wave,As) %>%
#   rename( Dataset = m,
#           Muslim_Warmth = Ys,
#           Exposure = As) %>%
#   summarise(n = n(), mean = mean(Muslim_Warmth), sd = sd(Muslim_Warmth) )%>%
#   mutate( across(where(is.numeric), round, 3)) %>%
#   kbl("latex",booktabs =T, caption = "Overall imputed mean/sd for warmth to Muslims by condition and wave")
#
#
# # This doesn't work
# # all_imputations_d <- bind_rows(unclass(imputed_d$imputations), .id = "m") %>%
# #   group_by(m) %>%
# #   nest()
# #
# # unnest(all_imputations_d) %>%
# #   select(m, Wave, As,Ys )%>%
# #    group_by(Wave,As) %>%
# #   rename( Dataset = m,
# #           Muslim_Warmth = Ys,
# #           Exposure = As) %>%
# #    summarise(n = n(), mean = mean(Muslim_Warmth), sd = sd(Muslim_Warmth) )%>%
# #   mutate( across(where(is.numeric), round, 3)) %>%
# #    kbl("latex",booktabs =T, caption = "Overall imputed mean/sd for warmth to Muslims by condition and wave")
#
# unnest(all_imputations_d) %>%
#   select(m, Wave, As,Ys )%>%
#   arrange((m), Wave) %>%
#   group_by(m,Wave,As) %>%
#   rename( Dataset = m,
#           Muslim_Warmth = Ys,
#           Exposure = As) %>%
#   summarise(mean = mean(Muslim_Warmth), sd = sd(Muslim_Warmth) )%>%
#   pivot_wider(names_from = c("Exposure","Wave"), values_from = c(mean,sd))%>%
#   mutate( across(where(is.numeric), round, 3))
# ```
#
# ## FULL BAYES
#
#
#
# ```{r}
# dat <-readRDS(here::here("_posts","mus","mods","dat"))
#
# ## Do not save residuals
# # m_bayes <- brms::brm(
# #   Ys ~ As *  Wave + (1|Id),
# #   data = dat,
# #   seed = 1234,
# #   warmup = 1000,
# #   iter = 2000,
# #   chains = 4,
# #  # backend = "cmdstanr",
# #   save_pars=save_pars(group=FALSE))
# #   file = here::here("_posts", "mus", "mods", "m_bayes.rds"))
#
#
# m_bayesR <- brms::brm(
#   Ys ~ As *  Wave + (1|Id),
#   data = dat,
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_bayesR.rds"))
#
# lazerhawk::brms_SummaryTable(m_bayesR)
# prior_summary(m_bayesR)
#
# # Print Table
# ouput <- tidy(m_us)
# str(ouput)
# ouput
# #output<- as.data.frame(ouput)
#
#
# library(lazerhawk)
# blh_tab <- brms_SummaryTable(m_us, panderize=F)
# blh_tab
# blh_tab %>%
#   kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
#   print()
#
#
# # smaller file
# save_pars=save_pars(group=FALSE)
#
#
# ## prior = c(prior(normal(0, 1), class = b)),
#
# #amelia.list<-readRDS(here::here("_posts","mus", "mods", "amelia.list"))
#
# ```
#
#
# ### Distributional model
#
# ```{r}
# m_dis <- brms::brm(
#   bf(Ys ~ As  *  Wave + (1|Id),
#      sigma ~ As * Wave + (1|Id)),
#   data = dat,
#   prior = c(prior(lognormal(0, 0.25), class = b, coef = "Wave")), # constrain wave to be positive
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_dis"))
# ```
#
#
# ### Graphs
#
# ```{r}
# #### GRAPH
# library(tidybayes)
# library(emmeans)
# marg_eff_attack_0a <- m_bayesR %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("2018", "2019")),
#               re_formula = NA)
#
# #saveRDS(marg_eff_attack_0a,  here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))
# #marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))
#
# #marg_eff_attack_0a
# # Obtain contrasts
# # marg_eff_attack_0a<- marg_eff_attack_0a%>%
# #   filter(As == 1 & Wave ==2019) #Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)
#
# scale_y_discrete(limits=rev)
#
#
# # marg_eff_attack_0
# plot_all <- ggplot(
#   marg_eff_attack_0a,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) +
#   #  scale_y_discrete(limits=rev) +
#   stat_halfeye() +
#   scale_fill_okabe_ito() +
#   labs(
#     x = "Predicted Warmth Response",
#     #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#     fill = "Conterfactual Contrasts",
#     subtitle = "Bayesian posterior locations of potential outcomes"
#   ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#   theme_pubclean() +
#   theme(legend.position = "bottom"
#   )
# plot_all
#
# ggsave(
#   plot_all,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 8,
#   height = 8,
#   units = "in",
#   filename = "plot_all.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )
#
# #plot_all
#
# #
# # #
# # grand_mean_ame_a <- m_bayesR %>%
# #   emmeans(~ As*Wave,
# #           epred = TRUE, re_formula = NA) %>%
# #   contrast() %>%
# #   gather_emmeans_draws()
# #
# # grand_mean_ame_a
# # #   filter(As == 1 & Wave ==2019) #Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)
# #
# # saveRDS(grand_mean_ame_a,  here::here("_posts", "mus", "mods", "grand_mean_ame_a.rds"))
# # #grand_mean_ame_a <- readRDS(here::here("_posts", "mus", "mods", "grand_mean_ame_a.rds"))
# #
# # # remove erro contrasts / we don't observe 0 in year two
# #
#
# # does not
#
# # plot_all__ame <- ggplot(grand_mean_ame_a,
# #                                      aes(x = .value)) +
# #   stat_halfeye(slab_alpha = 0.75, fill = "grey") +
# #   scale_fill_okabe_ito(order = c(3, 4)) +
# #   labs(x = "Average marginal effect of attack on warmth to Muslims",
# #        y = "Density") +
# #   #facet_wrap(vars(region)) +
# #   theme_classic() +
# #   theme(legend.position = "bottom") + labs(title= "Marginal effect of attack on warmth to Muslims")
# # plot_all__ame
# # Try
# library(patchwork)
# bayes_plots_a <- plot_all + plot_all__ame + plot_annotation(tag_levels = 'a')
# bayes_plots_a
#
# # ggsave(
# #   bayes_plots_a,
# #   path = here::here(here::here("_posts", "mus", "figs")),
# #   width = 10,
# #   height = 5,
# #   units = "in",
# #   filename = "bayes_plots_a.jpg",
# #   device = 'jpeg',
# #   limitsize = FALSE,
# #   dpi = 1200
# # )
#
#
# #
# # blh_tab %>%
# #    kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
# #     print()
# ```
#
# ## MI Bayes
#
# ```{r}
# ## Run on my other mac, cmdstanr was ausing problems
# gc()
#
# plot_bayes <- m_us %>%
#   emmeans( ~ As * Wave) %>%
#   contrast(method = "pairwise") %>%
#   gather_emmeans_draws() %>%
#   ggplot(aes(x = .value, y = contrast)) +
#   stat_halfeye()
#
#
#
# marg_eff_attack_0ab <- m_us %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1),Wave = c("2018", "2019")),
#               re_formula =NA)
#
# saveRDS(marg_eff_attack_0ab,  here::here("_posts", "mus", "mods", "marg_eff_attack_0ab.rds"))
# #marg_eff_attack_0ab <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0ab.rds"))
#
#
# # Obtain contrasts
#
# marg_eff_attack_0ab
# plot_all <- ggplot(
#   marg_eff_attack_0ab,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) +
#   #  scale_y_discrete(limits=rev) +
#   stat_halfeye() +
#   scale_fill_okabe_ito() +
#   labs(
#     x = "Predicted Warmth Response",
#     y = "w2018(years 2018-2019); w2019 (years 2019-2020)",
#     fill = "Conterfactual Contrasts",
#     subtitle = "Bayesian posterior locations of potential outcomes"
#   ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#   theme_pubclean() +
#   theme(legend.position = "bottom"
#   )
# plot_all
# #plot_all
#
#
# grand_mean_ame_ab <- m_us %>%
#   emmeans(~ As*Wave,
#           epred = TRUE, re_formula = NA) %>%
#   gather_emmeans_draws()
#
# grand_mean_ame_ab
# grand_mean_ame_ab
# saveRDS(grand_mean_ame_ab,  here::here("_posts", "mus", "mods", "grand_mean_ame_ab.rds"))
# grand_mean_ame_ab <- readRDS(here::here("_posts", "mus", "mods", "grand_mean_ame_ab.rds"))
# grand_mean_ame_ab%>%
#   slice(10000)
# # remove erro contrasts / we don't observe 0 in year two
#
#
# plot_all__ame_ab <- ggplot(grand_mean_ame_ab,
#                            aes(x = .value)) +
#   stat_halfeye(slab_alpha = 0.75, fill = "grey") +
#   scale_fill_okabe_ito(order = c(3, 4)) +
#   labs(x = "Average marginal effect of attack on warmth to Muslims",
#        y = "Density") +
#   #facet_wrap(vars(region)) +
#   theme_classic() +
#   theme(legend.position = "bottom") + labs(title= "Marginal effect of attack on warmth to Muslims")
# plot_all__ame_ab
# library(patchwork)
# bayes_plots_a <- plot_all + plot_all__ame + plot_annotation(tag_levels = 'a')
# bayes_plots_a
#
# ggsave(
#   bayes_plots_a,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 10,
#   height = 5,
#   units = "in",
#   filename = "bayes_plots_a.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )
#
# ```
#
#
#
#
# ## Weights graph
#
# ```{r}
#
# # set up data
# kn <- km_all
# km_all$Attack
# kn$attack <- km_all$Attack
#
#
# # set up of data
# imputed1<-imps$imputations$imp[[1]]
# im$attack <- as.numeric(im$As)-1
# im$warmth <- as.numeric(im$Ys)
# kn$warmth <- as.numeric(kn$Warm.Muslims)
# im$attack
#
#
# # check
# kn%>%
#   filter(Wave ==2019 & As ==1) %>%
#   select(Warm.Muslims) %>%
#   count(is.na(.))
#
#
# # kna<-km_all%>%filter(Wave==2019)
# # hist(kna$Warm.Muslims)
#
# plot1 <- ggplot() +
#   geom_histogram(data = filter(im, attack == 1 & Wave == 2018),
#                  #  bins = 10,
#                  aes(x = warmth),
#                  fill = colorspace::lighten("deepskyblue4", 0.55)) +
#   geom_histogram(data = filter(im, attack == 0 & Wave == 2018),
#                  # bins = 10,
#                  aes(x = warmth, y = -..count..),
#                  fill = colorspace::lighten("chocolate", 0.55))  +
#   geom_histogram(data = filter(kn, attack == 1& Wave == 2018),
#                  #bins = 10,
#                  aes(x = warmth),
#                  fill = colorspace::lighten("deepskyblue4", 0.1))+
#   geom_histogram(data = filter(kn, attack == 0 & Wave == 2018),
#                  # bins = 10,
#                  aes(x = warmth, y = -..count..),
#                  fill = colorspace::lighten("chocolate", 0.1)) +
#   annotate(geom = "label", x = 1.5, y = 3000, label = "Exposed\nobserved",
#            fill = colorspace::lighten("deepskyblue4",.1), color = "white", hjust = 1) +
#   annotate(geom = "label", x = 3.5, y = 7000, label = "Exposed\nimputed",
#            fill = colorspace::lighten("deepskyblue4",.35), color = "white", hjust = 1)+
#   annotate(geom = "label", x = 1.5, y = -5000, label = "Not Exposed\nobserved",
#            fill = colorspace::lighten("chocolate", 0.1), color = "white", hjust = 1) +
#   annotate(geom = "label", x = 3.5, y = -10000, label = "Not Exposed\nimputed",
#            fill = colorspace::lighten("chocolate",  .35), color = "white", hjust = 1) +
#   geom_hline(yintercept = 0, color = "white", size = 0.25) +
#   scale_y_continuous(label = abs) +
#   scale_y_continuous(limits=c(-20000,15000)) +
#   # coord_cartesian(xlim = c(0.1, 1)) +
#   labs(x = "Warmth to Muslim", y = "Counts") + labs(title= "Time 10")
#
#
#
#
# plot1
#
# length(unique(kn$Id))
# length(unique(imputed1$Id))
#
# hist(kn$Warm.Muslims)
#
# plot2 <- ggplot() +
#   geom_histogram(data = filter(im, attack == 1 & Wave == 2019),
#                  # bins = 10,
#                  aes(x = warmth),
#                  fill = colorspace::lighten("deepskyblue4", 0.55)) +
#   geom_histogram(data = filter(im, attack == 0 & Wave == 2019),
#                  # bins = 10,
#                  aes(x = warmth, y = -..count..),
#                  fill = colorspace::lighten("chocolate", 0.55))  +
#   geom_histogram(data = filter(kn, attack == 1 & Wave == 2019),
#                  #bins = 10,
#                  aes(x = warmth),
#                  fill = colorspace::lighten("deepskyblue4", 0.1))+
#   geom_histogram(data = filter(kn, attack == 0 & Wave == 2019),
#                  #  bins = 10,
#                  aes(x = warmth, y = -..count..),
#                  fill = colorspace::lighten("chocolate", 0.1)) +
#   annotate(geom = "label", x = 2.5, y = 5000, label = "Exposed\nobserved",
#            fill = colorspace::lighten("deepskyblue4",.1), color = "white", hjust = 1) +
#   annotate(geom = "label", x = 3.5, y = 14000, label = "Exposed\nimputed",
#            fill = colorspace::lighten("deepskyblue4",.35), color = "white", hjust = 1)+
#   annotate(geom = "label", x = 2.5, y = -500, label = "Not Exposed is\nfully imputed",
#            fill = colorspace::lighten("chocolate",  .35), color = "white", hjust = 1) +
#   geom_hline(yintercept = 0, color = "white", size = 0.25) +
#   scale_y_continuous(label = abs) +
#   # coord_cartesian(xlim = c(0.1, 1)) +
#   labs(x = "Warmth to Muslim", y = "Counts") + labs(title= "Time 11") +
#   scale_y_continuous(limits=c(-20000,15000))
# plot2
# imp_frq<- plot1 + plot2 + plot_annotation(tag_levels = "a", title = "Comparison of multiply-imputed responses and observed\nresponse distributions in Wave 10 and Wave 11")
#
#
# imp_frq
#
#
#
# ggsave(
#   imp_frq,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "imp_frq.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )
#
#
#
# model_treatment_freq <- glm(attack ~    Male  + Edu + EthnicCats + Religious + Pol.Orient + Urban + NZdep + Religious,  data = kn,family = binomial(link = "logit"))
#
#
# # Step 2: Use the treatment model to calculate propensity scores, and
# # Step 3: Use the propensity scores to calculate inverse probability of treatment weights
# km_with_weights <- augment(model_treatment_freq, kn,
#                            type.predict = "response") %>%
#   rename(propensity = .fitted) %>%
#   mutate(iptw = (attack / propensity) + ((1 - attack) / (1 - propensity)))
#
#
# max(km_with_weights$iptw)
#
#
# # Step 4: Use the IPTWs in a model that estimates the effect of treatment on outcome
# model_outcome_freq <- lm(Warm.Muslims ~ Attack   ,
#                          data = km_with_weights,
#                          weights = iptw)
#
# # Coefficient
# tidy(model_outcome_freq)
#
#
#
#
# library(MetBrewer)
# # ggplot() +
# #   geom_histogram(data = filter(km, Attack == 1),
# #                  bins = 50, aes(x = ipw),
# #                  fill = colorspace::lighten("darkorange", 0.5)) +
# #    geom_histogram(data = filter(km, Attack == 0),
# #                  bins = 50, aes(x = ipw, y = -..count..),
# #                  fill = colorspace::lighten("darkgreen", 0.5)) +
# #   geom_hline(yintercept = 0) +
# #   annotate(geom = "label", x = 0.8, y = 2000, label = "Treated",
# #            fill = colorspace::lighten("darkorange", 0.2), color = "white", hjust = 0) +
# #   annotate(geom = "label", x = 0.8, y = -2000, label = "Untreated",
# #            fill = colorspace::lighten("darkgreen", 0.2), color = "white", hjust = 0) +
# #   scale_y_continuous(label = abs) +
# #  #coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-10000, 10000)) +
# #   labs(x = "Propensity", y = "Count")
#
#
# #
# imputed
# ipwplot <- ggplot() +
#   geom_histogram(data = filter(km_with_weights, attack == 1),
#                  bins = 50, aes(x = propensity, weight = iptw),
#                  fill = colorspace::lighten("chocolate", 0.55)) +
#   geom_histogram(data = filter(km_with_weights, attack == 0),
#                  bins = 50, aes(x = propensity, weight = iptw, y = -..count..),
#                  fill = colorspace::lighten("deepskyblue4", 0.55)) +
#   geom_histogram(data = filter(km_with_weights, attack == 1),
#                  bins = 50, aes(x = propensity),
#                  fill = colorspace::lighten("chocolate", 0.1)) +
#   geom_histogram(data = filter(km_with_weights, attack == 0),
#                  bins = 50, aes(x = propensity, y = -..count..),
#                  fill = colorspace::lighten("deepskyblue4", 0.1)) +
#   annotate(geom = "label", x = 0.7, y = 1000, label = "Treated (actual)",
#            fill = colorspace::lighten("chocolate",.1), color = "white", hjust = 1) +
#   annotate(geom = "label", x = 0.7, y = 3000, label = "Treated (imputed\npseudo-population)",
#            fill = colorspace::lighten("chocolate",.35), color = "white", hjust = .5)+
#   annotate(geom = "label", x = 0.7, y = -1000, label = "Untreated (actual)",
#            fill = colorspace::lighten("deepskyblue4", 0.1), color = "white", hjust = 1) +
#   annotate(geom = "label", x = 0.7, y = -3000, label = "Untreated (imputed\npseudo-population)",
#            fill = colorspace::lighten("deepskyblue4",  .35), color = "white", hjust = 1) +
#   geom_hline(yintercept = 0, color = "white", size = 0.25) +
#   scale_y_continuous(label = abs) +
#   # coord_cartesian(xlim = c(0.1, 1)) +
#   labs(x = "Propensity", y = "Count") + labs(title= "Comparison of propensity score\nand original response distributions")
# ipwplot
#
#
#
# model_treatment_freq <- glm(attack ~
#                               Male  + Edu + EthnicCats + Religious + Pol.Orient + Urban + NZdep + Religious,
#                             data = kp18,family = binomial(link = "logit"))
#
#
# # Step 2: Use the treatment model to calculate propensity scores, and
# # Step 3: Use the propensity scores to calculate inverse probability of treatment weights
# km_with_weights <- augment(model_treatment_freq, kp18,
#                            type.predict = "response") %>%
#   rename(propensity = .fitted) %>%
#   mutate(iptw = (attack / propensity) + ((1 - attack) / (1 - propensity)))
#
#
# max(km_with_weights$iptw)
#
#
# # Step 4: Use the IPTWs in a model that estimates the effect of treatment on outcome
# model_outcome_freq <- lm(Warm.Muslims ~ Attack ,
#                          data = km_with_weights,
#                          weights = iptw)
#
# # Coefficient
# tidy(model_outcome_freq)
#
#
#
#
# library(MetBrewer)
# # ggplot() +
# #   geom_histogram(data = filter(km, Attack == 1),
# #                  bins = 50, aes(x = ipw),
# #                  fill = colorspace::lighten("darkorange", 0.5)) +
# #    geom_histogram(data = filter(km, Attack == 0),
# #                  bins = 50, aes(x = ipw, y = -..count..),
# #                  fill = colorspace::lighten("darkgreen", 0.5)) +
# #   geom_hline(yintercept = 0) +
# #   annotate(geom = "label", x = 0.8, y = 2000, label = "Treated",
# #            fill = colorspace::lighten("darkorange", 0.2), color = "white", hjust = 0) +
# #   annotate(geom = "label", x = 0.8, y = -2000, label = "Untreated",
# #            fill = colorspace::lighten("darkgreen", 0.2), color = "white", hjust = 0) +
# #   scale_y_continuous(label = abs) +
# #  #coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-10000, 10000)) +
# #   labs(x = "Propensity", y = "Count")
#
#
# #
# imputed
# ipwplot <- ggplot() +
#   geom_histogram(data = filter(km_with_weights, attack == 1),
#                  bins = 50, aes(x = propensity, weight = iptw),
#                  fill = colorspace::lighten("chocolate", 0.55)) +
#   geom_histogram(data = filter(km_with_weights, attack == 0),
#                  bins = 50, aes(x = propensity, weight = iptw, y = -..count..),
#                  fill = colorspace::lighten("deepskyblue4", 0.55)) +
#   geom_histogram(data = filter(km_with_weights, attack == 1),
#                  bins = 50, aes(x = propensity),
#                  fill = colorspace::lighten("chocolate", 0.1)) +
#   geom_histogram(data = filter(km_with_weights, attack == 0),
#                  bins = 50, aes(x = propensity, y = -..count..),
#                  fill = colorspace::lighten("deepskyblue4", 0.1)) +
#   # annotate(geom = "label", x = 0.7, y = 1000, label = "Treated (actual)",
#   #         fill = colorspace::lighten("chocolate",.1), color = "white", hjust = 1) +
#   # annotate(geom = "label", x = 0.7, y = 3000, label = "Treated (imputed\npseudo-population)",
#   #           fill = colorspace::lighten("chocolate",.35), color = "white", hjust = .5)+
#   # annotate(geom = "label", x = 0.7, y = -1000, label = "Untreated (actual)",
#   #         fill = colorspace::lighten("deepskyblue4", 0.1), color = "white", hjust = 1) +
#   # annotate(geom = "label", x = 0.7, y = -3000, label = "Untreated (imputed\npseudo-population)",
#   #          fill = colorspace::lighten("deepskyblue4",  .35), color = "white", hjust = 1) +
#   # geom_hline(yintercept = 0, color = "white", size = 0.25) +
#   # scale_y_continuous(label = abs) +
#   # coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-80, 100)) +
# labs(x = "Propensity", y = "Count") + labs(title= "Comparison of propensity score\nand original response distributions")
# ipwplot
#
#
#
#
#
# model_treatment_freq <- glm(attack ~
#                               Male  + Edu + EthnicCats + Religious + Pol.Orient + Urban + NZdep + Religious,
#                             data = kp19,family = binomial(link = "logit"))
#
#
# # Step 2: Use the treatment model to calculate propensity scores, and
# # Step 3: Use the propensity scores to calculate inverse probability of treatment weights
# km_with_weights <- augment(model_treatment_freq, kp19,
#                            type.predict = "response") %>%
#   rename(propensity = .fitted) %>%
#   mutate(iptw = (attack / propensity) + ((1 - attack) / (1 - propensity)))
#
#
# min(km_with_weights$iptw)
#
#
# # Step 4: Use the IPTWs in a model that estimates the effect of treatment on outcome
# model_outcome_freq <- lm(Warm.Muslims ~ Attack ,
#                          data = km_with_weights,
#                          weights = iptw)
#
# # Coefficient
# tidy(model_outcome_freq)
#
#
#
#
#
# ind <- imputed1
# ind$propensity<- rep(1,nrow(ind))
# ind$iptw<- rep(1,nrow(ind))
# ind$attack <- ind$As
#
# ipwplot <- ggplot() +
#   geom_histogram(data = filter(ind, attack == 1),
#                  bins = 50, aes(x = propensity, weight = iptw),
#                  fill = colorspace::lighten("chocolate", 0.55)) +
#   geom_histogram(data = filter(ind, attack == 0),
#                  bins = 50, aes(x = propensity, weight = iptw, y = -..count..),
#                  fill = colorspace::lighten("deepskyblue4", 0.55)) +
#   geom_histogram(data = filter(km_with_weights, attack == 1),
#                  bins = 50, aes(x = propensity),
#                  fill = colorspace::lighten("chocolate", 0.1)) +
#   geom_histogram(data = filter(km_with_weights, attack == 0),
#                  bins = 50, aes(x = propensity, y = -..count..),
#                  fill = colorspace::lighten("deepskyblue4", 0.1)) +
#   #  annotate(geom = "label", x = 0.7, y = 1000, label = "Treated (actual)",
#   #          fill = colorspace::lighten("chocolate",.1), color = "white", hjust = 1) +
#   #  annotate(geom = "label", x = 0.7, y = 3000, label = "Treated (IPTW\npseudo-population)",
#   #            fill = colorspace::lighten("chocolate",.35), color = "white", hjust = 1)+
#   #  annotate(geom = "label", x = 0.7, y = -1000, label = "Untreated (actual)",
#   #          fill = colorspace::lighten("deepskyblue4", 0.1), color = "white", hjust = 1) +
#   #  annotate(geom = "label", x = 0.7, y = -3000, label = "Untreated (IPTW\npseudo-population)",
#   #           fill = colorspace::lighten("deepskyblue4",  .35), color = "white", hjust = 1) +
#   #  geom_hline(yintercept = 0, color = "white", size = 0.25) +
#   #  scale_y_continuous(label = abs) +
#   # # coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-80, 100)) +
# labs(x = "Propensity", y = "Count") + labs(title= "Comparison of propensity score\nand original response distributions")#+ scale_y_continuous(limits=c(-15000,15000))
# ipwplot
#
#
#
# ggsave(
#   ipwplot,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 10,
#   height = 5,
#   units = "in",
#   filename = "ipweightplot.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )
#
#
# ```
#
#
# ## 3 waves approach 1
#
# ```{r}
#
# # select variables
# library(tidyverse)
# scale_y_discrete(limits=rev)
# # restrict to only thise who compleated all response
# df_raw<- df%>%
#   dplyr::filter( Wave == 2018 & YearMeasured ==1 | Wave == 2019 & YearMeasured ==1 | Wave == 2020 & YearMeasured ==1) %>%
#   droplevels() %>%
#   dplyr::select(
#     Id,
#     Age,
#     Wave,
#     EthnicCats,
#     Employed,
#     Urban,
#     Edu,
#     Male,
#     Pol.Orient,
#     NZdep,
#     Religious,
#     GenCohort,
#     Urban,
#     TSCORE,
#     Partner,
#     Parent,
#     # Warm.Overweight,
#     # Warm.Elderly,
#     # Warm.MentalIllness,
#     Warm.Muslims,
#     # Warm.Immigrants,
#     # Warm.Asians,
#     # Warm.Refugees,
#     # Wave,
#     # Warm.Maori,
#     # Warm.NZEuro,
#     # Warm.Indians,
#     # Warm.Chinese,
#     # Warm.Refugees,
#     # Warm.Pacific,
#     YearMeasured
#   ) %>%
#   group_by(Id)%>%
#   filter(n() == 3) %>%
#   dplyr::add_tally() %>%
#   filter(n == 3 ) %>%3
# dplyr::ungroup() %>%
#   dplyr::mutate(Attack = ifelse(TSCORE >= 3545, 1, 0)) %>%
#   ungroup()%>%
#   drop_na()
#
# t13<-table1::table1(~ Warm.Muslims|Wave*as.factor(Attack), data = df_raw, overall=FALSE)
# t13
# kable(t13, format ="latex", booktabs = TRUE)
#
# library(tidyverse)
# km_all3 <- df %>%
#   dplyr::select(
#     Id,
#     Age,
#     Wave,
#     EthnicCats,
#     Employed,
#     Urban,
#     Edu,
#     Male,
#     Pol.Orient,
#     NZdep,
#     Religious,
#     GenCohort,
#     Urban,
#     TSCORE,
#     Partner,
#     Parent,
#     # Warm.Overweight,
#     # Warm.Elderly,
#     # Warm.MentalIllness,
#     Warm.Muslims,
#     # Warm.Immigrants,
#     # Warm.Asians,
#     # Warm.Refugees,
#     # Wave,
#     # Warm.Maori,
#     # Warm.NZEuro,
#     # Warm.Indians,
#     # Warm.Chinese,
#     # Warm.Refugees,
#     # Warm.Pacific,
#     YearMeasured
#   ) %>%
#   dplyr::filter(Wave == 2018 | Wave ==2019 | Wave ==2020) %>%
#   droplevels() %>%
#   dplyr::mutate(org2018 =  ifelse(Wave == 2018 & YearMeasured ==1,1,0 ))%>%
#   group_by(Id) %>%
#   dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
#   filter(hold>0) %>% # hack to enable repeate of baseline in 2019
#   ungroup(Id) %>%
#   dplyr::mutate(Edu = as.numeric(Edu))%>%
#   arrange(Wave,Id) %>%
#   dplyr::mutate(Attack = as.numeric((ifelse(
#     (TSCORE >= 3545 & Wave == 2018)|(Wave==2019|Wave ==2020), 1, 0)))) %>% # All 2019s even if NA need to be 1
#   # dplyr::filter(Attack == 0) %>%
#   group_by(Id) %>%
#   dplyr::mutate(dys = (TSCORE - min(TSCORE)))%>%
#   dplyr::mutate(Ys = Warm.Muslims,
#                 As = Attack)%>%
#   dplyr::mutate(yrs =  (dys/365))%>%
#   dplyr::mutate(wave = as.numeric(Wave)-1) %>%
#   group_by(Id)%>% # need to fill this way
#   dplyr::mutate(pol_bz = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
#   fill(pol_bz) %>%
#   dplyr::mutate(rel_bz = if_else(Wave == "2018", (as.numeric(Religious)), NA_real_)) %>%
#   fill(rel_bz) %>%
#   dplyr::mutate(partner_bz = if_else(Wave == "2018", (as.numeric(Partner)), NA_real_)) %>%
#   fill(partner_bz) %>%
#   dplyr::mutate(parent_bz = if_else(Wave == "2018", (as.numeric(Parent)), NA_real_)) %>%
#   fill(parent_bz) %>%
#   dplyr::mutate(age_bz = if_else(Wave == "2018", (Age), NA_real_)) %>%
#   fill(age_bz) %>%
#   dplyr::mutate(nzdep_bz = if_else(Wave == "2018", (NZdep), NA_real_)) %>%
#   fill(nzdep_bz) %>%
#   dplyr::mutate(male_2z = if_else(Wave == "2018", (as.numeric(Male))/2, NA_real_)) %>%
#   fill(male_2z) %>%
#   dplyr::mutate(employed_bz = if_else(Wave == "2018", (as.numeric(Employed)), NA_real_)) %>%
#   fill(employed_bz) %>%
#   dplyr::mutate(edu_bz = if_else(Wave == "2018", (Edu), NA_real_)) %>%
#   fill(edu_bz) %>%
#   dplyr::mutate(ubran_bz = if_else(Wave == "2018", (as.numeric(Urban)), NA_real_)) %>%
#   fill(ubran_bz) %>%
#   dplyr::mutate(EthnicCats_b = if_else(Wave == "2018", as.numeric(EthnicCats), NA_real_)) %>%
#   fill(EthnicCats_b) %>%
#   dplyr::mutate(EthnicCats_b, as.factor(EthnicCats_b)) %>%
#   ungroup()%>%
#   arrange(Id,Wave)
# km_all3
#
#
# levels(km_all3$Wave) <- c("Time 10", "Time 11","Time 12")
# # check N
# km_all3
# length(unique(km_all3$Id))
# # correct
# km_all3
# t13<-table1::table1(~ Warm.Muslims|Wave * as.factor(Attack), data = km_all3, overall=FALSE)
# t13
# kable(t13, format ="latex", booktabs = TRUE)
#
# #modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")
#
# table1::table1(~Warm.Muslims|Wave*as.factor(Attack), data = km_all3, overall=FALSE)
#
# obtl<-table1::table1(~ Warm.Muslims|as.factor(Attack)*Wave, data = km_all3, overall=FALSE)
#
# kable(obtl, format ="latex", booktabs = TRUE)
#
# km_2018 <- km_all3 %>%
#   dplyr::filter(Wave =="Time 10")%>%
#   droplevels()
#
# x <- table1::table1(~  Age +Edu + Employed + EthnicCats  + Male + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban|Wave, data = km_all3, overall = FALSE)
# x
#
# t1kable(x, format ="latex")
#
#
# km_all3$As
# # create new data set
# library(tidyverse)
# kf3  <- km_all3 %>%
#   group_by(Id) %>%  # All ZEROS
#   # mutate(Attack == as.numeric(Attack))%>%
#   mutate(As = (ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", 0,
#                       ifelse(Wave=="Time 10" & Attack == 0, 1, Attack)))) %>%
#   mutate(Ys = ifelse(Wave=="Time 10" & Attack == 1|Wave == "Time 11"|Wave =="Time 12", NA,
#                      ifelse(Wave=="Time 10" & Attack == 0, NA, Warm.Muslims)))%>%
#   ungroup() %>%
#   arrange(Wave,Id)
#
#
#
# # Test NAs = Correct
# table1::table1(~Ys|Wave *as.factor(As), data =kf3, overall=F)
#
#
#
# ## Bind - double dataset to creat missing values
# ka3 <- km_all3%>%
#   bind_rows(kf3)%>%
#   arrange(Id,Wave) %>%
#   mutate(As = as.factor(As)) #%>%
# # select(-c(Warm.Muslims,Attack))
#
# head(ka3$Ys)
# # Check
#
# # Missing data problem
# t2<- table1::table1( ~Ys | Wave*As, data = ka3, overall=F)
# t2
#
# t1kable(t2, format ="latex")
# # impute missing data
# # avoid collineraity
#
# ka2_selected <-ka3%>%
#   dplyr::select(Id,Wave,wave,As,Ys,pol_bz,rel_bz,partner_bz,parent_bz,age_bz,nzdep_bz,male_2z,employed_bz,edu_bz,ubran_bz,EthnicCats_b)
#
# ka_amelia<-as.data.frame(ka2_selected)
#
#
# ### WITHOUT JOINT ASSUMPTION
# # filter only 0s and only 1s for the As
# d0 <- ka_amelia %>%
#   filter(As==0 )
#
# d18_only0 <- ka_amelia %>%
#   filter(As==0 & Wave == "Time 10")
#
#
# d18_only1 <- ka_amelia %>%
#   filter(As==1 & Wave =="Time 10")
#
# # need full distribution to obtain correct marginal for 19 = 1 (because we have 2018=0, and 2018=1)
# d19_only1 <- ka_amelia %>%
#   filter(Wave =="Time 10" | (As==1 & Wave =="Time 11" ))
#
# d20_only1 <- ka_amelia %>%
#   filter(Wave =="Time 10"|(As==1 & Wave =="Time 11" )|(As==1 & Wave =="Time 12" ))
#
#
# library(Amelia)
#
# # data needs to be a data frame if passed to Amelia
# d0 <-as.data.frame(d0)
#
#
# # We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.
# # assume Y^0|A=2018 = Y^0 2019
#
# imputed_0<- amelia(
#   set.seed=1234,
#   d0, #dataset to impute
#   # cs= c("Id"),
#   #ts= c("wave"),
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b"
#   ),
#   idvars=c("Wave","As", "Id","wave"))
#
# saveRDS(imputed_0, here::here("_posts","mus","mods", "imputed_0"))
#
#
# imputed_18_only0<- amelia(
#   set.seed=1234,
#   d18_only0, #dataset to impute
#   # cs= c("Id"),
#   #ts= c("wave"),
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b"
#   ),
#   idvars=c("Wave","As", "Id","wave"))
#
# saveRDS(imputed_18_only0, here::here("_posts","mus","mods", "imputed_18_only0"))
#
#
# d18_only1<-data.frame(d18_only1)
#
# ## For 2018 v 2020 contrast
# imputed_18only1<- amelia(
#   set.seed=1234,
#   d18_only1, #dataset to impute
#   # cs= c("Id"),
#   # ts= c("wave"),
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b"
#   ),
#   idvars=c("As", "Wave", "Id","wave"))
#
# saveRDS(imputed_18only1, here::here("_posts","mus","mods", "imputed_18only1"))
#
#
# # we impute all the 1s from 2018 seperately.  Y^1|A=2018 \ne Y^1 2019
# d19_only1<-as.data.frame(d19_only1)
#
#
# # This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.
# # Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later.
#
# imputed_19only1<- amelia(
#   d19_only1, #dataset to impute
#   cs= c("Id"),
#   ts= c("wave"),
#   m = 10, # number of imputations
#   noms = c(
#     "EthnicCats_b"
#   ),
#   idvars=c("As", "Wave"),
#   lags="Ys",
#   leads="Ys")
#
# saveRDS(imputed_19only1, here::here("_posts","mus","mods", "imputed_19only1"))
#
#
# # This is tricky. We impute all the 1s from 2019 with the 1s from the joint distribution of 2018 & 2018 seperately.
# # Y^1|A=2018,A=2019 \ne Y^1 2018.  We will need to filter the 2019 Ys later.
#
# d20_only1<-as.data.frame(d20_only1)
# imputed_20only1 <- amelia(
#   set.seed=14321,
#   d20_only1, #dataset to impute
#   m = 10, # number of imputations
#   cs= c("Id"),
#   ts= c("wave"),
#   noms = c(
#     "EthnicCats_b"
#   ),
#   idvars=c("Wave","As"),
#   lags="Ys",
#   leads="Ys")  # correct
#
# saveRDS(imputed_20only1, here::here("_posts","mus","mods", "imputed_20only1"))
#
#
# ## 18 one
# m<-10
# imputed_18one<-NULL
#
# for(i in 1:m) {
#   imputed_18one$imputations$imp[[i]] <- imputed_18only1$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 10" & As == 1) %>%
#     arrange(Wave, Id)
# }
#
# ## 18 Zeros
# m<-10
# imputed_18zero<-NULL
#
# for(i in 1:m) {
#   imputed_18zero$imputations$imp[[i]] <- imputed_18_only0$imputations[[i]] %>% # diff
#     dplyr::filter(Wave == "Time 10" & As == 0) %>%
#     arrange(Wave, Id)
# }
#
#
#
# # Here filter only the 2019 Y^1s
# m<-10
# imputed_19one<-NULL
#
# # use 20 wave: King says leads give better estimates (Amelia documentation)
# for(i in 1:m) {
#   imputed_19one$imputations$imp[[i]] <- imputed_19only1$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 11" & As == 1) %>%
#     arrange(Wave, Id)
# }
#
# # check
#
# imputed_19one$imputations$imp[[1]]
#
# ## 19 Zeros
# m<-10
# imputed_19zero<-NULL
#
# for(i in 1:m) {
#   imputed_19zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 11" & As == 0) %>%
#     arrange(Wave, Id)
# }
#
#
#
# # Here filter only the 2020 Y^1s
# m<-10
# imputed_20one<-NULL
#
# for(i in 1:m) {
#   imputed_20one$imputations$imp[[i]] <- imputed_20only1$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 12" & As == 1) %>%
#     arrange(Wave, Id)
# }
#
# m<-10
# imputed_20zero<-NULL
#
# for(i in 1:m) {
#   imputed_20zero$imputations$imp[[i]] <- imputed_0$imputations[[i]] %>%
#     dplyr::filter(Wave == "Time 12" & As == 0) %>%
#     arrange(Wave, Id)
# }
#
#
#
# # check
#
# imputed_20one$imputations$imp[[1]]
#
#
# # combine the data and arrange by wave
#
# m<-10
# imps_18 <-NULL
# for(i in 1:m) {
#   imps_18$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18zero$imputations$imp[[i]],
#                                                    imputed_18one$imputations$imp[[i]])
# }
#
# m<-10
# imps_19 <-NULL
# for(i in 1:m) {
#   imps_19$imputations$imp[[i]] <- dplyr::bind_rows(imputed_19zero$imputations$imp[[i]],
#                                                    imputed_19one$imputations$imp[[i]])
# }
#
# m<-10
# imps_20 <-NULL
# for(i in 1:m) {
#   imps_20$imputations$imp[[i]] <- dplyr::bind_rows(imputed_20zero$imputations$imp[[i]],
#                                                    imputed_20one$imputations$imp[[i]])
# }
#
#
# ## ALL 1s
#
#
# m<-10
# imps_all <-NULL
# for(i in 1:m) {
#   imps_all$imputations$imp[[i]] <- dplyr::bind_rows(
#     imputed_18zero$imputations$imp[[i]],
#     imputed_18one$imputations$imp[[i]],
#     imputed_19zero$imputations$imp[[i]],
#     imputed_19one$imputations$imp[[i]],
#     imputed_20zero$imputations$imp[[i]],
#     imputed_20one$imputations$imp[[i]]
#   ) %>%
#     arrange(Wave, Id)
# }
#
#
# # ## ALL 1s
# # # m<-10
# # # imps_trunc <-NULL
# # # for(i in 1:m) {
# # #   imps_all$imputations$imp[[i]] <- dplyr::bind_rows(
# # #     imputed_18zero$imputations$imp[[i]],
# # #     imputed_18one$imputations$imp[[i]],
# # #    # imputed_19zero$imputations$imp[[i]],
# # #     imputed_19one$imputations$imp[[i]],
# # #   #  imputed_20zero$imputations$imp[[i]],
# # #     imputed_20one$imputations$imp[[i]]
# # #   ) %>%
# # #     arrange(Wave, Id)
# # # }
# #
# # saveRDS(imps_trunc, here::here("_posts","mus","mods", "imps_all"))
# #
# # # Save data
# # #saveRDS(imps_all, here::here("_posts","mus","mods", "imps_all"))
# #
# #
# # #mod 2018
# # m<-10
# # model_18<-NULL
# # for(i in 1:m) {
# #   model_18[[i]] <- lm(Ys ~ As, data = imps_18$imputations$imp[[i]])
# #   }
# #
# # pool_parameters(model_18)
# # pred_18<- ggeffects::ggemmeans(model_18[[6]], terms = c("As"))
# # mus_plot_18 <-plot(pred_18)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# #
# # mus_plot_18
# #
# #
# # # mod 19
# # #mod 2018
# # m<-10
# # model_19<-NULL
# # for(i in 1:m) {
# #   model_19[[i]] <- lm(Ys ~ As, data = imps_19$imputations$imp[[i]])
# #   }
# #
# # pool_parameters(model_19)
# # pred_19<- ggeffects::ggemmeans(model_19[[6]], terms = c("As"))
# # mus_plot_19 <-plot(pred_19)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# #
# # mus_plot_19
# #
# # # mod 20
# # m<-10
# # model_20<-NULL
# # for(i in 1:m) {
# #   model_20[[i]] <- lm(Ys ~ As, data = imps_20$imputations$imp[[i]])
# #   }
# #
# #
# # pool_parameters(model_18)
# # pool_parameters(model_19)
# # pool_parameters(model_20)
# #
# #
# # model_20<- ggeffects::ggemmeans(model_20[[6]], terms = c("As"))
# # mus_plot_20 <-plot(model_20)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# #
# # mus_plot_18 + mus_plot_19 + mus_plot_20
#
#
# # all one
#
#
# imps_all$imputations$imp[[1]]$Wave
# # #for brms model
# dat_l<-imps_all$imputations$imp
# #saveRDS(dat_l, here::here("_posts", "mus", "mods", "dat_l"))
# # aveRDS(imps_all, here::here("_posts", "mus", "mods", "imps_all"))
#
#
# #USE
# m<-10
# model_all<-NULL
# for(i in 1:m) {
#   model_all[[i]] <- lmer(Ys ~  As * Wave + (1|Id), data = imps_all$imputations$imp[[i]])
# }
#
#
#
# # USE
# tab<-pool_parameters(model_all)
# tab
# tab [,c(1:5)]%>%
#   # print_md()%>%
#   kbl("latex",booktabs = TRUE,digits=2)
#
# plot(tab, show_labels = TRUE)
#
# pl_ml <- ggeffects::ggemmeans(model_all[[3]], terms = c("Wave","As"))
#
#
# mus_plot_model_all <-plot(pl_model_all)+ scale_y_continuous(limits=c(4.10,4.5))+labs(subtitle="Effect of attack on acceptance of Muslims") +  coord_flip()
# mus_plot_model_all
#
#
#
#
# ## Another approach a little better for writing code, identical results
# library(merTools)
# #imps_all<- readRDS(here::here("_posts", "mus", "mods","imps_all"))
#
# modList <- lmerModList(Ys ~  Wave*As + (1|Id), data =  imps_all$imputations$imp)
# fixef(modList) # model with dropped missing
#
# modelFixedEff(modList)[,c(1:5)]%>%
#   kbl("latex",booktabs = TRUE,digits=3)
#
#
#
#
# estimate_means(
#   model_all[[1]],
#   contrast = "As",
#   at = c("As","Wave"),
#   # fixed = NULL,
#   # transform = "none",
#   ci = 0.95,
#   adjust = "holm",
#   length = 2
# )
#
# estimate_contrasts( model_all[[1]],
#                     contrast = "As",
#                     at = c("Wave","As") )%>%
#   kbl("latex",booktabs = TRUE,digits=2)
#
#
#
#
#
# mus_plot_18 + mus_plot_19 + mus_plot_20
#
#
#
# # str(dat4)
# # str(dat3)
# #saveRDS(dat_l, here::here("_posts", "mus", "mods", "dat_l"))
# # # saveRDS(imps_all, here::here("_posts", "mus", "mods", "imps_all"))
# #  dat_l <- readRDS(here::here("_posts", "mus", "mods", "dat_l"))
# #
# # tscsPlot(imputed_20only1, var = "Ys", cs =c(2,4,491,564,76,582), draws = 100, conf = 0.9, misscol = "red",
# #   obscol = "black")
# #
#
# ## 2018-2020 contrast
# # m<-10
# # imps_base2020 <-NULL
# #
# # for(i in 1:m) {
# #   imps_base2020$imputations$imp[[i]] <- dplyr::bind_rows(imputed_18b$imputations[[i]],
# #                                       imputed_18_3$imputations[[i]],
# #                                       imputed_20s_3$imputations$imp[[i]]%>%
# #                                        droplevels())
# #   imps_base2020$imputations$imp[[i]] <- imps_base2020$imputations$imp[[i]] %>% arrange(Wave,Id)
# # }
# #
# # m<-10
# # imps_d <-NULL
# # dat_d <- NULL
# #
# # for(i in 1:m) {
# #   imps_d$imputations[[i]]<- as.data.frame(imps_base2020$imputations$imp[[i]])
# # }
# #
# # dat_d<-imps_d$imputations
# # m<-10
# # models_d<-NULL
# # for(i in 1:m) {
# #   models_d[[i]] <- lmer(Ys ~ As * Wave  + (1|Id), data=imps_d$imputations[[i]])
# # }
# #
# # pool_parameters(models_d)
# # pred__base20202<- ggeffects::ggemmeans(models_d[[6]], terms = c("Wave","As"))
# # plot(pred__base20202)
# # mus_plot_base20202 <-plot(pred__base20202)+ scale_y_continuous(limits=c(4,4.6))+labs(title="Effect of attack on acceptance of Muslims")
# #
# # mus_plot
#
#
# ### lognormal prior
#
# # m_lgn <- brms::brm(
# #   bf(Ys ~ As  *  Wave + (1|Id)),#,
# #   # sigma ~ As + Wave + (1|Id)),
# #   data = dat_l,
# #   prior = c(prior(lognormal(0, 1), class = b, coef = "WaveWave11"),
# #             prior(lognormal(0, 1), class = b, coef = "WaveWave12")),
# #   seed = 1234,
# #   warmup = 1000,
# #   iter = 2000,
# #   chains = 4,
# #   backend = "cmdstanr",
# #   #save_pars=save_pars(group=FALSE))
# #   file = here::here("_posts", "mus", "mods", "m_lgn"))
# #
# # lazerhawk::brms_SummaryTable(m_lgn, panderize=F)
#
#
#
# # USE?
#
# m_prior <- brms::brm(
#   bf(Ys ~ As  *  Wave + (1|Id)),#,
#   # sigma ~ As + Wave + (1|Id)),
#   data = dat_l,
#   prior = c(prior(normal(0, .25), class = b, coef = "WaveTime11"),
#             prior(normal(0, .25), class = b, coef = "WaveTime12")),
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_prior"))
#
# blh_tab_nat <- lazerhawk::brms_SummaryTable(m_prior, panderize=F)
# blh_tab_nat %>%
#   kable(booktabs = T, "latex", caption =  "Parameter for effect of attack on warmth to Muslims") %>%
#   print()
#
# # Same
#
#
# plot(m_prior)
#
#
# #
#
# # m_prior_t <- brms::brm(
# #   bf(Ys ~ 0 + As:Wave + (1|Id)),#,
# #   # sigma ~ As + Wave + (1|Id)),
# #   data = dat_l,
# #   seed = 1234,
# #   warmup = 1000,
# #   iter = 2000,
# #   chains = 4,
# #   backend = "cmdstanr",
# #   #save_pars=save_pars(group=FALSE))
# #   file = here::here("_posts", "mus", "mods", "m_prior_t"))
# #
# # lazerhawk::brms_SummaryTable(m_prior_t, panderize=F)
# # plot(m_prior_t)
#
# #  will not work
# # m_prior_c <- brms::brm(
# #   bf(Ys ~ As + Wave + (1|Id)),#,
# #   # sigma ~ As + Wave + (1|Id)),
# #   prior = c(set_prior("constant(0)", class = "b", coef = "WaveTime11"),
# #             set_prior("constant(0)", class = "b", coef = "WaveTime12")),
# #   data = dat_l,
# #   seed = 1234,
# #   warmup = 1000,
# #   iter = 2000,
# #   chains = 4,
# #   backend = "cmdstanr",
# #   #save_pars=save_pars(group=FALSE))
# #   file = here::here("_posts", "mus", "mods", "m_prior_c"))
#
# lazerhawk::brms_SummaryTable(m_prior_c, panderize=F)
# plot(m_prior_c)
#
#
# m_prior_s <- brms::brm(
#   bf(Ys ~ As  *  Wave + (1|Id)),#,
#   # sigma ~ As + Wave + (1|Id)),
#   data = dat_l,
#   prior = c(set_prior("constant(0)", class = "b", coef = "WaveWave11"),
#             set_prior("constant(0)", class = "b", coef = "WaveWave12")),
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_prior_s"))
#
# # # Same
# lazerhawk::brms_SummaryTable(m_prior_s, panderize=F)
#
# plot(m_prior_s)
# plot
#
#
# #
# # m_use <- brms::brm(
# #   bf(Ys ~  Wave  +  As:Wave + (1 | Id)),
# #   data = dat_l,
# #   prior = c(
# #     prior(normal(0, .25), class = b, coef = "WaveTime11"),
# #     prior(normal(0, .25), class = b, coef = "WaveTime12")
# #   ),
# #   # prior = c(set_prior("constant(0)", class = "b", coef = "WaveWave11"),
# #   #          set_prior("constant(0)", class = "b", coef = "WaveWave12")),
# #   seed = 1234,
# #   warmup = 1000,
# #   iter = 2000,
# #   chains = 4,
# #   backend = "cmdstanr",
# #   #save_pars=save_pars(group=FALSE))
# #   file = here::here("_posts", "mus", "mods", "m_use")
# #   )
#
# lazerhawk::brms_SummaryTable(m_use, panderize=F)
#
#
# m_use_pr <- brms::brm(
#   bf(Ys ~  Wave  +  As:Wave + (1 | Id)),
#   data = dat_l,
#   prior = c(set_prior("constant(0)", class = "b", coef = "WaveTime11"),
#             set_prior("constant(0)", class = "b", coef = "WaveTime12")),
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_use_pr")
# )
#
# lazerhawk::brms_SummaryTable(m_use_pr, panderize=F)
#
# plot(m_use_pr)
#
#
# ## interaction
# .06
# m_use_pr_int <- brms::brm(
#   bf(Ys ~  As * Wave + (1 | Id)),
#   data = dat_l,
#   prior = c(set_prior("constant(0)", class = "b", coef = "WaveTime11"),
#             set_prior("constant(0)", class = "b", coef = "WaveTime12")),
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_use_pr_int")
# )
#
# lazerhawk::brms_SummaryTable(m_use_pr_int, panderize=F)
#
# plot(m_use_pr_int)
#
#
#
#
#
# # m_use_pr_sens <- brms::brm(
# #   bf(Ys ~  As * Wave + (1 | Id)),
# #   data = dat_l,
# #   prior = c(set_prior("constant(0.06)", class = "b", coef = "WaveTime11"),
# #            set_prior("constant(0.06)", class = "b", coef = "WaveTime12")),
# #   seed = 1234,
# #   warmup = 1000,
# #   iter = 2000,
# #   chains = 4,
# #   backend = "cmdstanr",
# #   #save_pars=save_pars(group=FALSE))
# #   file = here::here("_posts", "mus", "mods", "m_use_pr_sens")
# #   )
#
# # sense 2
# m_use_pr_sens2 <- brms::brm(
#   bf(Ys ~  As * Wave + (1 | Id)),
#   data = dat_l,
#   prior = c(set_prior("constant(0.06)", class = "b", coef = "WaveTime11"),
#             set_prior("constant(0.12)", class = "b", coef = "WaveTime12")),
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_use_pr_sens2")
# )
#
# lazerhawk::brms_SummaryTable(m_use_pr_sens2, panderize=F)
#
# plot(m_use_pr_int)
# prior_summary(m_use_pr_int)
#
# m_use_pr_sens3 <- brms::brm(
#   bf(Ys ~  As * Wave + (1 | Id)),
#   data = dat_l,
#   prior = c(
#     set_prior("constant(4.20)", class = "Intercept"),
#     #set_prior("constant(1.47)", class = "sd", coef = "Intercept"),
#     set_prior("constant(0.06)", class = "b", coef = "WaveTime11"),
#     set_prior("constant(0.12)", class = "b", coef = "WaveTime12")),
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   #save_pars=save_pars(group=FALSE))
#   file = here::here("_posts", "mus", "mods", "m_use_pr_sens3")
# )
#
# lazerhawk::brms_SummaryTable(m_use_pr_sens3, panderize=F)
#
# plot(m_use_pr_int)
#
# ## Sensitivity analysis
#
#
# ```
#
# Marginal means
#
# ### Graphs
#
# ```{r}
# #### GRAPH
# library(tidybayes)
# library(emmeans)
# marg_eff_attack_0 <- m_use_pr_int %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
#               re_formula = NA)
#
# marg_eff_attack_0
# #saveRDS(marg_eff_attack_0,  here::here("_posts", "mus", "mods", "marg_eff_attack_0"))
# marg_eff_attack_0 <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0.rds"))
#
# marg_eff_attack_0a <- m_prior %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
#               re_formula = NA)
# #saveRDS(marg_eff_attack_0a,  here::here("_posts", "mus", "mods", "marg_eff_attack_0a"))
#
# # sensitivity
# marg_eff_attack_0s <- m_use_pr_sens %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
#               re_formula = NA)
# #saveRDS(marg_eff_attack_0s,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s"))
#
# marg_eff_attack_0s
# # marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))
#
#
#
# marg_eff_attack_0s2 <- m_use_pr_sens2 %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
#               re_formula = NA)
# saveRDS(marg_eff_attack_0s,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s2"))
#
# marg_eff_attack_0s2
# # marg_eff_attack_0a <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))
#
# marg_eff_attack_0s3 <- m_use_pr_sens3 %>%
#   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
#               re_formula = NA)
#
# saveRDS(marg_eff_attack_0s3,  here::here("_posts", "mus", "mods", "marg_eff_attack_0s3"))
#
#
#
# plot_baseline_unconstr <- ggplot(
#   marg_eff_attack_0a,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) +
#   scale_y_discrete(limits=rev) +
#   stat_halfeye() +
#   scale_fill_okabe_ito() +
#   labs(
#     x = "Predicted Warmth Response",
#     #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#     fill = "Conterfactual Contrasts",
#     subtitle = "Bayesian posterior locations of potential outcomes\nModel estimates"
#   ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#   theme_pubclean() +
#   theme(legend.position = "bottom"
#   )+scale_x_continuous(limits=c(4,4.6))
# plot_baseline_unconstr
#
# # marg_eff_m_use_pr <- m_use_pr %>%
# #   epred_draws(newdata = expand.grid(As = c(0, 1), Wave = c("Time 10", "Time 11","Time 12")),
# #               re_formula = NA)
# # saveRDS(marg_eff_m_use_pr,  here::here("_posts", "mus", "mods", "marg_eff_m_use_pr"))
# #
# #  <- readRDS(here::here("_posts", "mus", "mods", "marg_eff_attack_0a.rds"))
#
# ## plot all
# # marg_eff_attack_0
# plot_baseline_constr <- ggplot(
#   marg_eff_attack_0,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) +
#   scale_y_discrete(limits=rev) +
#   stat_halfeye() +
#   scale_fill_okabe_ito() +
#   labs(
#     x = "Predicted Warmth Response",
#     #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#     fill = "Conterfactual Contrasts",
#     subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: constrain baseline to Time 10"
#   ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#   theme_pubclean() +
#   theme(legend.position = "bottom"
#   )+scale_x_continuous(limits=c(4,4.6))
# plot_baseline_constr
#
# #
# # plot_baseline_sens <- ggplot(
# #   marg_eff_attack_0s,
# #   aes(
# #     x = .epred,
# #     y = Wave,
# #     fill = as.factor(As)
# #   ) ) +
# #   scale_y_discrete(limits=rev) +
# #     stat_halfeye() +
# #     scale_fill_okabe_ito() +
# #     labs(
# #       x = "Predicted Warmth Response",
# #       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
# #       fill = "Conterfactual Contrasts",
# #       subtitle = "Bayesian posterior locations of potential outcomes: Time 10,11,12.\nSensitivity analysis: strong time effect"
# #     ) +
# #   #  scale_x_continuous(limits=c(4.0,4.6)) +
# #     theme_pubclean() +
# #     theme(legend.position = "bottom"
# #     )+scale_x_continuous(limits=c(4,4.6))
# # plot_baseline_sens
#
#
# # plot_baseline_sens2 <- ggplot(
# #   marg_eff_attack_0s2,
# #   aes(
# #     x = .epred,
# #     y = Wave,
# #     fill = as.factor(As)
# #   ) ) +
# #   scale_y_discrete(limits=rev) +
# #     stat_halfeye() +
# #     scale_fill_okabe_ito() +
# #     labs(
# #       x = "Predicted Warmth Response",
# #       #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
# #       fill = "Conterfactual Contrasts",
# #       subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: assume strong time effect"
# #     ) +
# #   #  scale_x_continuous(limits=c(4.0,4.6)) +
# #     theme_pubclean() +
# #     theme(legend.position = "bottom"
# #     )+scale_x_continuous(limits=c(4,4.6))
# # plot_baseline_sens2
#
#
# plot_baseline_sens3 <- ggplot(
#   marg_eff_attack_0s3,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) +
#   scale_y_discrete(limits=rev) +
#   stat_halfeye() +
#   scale_fill_okabe_ito() +
#   labs(
#     x = "Predicted Warmth Response",
#     #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#     fill = "Conterfactual Contrasts",
#     subtitle = "Bayesian posterior locations of potential outcomes\nSensitivity analysis: assume strong time effect"
#   ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#   theme_pubclean() +
#   theme(legend.position = "bottom"
#   )+scale_x_continuous(limits=c(4,4.6))
# plot_baseline_sens2
#
# # ML Graph
#
# d_ml <- ggeffects::ggemmeans(model_all[[3]], terms = c("Wave","As")) + labs(subtitle="maximu")
#
# pl_ml <-plot(d_ml) +
#   scale_y_reverse() +
#   coord_flip() +
#   scale_y_continuous(limits=c(4,4.6))+
#   labs(x = "Predicted Warmth Response",
#        #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#        title = '',
#        fill = "Conterfactual Contrasts",
#        subtitle = "Frequentist posterior locations of potential outcomes:\nSensitivity analysis: assume strong time effect") +
#   theme_pubclean() + theme(legend.position = "bottom")
# pl_ml
#
# plot_baseline_constr
# outplot <- (pl_ml + plot_baseline_unconstr ) /
#   (plot_baseline_constr + plot_baseline_sens2 )  +
#   plot_annotation(tag_levels = 'a', title = "Estimation of  predicted marginal effects of the attack under different assumptions")
# outplot
# ggsave(
#   outplot,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 12,
#   height =9,
#   units = "in",
#   filename = "outplot.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 800
# )
#
#
#
#
#
# #marg_eff_attack_0a
# # Obtain contrasts
# library(tidyverse)
# marg_t <- marg_eff_attack_0%>%
#   filter((Wave=="Time 10" | Wave == "Time 11" ) )#Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)
# marg_t
#
#
# # marg_eff_attack_0
# plot_2019 <- ggplot(
#   marg_t,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) +
#   scale_y_discrete(limits=rev) +
#   stat_halfeye() +
#   scale_fill_okabe_ito() +
#   labs(
#     x = "Predicted Warmth Response",
#     #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#     fill = "Conterfactual Contrasts",
#     subtitle = "Bayesian posterior locations of potential outcomes: 2018 vs 2020"
#   ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#   theme_pubclean() +
#   theme(legend.position = "bottom"
#   )+scale_x_continuous(limits=c(4,4.7))
# plot_2019
#
# ggsave(
#   plot_2019,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 12,
#   height = 6,
#   units = "in",
#   filename = "plot_2019",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )
#
#
# ## 2020
# marg_t2 <- marg_eff_attack_0%>%
#   filter((Wave=="Time 10" | Wave == "Time 12" ) )#Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)
# marg_t2
#
#
# # marg_eff_attack_0
# plot_2020 <- ggplot(
#   marg_t2,
#   aes(
#     x = .epred,
#     y = Wave,
#     fill = as.factor(As)
#   ) ) +
#   scale_y_discrete(limits=rev) +
#   stat_halfeye() +
#   scale_fill_okabe_ito() +
#   labs(
#     x = "Predicted Warmth Response",
#     #y = "wave 2018(yrs 2018-2019) & wave 2018 (yrs 2019-2020)",
#     fill = "Conterfactual Contrasts",
#     subtitle = "Bayesian posterior locations of potential outcomes: 2018 vs. 2020"
#   ) +
#   #  scale_x_continuous(limits=c(4.0,4.6)) +
#   theme_pubclean() +
#   theme(legend.position = "bottom"
#   )+scale_x_continuous(limits=c(4,4.7))
# plot_2020
#
# ggsave(
#   plot_2020,
#   path = here::here(here::here("_posts", "mus", "figs")),
#   width = 12,
#   height = 6,
#   units = "in",
#   filename = "plot_2020",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )
#
#
# #plot_all
#
# #
# # #
# # grand_mean_ame_a <- m_bayesR %>%
# #     emmeans(~ As*Wave,
# #           epred = TRUE, re_formula = NA) %>%
# #   contrast() %>%
# #   gather_emmeans_draws()
# #
# # grand_mean_ame_a
# # #   filter(As == 1 & Wave ==2019) #Attack == 1 & Wave ==2018|  Attack == 0 & Wave ==2018)
# #
# # saveRDS(grand_mean_ame_a,  here::here("_posts", "mus", "mods", "grand_mean_ame_a.rds"))
# # #grand_mean_ame_a <- readRDS(here::here("_posts", "mus", "mods", "grand_mean_ame_a.rds"))
# #
# # # remove erro contrasts / we don't observe 0 in year two
# #
#
# # does not
#
# # plot_all__ame <- ggplot(grand_mean_ame_a,
# #                                      aes(x = .value)) +
# #   stat_halfeye(slab_alpha = 0.75, fill = "grey") +
# #   scale_fill_okabe_ito(order = c(3, 4)) +
# #   labs(x = "Average marginal effect of attack on warmth to Muslims",
# #        y = "Density") +
# #   #facet_wrap(vars(region)) +
# #   theme_classic() +
# #   theme(legend.position = "bottom") + labs(title= "Marginal effect of attack on warmth to Muslims")
# # plot_all__ame
# # Try
# library(patchwork)
# bayes_plots_a <- plot_all + plot_all__ame + plot_annotation(tag_levels = 'a')
# bayes_plots_a
#
# # ggsave(
# #   bayes_plots_a,
# #   path = here::here(here::here("_posts", "mus", "figs")),
# #   width = 10,
# #   height = 5,
# #   units = "in",
# #   filename = "bayes_plots_a.jpg",
# #   device = 'jpeg',
# #   limitsize = FALSE,
# #   dpi = 1200
# # )
#
#
