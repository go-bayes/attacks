# pre-attack trend

# Analyisis
# author Joseph Bulbulia: joseph.bulbulia@gmail.com
# created by assembling materials from original analysis into one script.


# packages ----------------------------------------------------------------
source(here::here("R", "libs.R"))



# import data -------------------------------------------------------------
#df <- readRDS(here::here("data_raw", "df"))# orig data

# jittered data for reproduction (NZAVS ethics does not permit original data deposited
# to internet. However, as per note, original data can be obtained, just email c.sibley@auckland.ac.nz or the chair of the U of Auckland ethics committee.

df <- readRDS(here::here("data", "df_s"))

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
  droplevels() %>% dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
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
  arrange(Wave, Id) |>
  droplevels()

# levels for waves - nicer graphs
levels(all_d$Wave) <-c("Time4", "Time5", "Time6", "Time7", "Time8", "Time9")


# select variables for pre-attack model (which will give us the slope of antimuslim prejudice in the years before the attacks)
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
  table1::table1(~ Ys |
                   Wave, data = all_d_selected, overall = FALSE)

# create table
kable(tall, format = "latex", booktabs = TRUE)



# Atrition and non-response may lead to bias. We need to deal with missingness.  To multiply impute missing data we use the Amelia package

# multiple imputation
library(Amelia)

# We impute the entire distribution of Attack = 0,


# assume Y^0|A=2018 = Y^0 2019
match("Ys", names(all_d_selected))  # for obtaining bounds for Muslim outcome

# aata needs to be a data frame to pass to  Amelia
all_d_selected <- as.data.frame(all_d_selected)

saveRDS(all_d_selected, here::here("mods", "all_d_selected"))

# bounds for warmth
bds <- matrix(c(5, 1, 7), nrow = 1, ncol = 3) # bounds for

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

# save data here
saveRDS(imputed_m, here::here("mods", "imputed_m"))

# fetch data
imputed_m <-
  readRDS(here::here("mods", "imputed_m"))

# check data range
summary(imputed_m$imputations[[1]]$yrs)

# to compare with linear model
library(splines)

# Assume non-linear trajectory
m <- 10
model_m <- NULL
for (i in 1:m) {
  model_m$model[[i]] <-
    lmer(Ys ~ bs(yrs)  + (1 |Id), data = imputed_m$imputations[[i]])
}

pool_parameters(model_m$model)


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
            size = 1 / 4) + theme_classic() +  scale_y_continuous(limits =
                                                                    c(4.0, 4.5)) +
  labs(title = "National New Zealand trajectory in Muslim acceptance",
       subtitle = "years: 2012-2017/18; N = 12179") +
  labs(y = "Muslim Warmth",
       x = "Years:2012-2017/19") +
  scale_y_continuous(limits = c(3, 4.5)) +  theme_classic() #coord_flip()

plot_time


# save graph
ggsave(
  plot_time,
  path = here::here(here::here("figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "pplot_time,jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


### DATA FOR MISSIN YEARS

