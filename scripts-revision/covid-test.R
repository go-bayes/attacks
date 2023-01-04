# set digits
options(scipen = 999)

#libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

library("modelsummary")
options("modelsummary_format_numeric_latex" = "plain")

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("lead", "dplyr")

# additional (add to libs later)
library(ggdist)
library(geepack)

# read functions (most not used here)
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


# bayesian models set up
library("brms")
library("rstan")
rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
theme_set(theme_pubclean()) # nice theme
library(cmdstanr)

# for saving models (again for JB only - set paths for your own directory)

# set paths for JB** YOU NEED TO SET YOUR OWN **
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/attacks/mods")

push_figs <-
  fs::path_expand(" /Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/attacks/figs")

pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )


# Read data
dat <- arrow::read_parquet(pull_path)

dt <- dat |>
  dplyr::filter((Wave == "2020" & YearMeasured == 1) |
                  (Wave == "2021"  & YearMeasured == 1)) |>
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  group_by(Id) |>  # only those who were in 2020
  dplyr::mutate(org2020 =  ifelse(Wave == 2020 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold20 = mean(org2020, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold20 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  droplevels() %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0),
                SexualOrientation = as.factor(if_else(
                  SexualOrientationL1 == 1,
                  "Heterosexual",
                  if_else(SexualOrientationL1 ==
                            2, "Homosexual", "OtherSexuality")
                ))) %>%
  dplyr::mutate(Gender3 = as.factor(ifelse(
    GendAll == 0,
    "Female",
    if_else(GendAll == 1, "Male", "GenderDiverse")
  ))) %>%
  mutate(wave = as.numeric(Wave) - 1) |>
  arrange(Id, Wave)

levels(dt$Wave) <- c("Time12", "Time13")


N <- length(unique(dt$Id))

# Check
N
# below not run


# increasing rate
dat %>%
  group_by(Wave) %>%
  summarise(mean(Warm.Muslims, na.rm = TRUE))




sub_dat <- dat %>%
  filter(Wave == 2019) %>%
  dplyr::mutate(cum_lockdowns_time11 = if_else(
    COVID19.Timeline < 1.2,
    0,
    if_else(
      COVID19.Timeline >  1.2 & COVID19.Timeline  < 2,
      2,
      ifelse(
        COVID19.Timeline > 2 &
          REGC_2022 == 2  | COVID19.Timeline > 2 & REGC_2022 == 1,
        4,
        3
      )
    )
  )) |>
  mutate(pre_post = if_else (  COVID19.Timeline >  1.1, 1, 0 )) |>
  select(Warm.Muslims, cum_lockdowns_time11, REGC_2022, Id , COVID19.Timeline, pre_post) |>
  drop_na()


length(unique(sub_dat$Id))

# Cumulative locksdowns nothing
lm(data = sub_dat, Warm.Muslims ~ cum_lockdowns_time11 + as.factor(REGC_2022)) |>
  model_parameters()



# nothing
lm(data = sub_dat, Warm.Muslims ~ pre_post ) |>
  model_parameters()|>
  kbl(format = "latex", booktabs = TRUE, digits = 3)


# nothing
lm(data = sub_dat, Warm.Muslims ~ cum_lockdowns_time11 ) |>
  model_parameters() |>
  kbl(format = "latex", booktabs = TRUE, digits = 3)



## Time 4 cohort

dat_bayes <- arrow::read_parquet(here::here(push_mods, "2012_cohort_attacks"))


sub_dat4 <- dat_bayes %>%
  filter(Wave == "Time11") %>%
  dplyr::mutate(cum_lockdowns_time11 = if_else(
    COVID19.Timeline < 1.2,
    0,
    if_else(
      COVID19.Timeline >  1.2 & COVID19.Timeline  < 2,
      2,
      ifelse(
        COVID19.Timeline > 2 &
          REGC_2022 == 2  | COVID19.Timeline > 2 & REGC_2022 == 1,
        4,
        3
      )
    )
  )) |>
  mutate(pre_post = if_else (  COVID19.Timeline >  1.1, 1, 0 )) |>
  select(Warm.Muslims, cum_lockdowns_time11, REGC_2022, Id , COVID19.Timeline, pre_post) |>
  drop_na()


length(unique(sub_dat4$Id))

# Cumulative locksdowns nothing
lm(data = sub_dat4, Warm.Muslims ~ cum_lockdowns_time11 + as.factor(REGC_2022)) |>
  model_parameters()



# nothing
lm(data = sub_dat4, Warm.Muslims ~ pre_post ) |>
  model_parameters()|>
  kbl(format = "latex", booktabs = TRUE, digits = 3)


# nothing
lm(data = sub_dat, Warm.Muslims ~ cum_lockdowns_time11 ) |>
  model_parameters() |>
  kbl(format = "latex", booktabs = TRUE, digits = 3)






















# pre_vals ----------------------------------------------------------------

sub_dat2 <- tab_in |>
  mutate(lag_warm_muslims_z = scale( dplyr::lag(Warm.Muslims, n = 1) ) ,
         lag_pol_orient_z = scale( dplyr::lag(Pol.Orient, n = 1))) |>
  filter(Wave == 2019) %>%
  dplyr::mutate(cum_lockdowns_time11 = if_else(
    COVID19.Timeline < 1.2,
    0,
    if_else(
      COVID19.Timeline >  1.2 & COVID19.Timeline  < 2,
      2,
      ifelse(
        COVID19.Timeline > 2 &
          REGC_2022 == 2  | COVID19.Timeline > 2 & REGC_2022 == 1,
        4,
        3
      )
    )
  )) |>
  mutate(pre_post = if_else (  COVID19.Timeline >  1.1, 1, 0 )) |>
  select(lag_warm_muslims_z, Warm.Muslims, cum_lockdowns_time11, REGC_2022, Id , COVID19.Timeline, pre_post) |>
  droplevels()

length(unique(sub_dat2$Id))

lm(data = sub_dat2, Warm.Muslims ~ cum_lockdowns_time11 + as.factor(REGC_2022) + lag_warm_muslims_z) |>
  model_parameters()


lm(data = sub_dat2, Warm.Muslims ~ cum_lockdowns_time11 + pre_post + lag_warm_muslims_z) |>
  model_parameters()



lm(data = sub_dat2, Warm.Muslims ~ pre_post  + lag_warm_muslims_z) |>
  model_parameters()





