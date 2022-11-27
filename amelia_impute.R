# amelia

# revision conducted on 24 Nov 2022.
# joseph.bulbulia


# bayesian approach
# impute religious identification
# see this https://github.com/paul-buerkner/brms/issues/1385
# perfect impute
# note:
# potentially helpful for brms predict with added noise: https://github.com/paul-buerkner/brms/issues/544
# see also: https://discourse.mc-stan.org/t/predict-brms-in-multivariate-model-with-imputation/6334

# remove scientific notation
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

pull_path
#arrow::write_parquet(time13, (here::here("data", "time13")))
# wow this is fast
#time13 <- read_parquet( (here::here("data", "time13")))

dat <- arrow::read_parquet(pull_path)

dat$Warm.Overweight
table1::table1( ~ Urban| Wave, data = dat)
table1::table1( ~ Urban| Wave, data = dat)


table1::table1( ~ Warm.Overweight | Wave, data = dat)
dat$Relid
# wrangle data
# create basic outcomewide dataframe from which we will select the a small dataframe.
dat_bayes2 <- dat |>
  arrange(Id, Wave) |>
  mutate(Male = if_else(GendAll == 1, 1, 0)) |>
  dplyr::select(
    Id,
    Wave,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Age,
    Male,
    EthCat,
    BornNZ,
    Employed,
    Urban,
    Edu,
    Pol.Orient,
    SDO,
    RWA,
    NZSEI13,
    NZDep.2013,
    Relid,
    Partner,
    Parent,
    TSCORE,
    Warm.Asians,
    Warm.Chinese,
    #Warm.Disabled, #only in wave12
    #Warm.Elderly,
    Warm.Immigrants,
    Warm.Indians,
    Warm.Maori,
    #  Warm.MentalIllness,
    # not in 8
    Warm.Muslims,
    Warm.NZEuro,
    Warm.Overweight,
    Warm.Pacific,
    RaceRejAnx,
    #   Warm.Refugees,
    # not in 8
    TSCORE,
    YearMeasured
  ) |>
  dplyr::mutate(Employed = as.numeric(Employed)) |>
  dplyr::filter(
    (Wave ==   2016 & YearMeasured == 1) |
      (Wave ==  2017 & YearMeasured != -1) |
      (Wave ==  2018 & YearMeasured != -1) |
      (Wave ==  2019 & YearMeasured != -1) |
      (Wave ==  2020 & YearMeasured != -1) |
      (Wave == 2021 & YearMeasured != -1)
  ) %>%
  droplevels() |>
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  ungroup() %>%
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
                                    YearMeasured == 1, 1, 0)) %>%
  # dplyr::mutate(org2017 =  ifelse(Wave == 2017 &
  #                                   YearMeasured == 1, 1, 0)) %>%
  # dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
  #                                   YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold > 0) %>%
  # dplyr::mutate(hold2 = mean(org2017, na.rm = TRUE)) %>%  # Hack
  # dplyr::filter(hold2 > 0) %>%
  # dplyr::mutate(hold3 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  # dplyr::filter(hold3 > 0) %>%
  ungroup(Id) |>
  dplyr::mutate(Edu = as.numeric(Edu)) |>
  arrange(Id, Wave) %>%
  group_by(Id) |>
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
        ifelse(
          YearMeasured == 0 &
            Wave == 2020,
          TSCORE_b + 1459,
          if_else(YearMeasured == 0 &
                    Wave == 2021,   TSCORE_b + 1824,
                  TSCORE)
        )
      )
    )
  ))  %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 &
       Wave == 2018) |
      (Wave == 2019 |
         Wave == 2020 |
         Wave == 2021),
    1,
    0
  )))) %>% # All 2019s even if NA need to be 1
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i))) %>%
  dplyr::mutate(
    Y_Warm.Asians = Warm.Asians,
    Y_Warm.Chinese = Warm.Chinese,
    # Warm.Disabled, only in wave12
    # Y_Warm.Elderly = Warm.Elderly,
    Y_Warm.Immigrants = Warm.Immigrants,
    Y_Warm.Indians = Warm.Indians,
    Y_Warm.Maori = Warm.Maori,
    #  Y_Warm.MentalIllness = Warm.MentalIllness,
    # not in 8
    Y_Warm.Muslims = Warm.Muslims,
    Y_Warm.NZEuro = Warm.NZEuro,
    Y_Warm.Overweight = Warm.Overweight,
    Y_Warm.Pacific = Warm.Pacific,
    #  Y_Warm.Refugees = Warm.Refugees,
    As = Attack
  ) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i))) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  # dplyr::mutate(Age_c = if_else(Wave == "2016", (Age), NA_real_)) %>%
  # fill(Age_c, .direction = "downup") %>%
  # dplyr::mutate(Warm.Muslims_b = if_else(Wave == "2016", (Warm.Muslims), NA_real_)) %>%
  # fill(Warm.Muslims_b, .direction = "downup") %>%
  # dplyr::mutate(Warm.Overweight_b = if_else(Wave == "2016", (Warm.Overweight), NA_real_)) %>%
  # fill(Warm.Overweight_b, .direction = "downup") %>%
  # dplyr::mutate(CONSCIENTIOUSNESS_c = if_else(Wave == "2016", (CONSCIENTIOUSNESS), NA_real_)) %>%
  # fill(CONSCIENTIOUSNESS_c,  .direction = "downup") %>%
  # dplyr::mutate(OPENNESS_c = if_else(Wave == "2016", (OPENNESS), NA_real_)) %>%
  # fill(OPENNESS_c,  .direction = "downup") |>
  # dplyr::mutate(HONESTY_HUMILITY_c = if_else(Wave == "2016", (HONESTY_HUMILITY), NA_real_)) %>%
  # fill(HONESTY_HUMILITY_c,  .direction = "downup") |>
  # dplyr::mutate(EXTRAVERSION_c = if_else(Wave == "2016", (EXTRAVERSION), NA_real_)) %>%
  # fill(EXTRAVERSION_c,  .direction = "downup") |>
  # dplyr::mutate(NEUROTICISM_c = if_else(Wave == "2016", (NEUROTICISM), NA_real_)) %>%
  # fill(NEUROTICISM_c,  .direction = "downup") |>
  # dplyr::mutate(AGREEABLENESS_c = if_else(Wave == "2016", (AGREEABLENESS), NA_real_)) %>%
  # fill(AGREEABLENESS_c,  .direction = "downup") |>
  # dplyr::mutate(Male_c = if_else(Wave == "2016", as.numeric(Male), NA_real_)) %>%
  # fill(Male_c,  .direction = "downup") %>%
  # dplyr::mutate(NZDep.2013_c = if_else(Wave == "2016", as.numeric(NZDep.2013), NA_real_)) %>%
  # fill(NZDep.2013_c,  .direction = "downup")  %>%
  # dplyr::mutate(RaceRejAnx_c = if_else(Wave == "2016", as.numeric(RaceRejAnx), NA_real_)) %>%
  # fill(RaceRejAnx_c,  .direction = "downup")  %>%
  # dplyr::mutate(EthCat_c = if_else(Wave == "2016", as.numeric(EthCat), NA_real_)) %>%
  # fill(EthCat_c,  .direction = "downup") %>%
  # dplyr::mutate(BornNZ_c = if_else(Wave == "2016", as.numeric(BornNZ), NA_real_)) %>%
  # fill(BornNZ_c,  .direction = "downup")  %>%
  # dplyr::mutate(Pol.Orient_c = if_else(Wave == "2016", (Pol.Orient), NA_real_)) %>%
  # fill(Pol.Orient_c,  .direction = "downup") %>%
  # dplyr::mutate(Relid_c = if_else(Wave == "2016", as.numeric(Relid), NA_real_)) %>%
  # fill(Relid_c,  .direction = "downup") %>%
  # dplyr::mutate(Partner_c = if_else(Wave == "2016", (as.numeric(Partner)), NA_real_)) %>%
  # fill(Partner_c,  .direction = "downup") %>%
  # dplyr::mutate(Parent_c = if_else(Wave == "2016", (as.numeric(Parent)), NA_real_)) %>%
  # fill(Parent_c,  .direction = "downup") %>%
  # dplyr::mutate(Employed_c = if_else(Wave == "2016", (as.numeric(Employed)), NA_real_)) %>%
  # fill(Employed_c,  .direction = "downup") %>%
  # dplyr::mutate(Edu_c = if_else(Wave == "2016", (Edu), NA_real_)) %>%
  # fill(Edu_c,  .direction = "downup") %>%
  # dplyr::mutate(Urban_c = if_else(Wave == "2016", (as.numeric(Urban)), NA_real_)) %>%
  # fill(Urban_c,  .direction = "downup") %>%
  # dplyr::mutate(SDO_c = if_else(Wave == "2016", (as.numeric(SDO)), NA_real_)) %>%
  # fill(SDO_c,  .direction = "downup") %>%
  # dplyr::mutate(RWA_c = if_else(Wave == "2016", (as.numeric(RWA)), NA_real_)) %>%
  # fill(RWA_c,  .direction = "downup") %>%
  # dplyr::mutate(NZSEI13_c = if_else(Wave == "2016", (as.numeric(NZSEI13)), NA_real_)) %>%
  # fill(NZSEI13_c,  .direction = "downup") %>%
  # dplyr::mutate(Warm.Muslims_c = if_else(Wave == "2016", (as.numeric(Warm.Muslims)), NA_real_)) %>%
  # fill(Warm.Muslims_c,  .direction = "downup") %>%
  # dplyr::mutate(Warm.Overweight_c = if_else(Wave == "2016", (as.numeric(Warm.Overweight)), NA_real_)) %>%
  # fill(Warm.Overweight_c,  .direction = "downup") %>%
  ungroup() %>%
  # select(
  #   -c(
  #     Employed,
  #     Urban,
  #     Edu,
  #     Pol.Orient,
  #     SDO,
  #     RWA,
  #     NZSEI13,
  #     NZDep.2013,
  #     Age,
  #     Relid,
  #     RaceRejAnx,
  #     Partner,
  #     Parent,
  #     hold,
  #     Age,
  #     EthCat,
  #     BornNZ,
  #     TSCORE,
  #     org2016,
  #     hold,
  #     CONSCIENTIOUSNESS,
  #     OPENNESS,
  #     HONESTY_HUMILITY,
  #     EXTRAVERSION,
  #     NEUROTICISM,
  #     AGREEABLENESS
  #   )
  # ) |>
  dplyr::mutate(EthCat = as.factor(EthCat)) |>
  # dplyr::mutate(
  #   Age_z = scale(Age),
  #   BornNZ_cZ = scale(BornNZ),
  #   Male_cZ = scale (Male_c),
  #   Edu_cZ = scale(Edu_c),
  #   Employed_cZ = scale(Employed_c),
  #   # EthCat_c = EthCat_c,
  #   Parent_cZ = scale(Parent_c),
  #   Partner_cZ = scale(Partner_c),
  #   Relid_cZ = scale(Relid_c),
  #   RaceRejAnx_cZ = scale(RaceRejAnx_c),
  #   Pol.Orient_cZ = scale(Pol.Orient_c),
  #   Urban_cZ = scale(Urban_c),
  #   SDO_cZ = scale(SDO_c),
  #   RWA_cZ = scale(RWA_c),
  #   NZDep.2013_cZ = scale(NZDep.2013_c),
  #   NZSEI13_cZ = scale(NZSEI13_c),
  #   AGREEABLENESS_cZ = scale(AGREEABLENESS_c),
  #   CONSCIENTIOUSNESS_cZ = scale(CONSCIENTIOUSNESS_c),
  #   OPENNESS_cZ = scale(OPENNESS_c),
  #   HONESTY_HUMILITY_cZ = scale(HONESTY_HUMILITY_c),
  #   EXTRAVERSION_cZ = scale(EXTRAVERSION_c),
  #   NEUROTICISM_cZ = scale(NEUROTICISM_c)
  # ) %>%
  dplyr::arrange(Id, Wave)

# relabel wave
levels(dat_bayes2$Wave) <-
  c("Time8", "Time9", "Time10", "Time11", "Time12", "Time13")

length(unique(dat_bayes2$Id)) #  21936

table(dat_bayes2$Wave)
# image
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = dat_bayes)


# check
x <- table1::table1(
  ~ Y_Warm.Muslims + Y_Warm.Overweight |
    factor(Wave) * factor(As),
  data = dat_bayes2,
  overall = F
)
x

kable(x, format = "latex", booktabs = TRUE)
t1kable(x, format = "latex")

# Missing data problem
t2 <- table1::table1(~ Y_Warm.Muslims |
                       Wave * as.factor(As),
                     data = dat_bayes2,
                     overall = F)

# data prep

# create new data set
dt_five_prep2 <- dat_bayes2 %>%
  group_by(Id) %>%
  mutate(As = (
    ifelse(
      Wave == "Time10" & Attack == 1 | Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
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
  mutate(
    Y_Warm.Asians = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8",
        NA,
        Warm.Asians
      )
    )
  ) %>%
  mutate(
    Y_Warm.Overweight = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8",
        NA,
        Warm.Overweight
      )
    )
  ) %>%
  mutate(
    Y_Warm.Chinese = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8",
        NA,
        Warm.Chinese
      )
    )
  ) %>%
  # mutate(
  #   Y_Warm.Elderly = ifelse(
  #     Wave == "Time10" & Attack == 1 |
  #       Wave == "Time11" |
  #       Wave == "Time12",
  #     NA,
  #     ifelse(
  #       Wave == "Time10" & Attack == 0 |
  #         Wave == "Time9" |
  #         Wave == "Time8",
  #       NA,
#       Warm.Elderly
#     )
#   )
# ) %>%
mutate(
  Y_Warm.Immigrants = ifelse(
    Wave == "Time10" & Attack == 1 |
      Wave == "Time11" |
      Wave == "Time12" | Wave == "Time13",
    NA,
    ifelse(
      Wave == "Time10" & Attack == 0 |
        Wave == "Time9" |
        Wave == "Time8",
      NA,
      Warm.Immigrants
    )
  )
) %>%
  mutate(
    Y_Warm.Indians = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8",
        NA,
        Warm.Indians
      )
    )
  ) %>%
  mutate(
    Y_Warm.Maori = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8",
        NA,
        Warm.Maori
      )
    )
  ) %>%
  # mutate(
  #   Y_Warm.MentalIllness = ifelse(
  #     Wave == "Time10" & Attack == 1 |
  #       Wave == "Time11" |
  #       Wave == "Time12",
  #     NA,
  #     ifelse(
  #       Wave == "Time10" & Attack == 0 |
  #         Wave == "Time9" |
  #         Wave == "Time8",
  #       NA,
#       Warm.MentalIllness
#     )
#   )
# ) %>%
mutate(
  Y_Warm.Muslims = ifelse(
    Wave == "Time10" & Attack == 1 |
      Wave == "Time11" |
      Wave == "Time12" | Wave == "Time13",
    NA,
    ifelse(
      Wave == "Time10" & Attack == 0 |
        Wave == "Time9" |
        Wave == "Time8",
      NA,
      Warm.Muslims
    )
  )
) %>%
  mutate(
    Y_Warm.NZEuro = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8",
        NA,
        Warm.NZEuro
      )
    )
  ) |>
  mutate(
    Y_Warm.Pacific = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8",
        NA,
        Warm.Pacific
      )
    )
  ) |>
  ungroup() %>%
  arrange(Id, Wave)

# check
length(unique(dt_five_prep2$Id))


skimr::skim(dt_five_prep2) %>%
  arrange(n_missing)




# bind data
dt_five_bind2 <-  dat_bayes2 %>%
  bind_rows(dt_five_prep2) %>%
  arrange(Id, Wave)


# Test NAs = Correct
table1::table1(
  ~ Y_Warm.Muslims +
    Y_Warm.Chinese +
    # Warm.Disabled, only in wave12
    #  Y_Warm.Elderly +
    Y_Warm.Immigrants +
    Y_Warm.Indians +
    Y_Warm.Maori +
    #   Y_Warm.MentalIllness +  # not in 8
    Y_Warm.Muslims +
    Y_Warm.NZEuro +
    Y_Warm.Overweight +
    Y_Warm.Pacific# +
  #   Y_Warm.Refugees
  |
    Wave * as.factor(As),
  data = dt_five_bind2,
  overall = F
)


#  data wrangle


# link 5 dfs for zero estimate -----------------------------------------

# five data
head()

dt_five_zero_noimpute_temp2 <- dt_five_bind2 |>
  filter((As == 0 & YearMeasured != -1)) |>
  # filter(Wave != "Time11" & Wave != "Time12") |>
  mutate(wave = as.numeric(Wave)) |>
  arrange(Id, Wave)

dt_five_zero_noimpute2 <- dt_five_zero_noimpute_temp2


dt_five_zero_noimpute2$wave  =  dt_five_zero_noimpute_temp2$wave - 3

table(dt_five_zero_noimpute2$wave)

## no impute but with the years



# one data
dt_five_one_noimpute_temp2 <- dt_five_bind2 |>
  filter((As == 1 & Wave == "Time10") |
           (As == 1 & Wave == "Time11") |
           (As == 1 & Wave == "Time12") |
           (As == 1 & Wave == "Time13")
  ) %>%
  mutate(wave = as.numeric(Wave)) |>
  arrange(Id, Wave)

dt_five_one_noimpute2 <- dt_five_one_noimpute_temp2
dt_five_one_noimpute2$Wave
dt_five_one_noimpute2$wave  <- dt_five_one_noimpute_temp2$wave - 3

table(dt_five_one_noimpute2$wave) # Correct


str(dt_five_one_noimpute2)

# Check missing
library(naniar)
naniar::gg_miss_var(dt_five_one_noimpute2)
naniar::gg_miss_var(dt_five_one_noimpute2)

skimr::skim(dt_five_one_noimpute2) %>%
  arrange(n_missing)


colnames(dt_five_one_noimpute2)
# naniar::vis_miss(dt_five_one_noimpute,
#                  warn_large_data = FALSE)


## save data
arrow::write_parquet(
  dt_five_zero_noimpute2,
  here::here(push_mods, "dt_five_zero_noimpute-attacks2.rds")
)

arrow::write_parquet(dt_five_one_noimpute2,
                     here::here(push_mods, "dt_five_one_noimpute-attacks2.rds"))



dt_five_zero_noimpute2 <-
  arrow::read_parquet(here::here(push_mods, "dt_five_zero_noimpute-attacks2.rds"))
dt_five_one_noimpute2 <-
  arrow::read_parquet(here::here(push_mods, "dt_five_one_noimpute-attacks2.rds"))


# bayes models ------------------------------------------------------------


prior = c(
  set_prior('normal(0, .25)', class = "b", coef = "wave"),
  set_prior("student_t(3, 4, 1)", class = "Intercept"),
  set_prior("cauchy(0, 1)", class = "sigma")
)

# set N for id counts
id_0 <- dt_five_zero_noimpute$Id
id_1 <- dt_five_one_noimpute$Id



# set name for error
name_error = "sd"


## impute muslim

# ensure parrallel computations -------------------------------------------


library("brms")
library("rstan")

rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
theme_set(theme_pubclean()) # nice theme
library(cmdstanr)

bform_mus <-
  bf(
    Y_Warm.Muslims | mi()  ~  wave  *
      (
        AGREEABLENESS_cZ +
          CONSCIENTIOUSNESS_cZ +
          OPENNESS_cZ +
          HONESTY_HUMILITY_cZ +
          EXTRAVERSION_cZ +
          NEUROTICISM_cZ +
          Age_cZ +
          BornNZ_cZ +
          Male_cZ +
          Edu_cZ  +
          Employed_cZ +
          EthCat_c  +
          NZDep.2013_cZ +
          NZSEI13_cZ  +
          Parent_cZ  +
          Partner_cZ +
          Pol.Orient_cZ +
          Relid_cZ +
          RaceRejAnx_cZ +
          SDO_cZ +
          RWA_cZ +
          Urban_cZ
      )  +
      (1 | Id)
  )



# test
m_0 <- brm(
  backend = "cmdstanr",
  data = dt_five_zero_noimpute,
  family = "gaussian",
  bform_mus,
  prior = prior,
  init = 0,
  file =  here::here(push_mods, "five-zero-MUS-attacks-use.rds")
)

# one
m_1 <- brm(
  backend = "cmdstanr",
  data = dt_five_one_noimpute,
  family = "gaussian",
  bform_mus,
  prior = prior,
  init = 0,
  file = here::here(push_mods, "five-one-MUS-attacks-use.rds")
)




# overweight --------------------------------------------------------------


bform_overweight <-
  bf(
    Y_Warm.Overweight | mi()  ~     wave  *
      (
        AGREEABLENESS_cZ +
          CONSCIENTIOUSNESS_cZ +
          OPENNESS_cZ +
          HONESTY_HUMILITY_cZ +
          EXTRAVERSION_cZ +
          NEUROTICISM_cZ +
          Age_cZ +
          BornNZ_cZ +
          Male_cZ +
          Edu_cZ  +
          Employed_cZ +
          EthCat_c  +
          NZDep.2013_cZ +
          NZSEI13_cZ  +
          Parent_cZ  +
          Partner_cZ +
          Pol.Orient_cZ +
          Relid_cZ +
          RaceRejAnx_cZ +
          SDO_cZ +
          RWA_cZ +
          Urban_cZ
      )  +
      (1 | Id)
  )


m_3 <- brm(
  backend = "cmdstanr",
  data = dt_five_zero_noimpute,
  family = "gaussian",
  bform_overweight,
  prior = prior,
  init = 0,
  file = here::here(push_mods, "five-zero-OVERWEIGHT-attacks-use.rds")
)


m_4 <- brm(
  backend = "cmdstanr",
  data = dt_five_one_noimpute,
  family = "gaussian",
  bform_overweight,
  prior = prior,
  init = 0,
  file = here::here(push_mods, "five-one-OVERWEIGHT-attacks-use.rds")
)

library(ggeffects)
library(sjmisc)

summary(m_0)
summary(m_1)
summary(m_3)
summary(m_4)

conditional_smooths()
p0 <-
  plot(ggeffects::ggpredict(m_0, terms = c("wave [0:3]", "Pol.Orient_cZ[sd]"))) + scale_y_continuous(limits = c(3, 5))
p1 <-
  plot(ggeffects::ggpredict(m_1, terms = c("wave [0:3]", "Pol.Orient_cZ[sd]"))) + scale_y_continuous(limits = c(3, 5))
p3 <-
  plot(ggeffects::ggpredict(m_3, terms = c("wave [0:3]",  "Pol.Orient_cZ[sd]"))) + scale_y_continuous(limits = c(3, 5))
p4 <-
  plot(ggeffects::ggpredict(m_4, terms = c("wave [0:3]", "Pol.Orient_cZ[sd]"))) + scale_y_continuous(limits = c(3, 5))
p0 + p1
p3 + p4


