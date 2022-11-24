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


# read data (again bulbulia only) If replicating use the jittered data in the data folder
pull_path <-
  fs::path_expand(
    "~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5"
  )
dat <- readRDS(pull_path)

# wrangle data
# create basic outcomewide dataframe from which we will select the a small dataframe.
dat_bayes <- dat |>
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
    Religious,
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
    (Wave ==    2016 & YearMeasured == 1) |
      (Wave ==  2017 & YearMeasured != -1) |
      (Wave ==  2018 & YearMeasured != -1) |
      (Wave ==  2019 & YearMeasured != -1) |
      (Wave ==  2020 & YearMeasured != -1)
  ) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() |>
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold > 0) %>%
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
  dplyr::mutate(Age_c = if_else(Wave == "2016", (Age), NA_real_)) %>%
  fill(Age_c) %>%  #
  dplyr::mutate(CONSCIENTIOUSNESS_c = if_else(Wave == "2016", (CONSCIENTIOUSNESS), NA_real_)) %>%
  fill(CONSCIENTIOUSNESS_c) %>%
  dplyr::mutate(OPENNESS_c = if_else(Wave == "2016", (OPENNESS), NA_real_)) %>%
  fill(OPENNESS_c) |>
  dplyr::mutate(HONESTY_HUMILITY_c = if_else(Wave == "2016", (HONESTY_HUMILITY), NA_real_)) %>%
  fill(HONESTY_HUMILITY_c) |>
  dplyr::mutate(EXTRAVERSION_c = if_else(Wave == "2016", (EXTRAVERSION), NA_real_)) %>%
  fill(EXTRAVERSION_c) |>
  dplyr::mutate(NEUROTICISM_c = if_else(Wave == "2016", (NEUROTICISM), NA_real_)) %>%
  fill(NEUROTICISM_c) |>
  dplyr::mutate(AGREEABLENESS_c = if_else(Wave == "2016", (AGREEABLENESS), NA_real_)) %>%
  fill(AGREEABLENESS_c) |>
  dplyr::mutate(Male_c = if_else(Wave == "2016", as.numeric(Male), NA_real_)) %>%
  fill(Male_c) %>%
  dplyr::mutate(NZDep.2013_c = if_else(Wave == "2016", as.numeric(NZDep.2013), NA_real_)) %>%
  fill(NZDep.2013_c) %>%
  dplyr::mutate(EthCat_c = if_else(Wave == "2016", as.numeric(EthCat), NA_real_)) %>%
  fill(EthCat_c) %>%
  dplyr::mutate(BornNZ_c = if_else(Wave == "2016", as.numeric(BornNZ), NA_real_)) %>%
  fill(BornNZ_c) %>%
  dplyr::mutate(Pol.Orient_c = if_else(Wave == "2016", (Pol.Orient), NA_real_)) %>%
  fill(Pol.Orient_c) %>%
  dplyr::mutate(Religious_c = if_else(Wave == "2016", as.numeric(Religious), NA_real_)) %>%
  fill(Religious_c) %>%
  dplyr::mutate(Partner_c = if_else(Wave == "2016", (as.numeric(Partner)), NA_real_)) %>%
  fill(Partner_c) %>%
  dplyr::mutate(Parent_c = if_else(Wave == "2016", (as.numeric(Parent)), NA_real_)) %>%
  fill(Parent_c) %>%
  dplyr::mutate(Employed_c = if_else(Wave == "2016", (as.numeric(Employed)), NA_real_)) %>%
  fill(Employed_c) %>%
  dplyr::mutate(Edu_c = if_else(Wave == "2016", (Edu), NA_real_)) %>%
  fill(Edu_c) %>%
  dplyr::mutate(Urban_c = if_else(Wave == "2016", (as.numeric(Urban)), NA_real_)) %>%
  fill(Urban_c) %>%
  dplyr::mutate(SDO_c = if_else(Wave == "2016", (as.numeric(SDO)), NA_real_)) %>%
  fill(SDO_c) %>%
  dplyr::mutate(RWA_c = if_else(Wave == "2016", (as.numeric(RWA)), NA_real_)) %>%
  fill(RWA_c) %>%
  dplyr::mutate(NZSEI13_c = if_else(Wave == "2016", (as.numeric(NZSEI13)), NA_real_)) %>%
  fill(NZSEI13_c) %>%
  ungroup() %>%
  select(
    -c(
      Employed,
      Urban,
      Edu,
      Pol.Orient,
      SDO,
      RWA,
      NZSEI13,
      NZDep.2013,
      Age,
      Religious,
      Partner,
      Parent,
      hold,
      Age,
      EthCat,
      BornNZ,
      TSCORE,
      org2016,
      CONSCIENTIOUSNESS,
      OPENNESS,
      HONESTY_HUMILITY,
      EXTRAVERSION,
      NEUROTICISM,
      AGREEABLENESS)
  ) |>
  dplyr::mutate(EthCat_c = as.factor(EthCat_c)) |>
  dplyr::filter(
    !is.na(Age_c),
    !is.na(BornNZ_c),
    !is.na(Male_c),
    !is.na(Edu_c),
    !is.na(Employed_c),
    !is.na(EthCat_c),
    !is.na(Parent_c),
    !is.na(Partner_c),
    !is.na(Religious_c),
    !is.na(Pol.Orient_c),
    !is.na(Urban_c),
    !is.na(SDO_c),
    !is.na(RWA_c),
    !is.na(NZDep.2013_c),
    !is.na(NZSEI13_c),
    !is.na(AGREEABLENESS_c),
    !is.na(CONSCIENTIOUSNESS_c),
    !is.na(OPENNESS_c),
    !is.na(HONESTY_HUMILITY_c),
    !is.na(EXTRAVERSION_c),
    !is.na(NEUROTICISM_c)
  ) |>
  dplyr::arrange(Id, Wave)

# relabel wave
levels(dat_bayes$Wave) <-
  c("Time8", "Time9", "Time10", "Time11", "Time12")

length(unique(dat_bayes$Id)) # 19820

# image
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = dat_bayes)


# check
x <- table1::table1( ~ Y_Warm.Muslims |
                  factor(Wave) * factor(As), data = dat_bayes, overall = F)

kable(x, format = "latex", booktabs = TRUE)
t1kable(x, format = "latex")

# Missing data problem
t2 <- table1::table1( ~ Y_Warm.Muslims |
                    Wave * As, data = dat_bayes, overall = F)

# data prep

# create new data set
dt_five_prep <- dat_bayes %>%
  group_by(Id) %>%
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
  mutate(
    Y_Warm.Asians = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12",
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
        Wave == "Time12",
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
        Wave == "Time12",
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
      Wave == "Time12",
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
        Wave == "Time12",
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
        Wave == "Time12",
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
      Wave == "Time12",
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
        Wave == "Time12",
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
        Wave == "Time12",
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
  ungroup() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}Z")) %>%
  arrange(Id, Wave)

# check
length(unique(dt_five_prep$Id))

head(dt_five_prep)
colnames(dt_five_prep)

str(dt_five_prep)
hist(dt_five_prep$Age_cZ)

head(dt_five_prep)



# bind data
dt_five_bind <-  dat_bayes %>%
  bind_rows(dt_five_prep) %>%
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
  data = dt_five_bind,
  overall = F
)


#  data wrangle


# link 5 dfs for zero estimate -----------------------------------------

# five data
head()

dt_five_zero_noimpute_temp <- dt_five_bind |>
  filter((As == 0 & YearMeasured != -1)) |>
  # filter(Wave != "Time11" & Wave != "Time12") |>
  mutate(wave = as.numeric(Wave)) |>
  arrange(Id, Wave)

dt_five_zero_noimpute <- dt_five_zero_noimpute_temp

dt_five_zero_noimpute$wave  =  dt_five_zero_noimpute_temp$wave - 3

table(dt_five_zero_noimpute$wave)

## no impute but with the years



# one data
dt_five_one_noimpute_temp <- dt_five_bind |>
  filter((As == 1 & Wave == "Time10") |
           (As == 1 & Wave == "Time11") |
           (As == 1 & Wave == "Time12")) %>%
  mutate(wave = as.numeric(Wave)) |>
  arrange(Id, Wave)

dt_five_one_noimpute <- dt_five_one_noimpute_temp
dt_five_one_noimpute$Wave
dt_five_one_noimpute$wave  <- dt_five_one_noimpute_temp$wave - 3

table(dt_five_one_noimpute$wave) # Correct



# Check missing
library(naniar)
naniar::gg_miss_var(dt_five_one_noimpute)
naniar::gg_miss_var(dt_five_one_noimpute)

# naniar::vis_miss(dt_five_one_noimpute,
#                  warn_large_data = FALSE)


## save data
saveRDS(dt_five_zero_noimpute, here::here(push_mods,"dt_five_zero_noimpute-attacks"))

saveRDS(dt_five_one_noimpute, here::here(push_mods,"dt_five_one_noimpute-attacks"))



dt_five_zero_noimpute <- readRDS( here::here(push_mods,"dt_five_zero_noimpute-attacks"))
dt_five_one_noimpute <- readRDS( here::here(push_mods,"dt_five_one_noimpute-attacks"))


# bayes models ------------------------------------------------------------


prior = c(
  set_prior('normal(0, 1)', class = "b", coef = "wave"),
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
    Y_Warm.Muslims | mi()  ~ wave +
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
      EthCat_cZ  +
      NZDep.2013_cZ +
      NZSEI13_cZ  +
      Parent_cZ  +
      Partner_cZ +
      Pol.Orient_cZ +
      Religious_cZ +
      SDO_cZ +
      RWA_cZ +
      Urban_cZ + (1 | Id)
  )

m_0 <- brm(
  backend = "cmdstanr",
  data = dt_five_zero_noimpute,
  family = "gaussian",
  bform_mus,
  prior = prior,
  init = 0,
  file = here::here(push_mods,"five-zero-MUS-attacks.rds")
)


m_1 <- brm(
  backend = "cmdstanr",
  data = dt_five_one_noimpute,
  family = "gaussian",
  bform_mus,
  prior = prior,
  init = 0,
  file = here::here(push_mods,"five-zero-MUS-attacks.rds")
)




# overweight --------------------------------------------------------------


bform_overweight <-
  bf(
    Y_Warm.Overweight | mi()  ~ wave +
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
      EthCat_cZ  +
      NZDep.2013_cZ +
      NZSEI13_cZ  +
      Parent_cZ  +
      Partner_cZ +
      Pol.Orient_cZ +
      Religious_cZ +
      SDO_cZ +
      RWA_cZ +
      Urban_cZ + (1 | Id)
  )

m_3 <- brm(
  backend = "cmdstanr",
  data = dt_five_zero_noimpute,
  family = "gaussian",
  bform_overweight,
  prior = prior,
  init = 0,
  file = here::here(push_mods,"five-zero-OVERWEIGHT-attacks.rds")
)


m_4 <- brm(
  backend = "cmdstanr",
  data = dt_five_one_noimpute,
  family = "gaussian",
  bform_overweight,
  prior = prior,
  init = 0,
  file = here::here(push_mods,"five-zero-MUS-attacks.rds")
)


