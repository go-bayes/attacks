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





# check urban variable ----------------------------------------------------
#
#
# table1::table1(~ Urban | Wave, data = dat, transpose = TRUE)
#
# dat_f <- dat|>
#   dplyr::filter(YearMeasured == 1) |>
#   dplyr::select(Urban, Wave, Id)
#
# table1::table1(~ as.factor(Urban) | Wave, data = dat_f, transpose = TRUE )
#
#
# msm::statetable.msm(Urban, Id, data = dat_f)
#
# df<- dat %>%
#   as.data.frame() |>
#   filter(YearMeasured == 1) |>
#   filter(Wave != 2009 & Wave != 2010 & Wave !=2011) |>
#   dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
#                                     YearMeasured == 1, 1, 0)) %>%
#   # dplyr::mutate(org2017 =  ifelse(Wave == 2017 &
#   #                                   YearMeasured == 1, 1, 0)) %>%
#   # dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
#   #                                   YearMeasured == 1, 1, 0)) %>%
#   group_by(Id) %>%
#   dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
#   dplyr::filter(hold > 0) %>%
#   arrange(Wave, Id) |>
#   select(Wave, Urban, Id) |>
#   group_by(Id) |>
#   mutate(lag_urban = dplyr::lag(Urban)) |>
#   mutate( state = ifelse( Urban == lag_urban, "stable", "change")) |>
#   ungroup()
#
# head(df)
#
#
# table1::table1( ~state | Wave, data = df,  transpose = TRUE)
#
# glm(data = df,  state ~ Wave, family = "binomial")
#
# family


# amelia ------------------------------------------------------------------
colnames(dt_five_zero_noimpute2)

df_zero <- dt_five_zero_noimpute2 |>
  select(-c(hold, TSCORE_b, TSCORE_i, dys, yrs, org2016, TSCORE))
colnames(df_zero)


#match("Ys", names(bind_zero1))  # for obtaining bounds for Muslim outcome

#bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

# cannot get dys/years easily, use wave
am_zero <- amelia(
  set.seed = 1234,
  df_zero,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  #ords = "Ys",  #*** NOTE THAT IN ORIGINAL ANLYSIS WE USE ORDS
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "Urban"),
  # yrs not working
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
 # bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(df_zero)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0, here::here("mods", "imputed0"))

#52965 Total N
length(unique(imputed0$imputations$imp1$Id))
head(imputed0$imputations$imp1$Id)

# inspect
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0$imputations$imp10,
               overall = FALSE)


# impute Attack condition = 1s ---------------------------------------------------------------

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
table1::table1(~ Ys | Wave * As, data = km_one, overall = FALSE)



head(km_one)
dim(km_one)
# make data frame
km_one <- as.data.frame(km_one)


at <-
  table1::table1(~ Ys |
                   As * Wave,
                 data = km_one,
                 overall = F,
                 transpose = F)

t1kable(at, format = "latex")

# create bounds for Ys
head(km_one)

match("Ys", names(km_one))  # for obtaining bounds for Muslim outcome


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

saveRDS(imputed1, here::here("mods", "imputed1"))



# traceplot 1s ------------------------------------------------------------
imputed1 <- readRDS(here::here("mods", "imputed1"))
dev.off()
set.seed(0)
out1 <-
  as.character(sample(imputed1$imputations$imp1$Id, 12, replace = FALSE))
out1


imputed0 <- readRDS(here::here("mods", "imputed0"))
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


table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1$imputations$imp10,
               overall = FALSE)

# make frames compatible --------------------------------------------------
imputed0 <- readRDS(here::here("mods", "imputed0"))
imputed1 <- readRDS(here::here("mods", "imputed1"))


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
  readRDS(here::here("mods", "listbayes"))


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
  path = here::here(here::here("figs")),
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
  path = here::here(here::here("figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)









