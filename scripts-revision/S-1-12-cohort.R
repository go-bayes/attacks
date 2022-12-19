# revision conducted on 24 Nov 2022.
# joseph.bulbulia


# set digits
options(scipen = 999)

#libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# for saving models (bulbulia only - use own paths for simulated data)

# set paths for JB** YOU NEED TO SET YOUR OWN **
push_mods <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/attacks/mods")
push_figs <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/attacks/figs")

pull_path <-
  fs::path_expand(
    "/Users/joseph/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )


# Read data
dat <- arrow::read_parquet(pull_path)



# select waves from t5 to t13

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
    HLTH.BMI,
    REGC_2022,
    Rural_GCH2018,
    SampleOriginYear,
    #  BornTerritorialAuthority, # Not coded.
    REGC_2022,
    Age,
    Male,
    EthCat,
    BornNZ,
    Employed,
    Edu,
    Pol.Orient,
    SDO,
    RWA,
    NZSEI06,
    NZSEI13,
    NZDep2013,
    Relid,
    Partner,
    Parent,
    TSCORE,
    Warm.Asians,
    Warm.Chinese,
    #Warm.Disabled, # begins wave12
    #Warm.Elderly,  # begins wave 10
    Warm.Immigrants,
    Warm.Indians,
    Warm.Maori,
    #  Warm.MentalIllness, begins wave 9
    Warm.Muslims,
    Warm.NZEuro,
    Warm.Overweight,
    Warm.Pacific,
    RaceRejAnx,
    #   Warm.Refugees, begins wave9
    TSCORE,
    YearMeasured
  ) |>
  dplyr::mutate(Employed = as.numeric(Employed)) |>
  dplyr::filter(
    (Wave ==   2012 & YearMeasured == 1) |
      (Wave ==   2013 & YearMeasured == 1) |
      (Wave ==   2014 & YearMeasured == 1) |
      (Wave ==   2015 & YearMeasured == 1) |
      (Wave ==   2016 & YearMeasured == 1) |
      (Wave ==  2017 & YearMeasured != -1) |
      (Wave ==  2018 & YearMeasured != -1) |
      (Wave ==  2019 & YearMeasured != -1) |
      (Wave ==  2020 & YearMeasured != -1) |
      (Wave == 2021 & YearMeasured != -1)
  ) %>%
  droplevels() |>
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  dplyr::mutate(
    lag_warm.muslims = dplyr::lag(Warm.Muslims),
    lag_overweight = dplyr::lag(Warm.Overweight)
  ) |>
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
                                    YearMeasured == 1, 1, 0)) %>%  # low N.  use the Time 5 cohort to double
  dplyr::mutate(org2013 =  ifelse(Wave == 2013 &
                                    YearMeasured == 1, 1, 0)) %>%
  dplyr::mutate(org2014 =  ifelse(Wave == 2014 &
                                    YearMeasured == 1, 1, 0)) %>%
  dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
                                    YearMeasured == 1, 1, 0)) %>%
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
                                    YearMeasured == 1, 1, 0)) %>%
  dplyr::mutate(org2017 =  ifelse(Wave == 2017 &
                                    YearMeasured == 1, 1, 0)) %>%
  # dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
  #                                   YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold2 = mean(org2017, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold2 > 0) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold > 0) %>%
  dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold15 > 0) %>%
  dplyr::mutate(hold14 = mean(org2014, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold14 > 0) %>%
  dplyr::mutate(hold13 = mean(org2013, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold13 > 0) %>%
  dplyr::mutate(hold12 = mean(org2012, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold12 > 0) %>%
  # dplyr::mutate(hold3 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  # dplyr::filter(hold3 > 0) %>%
  ungroup(Id) |>
  dplyr::mutate(Edu = as.numeric(Edu)) |>
  arrange(Id, Wave) %>%
  group_by(Id) |>
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2016", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b,  .direction = "downup") %>%
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
  )) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    (TSCORE_i >= 3545 &
       Wave == 2018) |
      (Wave == 2019 |
         Wave == 2020 |
         Wave == 2021),
    1,
    0
  )))) %>% # All 2019s even if NA need to be 1
  #dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i))) %>%
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
  dplyr::mutate(Warm.Muslims_b = if_else(Wave == "2016", (Warm.Muslims), NA_real_)) %>%
  fill(Warm.Muslims_b, .direction = "downup") %>%
  dplyr::mutate(Warm.Overweight_b = if_else(Wave == "2016", (Warm.Overweight), NA_real_)) %>%
  fill(Warm.Overweight_b, .direction = "downup") %>%
  dplyr::mutate(Warm.Muslims_c = if_else(Wave == "2017", (Warm.Muslims), NA_real_)) %>%
  fill(Warm.Muslims_c, .direction = "downup") %>%
  dplyr::mutate(Warm.Overweight_c = if_else(Wave == "2017", (Warm.Overweight), NA_real_)) %>%
  fill(Warm.Overweight_c, .direction = "downup") %>%
  dplyr::mutate(SampleOriginYear_b = if_else(Wave == "2016", (SampleOriginYear - 1), NA_real_)) %>%
  fill(SampleOriginYear_b, .direction = "downup") %>%
  dplyr::mutate(REGC_2022_c = if_else(Wave == "2017", as.numeric(REGC_2022), NA_real_)) %>%
  fill(REGC_2022_c, .direction = "downup") %>%
  dplyr::mutate(Rural_GCH2018_c = if_else(Wave == "2017", as.numeric(Rural_GCH2018), NA_real_)) %>%
  fill(Rural_GCH2018_c, .direction = "downup") |>
  dplyr::mutate(Age_c = if_else(Wave == "2017", (Age), NA_real_)) %>%
  fill(Age_c, .direction = "downup") %>%
  dplyr::mutate(HLTH.BMI_c = if_else(Wave == "2017", (HLTH.BMI), NA_real_)) %>%
  fill(HLTH.BMI_c, .direction = "downup") %>%
  dplyr::mutate(CONSCIENTIOUSNESS_c = if_else(Wave == "2017", (CONSCIENTIOUSNESS), NA_real_)) %>%
  fill(CONSCIENTIOUSNESS_c,  .direction = "downup") %>%
  dplyr::mutate(OPENNESS_c = if_else(Wave == "2017", (OPENNESS), NA_real_)) %>%
  fill(OPENNESS_c,  .direction = "downup") |>
  dplyr::mutate(HONESTY_HUMILITY_c = if_else(Wave == "2017", (HONESTY_HUMILITY), NA_real_)) %>%
  fill(HONESTY_HUMILITY_c,  .direction = "downup") |>
  dplyr::mutate(EXTRAVERSION_c = if_else(Wave == "2017", (EXTRAVERSION), NA_real_)) %>%
  fill(EXTRAVERSION_c,  .direction = "downup") |>
  dplyr::mutate(NEUROTICISM_c = if_else(Wave == "2017", (NEUROTICISM), NA_real_)) %>%
  fill(NEUROTICISM_c,  .direction = "downup") |>
  dplyr::mutate(AGREEABLENESS_c = if_else(Wave == "2017", (AGREEABLENESS), NA_real_)) %>%
  fill(AGREEABLENESS_c,  .direction = "downup") |>
  dplyr::mutate(Male_c = if_else(Wave == "2017", as.numeric(Male), NA_real_)) %>%
  fill(Male_c,  .direction = "downup") %>%
  dplyr::mutate(NZDep2013_c = if_else(Wave == "2017", as.numeric(NZDep2013), NA_real_)) %>%
  fill(NZDep2013_c,  .direction = "downup")  %>%
  dplyr::mutate(RaceRejAnx_c = if_else(Wave == "2017", as.numeric(RaceRejAnx), NA_real_)) %>%
  fill(RaceRejAnx_c,  .direction = "downup")  %>%
  dplyr::mutate(EthCat_c = if_else(Wave == "2017", as.numeric(EthCat), NA_real_)) %>%
  fill(EthCat_c,  .direction = "downup") %>%
  dplyr::mutate(BornNZ_c = if_else(Wave == "2017", as.numeric(BornNZ), NA_real_)) %>%
  fill(BornNZ_c,  .direction = "downup")  %>%
  dplyr::mutate(Pol.Orient_c = if_else(Wave == "2017", (Pol.Orient), NA_real_)) %>%
  fill(Pol.Orient_c,  .direction = "downup") %>%
  dplyr::mutate(Relid_c = if_else(Wave == "2017", as.numeric(Relid), NA_real_)) %>%
  fill(Relid_c,  .direction = "downup") %>%
  dplyr::mutate(Partner_c = if_else(Wave == "2017", (as.numeric(Partner)), NA_real_)) %>%
  fill(Partner_c,  .direction = "downup") %>%
  dplyr::mutate(Parent_c = if_else(Wave == "2017", (as.numeric(Parent)), NA_real_)) %>%
  fill(Parent_c,  .direction = "downup") %>%
  dplyr::mutate(Employed_c = if_else(Wave == "2017", (as.numeric(Employed)), NA_real_)) %>%
  fill(Employed_c,  .direction = "downup") %>%
  dplyr::mutate(Edu_c = if_else(Wave == "2017", (Edu), NA_real_)) %>%
  fill(Edu_c,  .direction = "downup") %>%
  dplyr::mutate(SDO_c = if_else(Wave == "2017", (as.numeric(SDO)), NA_real_)) %>%
  fill(SDO_c,  .direction = "downup") %>%
  dplyr::mutate(RWA_c = if_else(Wave == "2017", (as.numeric(RWA)), NA_real_)) %>%
  fill(RWA_c,  .direction = "downup") %>%
  dplyr::mutate(NZSEI13_c = if_else(Wave == "2017", (as.numeric(NZSEI13)), NA_real_)) %>%
  fill(NZSEI13_c,  .direction = "downup") |>
  ungroup() %>%
  # select(
  #   -c(
  #     Employed,
  #     Urban,
  #     Edu,
  #     HLTH.BMI,
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
    !is.na(Relid_c),
    !is.na(RaceRejAnx_c),
    !is.na(Pol.Orient_c),
    !is.na(NZDep2013_c),
    #  !is.na(REGC_2022_c),
    !is.na(NZSEI13_c),
    !is.na(Rural_GCH2018_c),
    !is.na(EthCat_c),
    !is.na(Pol.Orient_c),
    !is.na(AGREEABLENESS_c),
    !is.na(CONSCIENTIOUSNESS_c),
    !is.na(OPENNESS_c),
    !is.na(HONESTY_HUMILITY_c),
    !is.na(EXTRAVERSION_c),
    !is.na(NEUROTICISM_c)
  ) |>
  dplyr::mutate(
    Rural_GCH2018_c = as.factor(Rural_GCH2018_c),
    #SampleOriginYear = as.factor(SampleOriginYear),
    #    BornTerritorialAuthority =  as.factor(BornTerritorialAuthority),
    REGC_2022_c = as.factor(REGC_2022_c),
    Age_cZ = scale(Age_c),
    HLTH.BMI_cZ = scale(HLTH.BMI_c),
    BornNZ_cZ = scale(BornNZ_c),
    Male_cZ = scale (Male_c),
    Edu_cZ = scale(Edu_c),
    Employed_cZ = scale(Employed_c),
    # EthCat_c = EthCat_c,
    Parent_cZ = scale(Parent_c),
    Partner_cZ = scale(Partner_c),
    Relid_cZ = scale(Relid_c),
    RaceRejAnx_cZ = scale(RaceRejAnx_c),
    Pol.Orient_cZ = scale(Pol.Orient_c),
    # Urban_cZ = scale(Urban_c),
    SDO_cZ = scale(SDO_c),
    RWA_cZ = scale(RWA_c),
    NZDep2013_cZ = scale(NZDep2013_c),
    NZSEI13_cZ = scale(NZSEI13_c),
    AGREEABLENESS_cZ = scale(AGREEABLENESS_c),
    CONSCIENTIOUSNESS_cZ = scale(CONSCIENTIOUSNESS_c),
    OPENNESS_cZ = scale(OPENNESS_c),
    HONESTY_HUMILITY_cZ = scale(HONESTY_HUMILITY_c),
    EXTRAVERSION_cZ = scale(EXTRAVERSION_c),
    NEUROTICISM_cZ = scale(NEUROTICISM_c)
  ) %>%
  droplevels() |>
  mutate(Sample = as.factor(if_else(
    SampleOriginYear < 2,
    0,
    if_else(
      SampleOriginYear >= 2  &
        SampleOriginYear < 4,
      1,
      if_else(SampleOriginYear == 4, 2, 3)
    )
  ))) |>
  droplevels() |>
  dplyr::arrange(Id, Wave)



#"Time4"
table(dat_bayes$Sample)
# relabel wave
levels(dat_bayes$Wave) <-
  c(
    "Time4",
    "Time5",
    "Time6",
    "Time7",
    "Time8",
    "Time9",
    "Time10",
    "Time11",
    "Time12",
    "Time13"
  )

table(dat_bayes$Sample)

length(unique(dat_bayes$Id)) #5117
table(dat_bayes$Sample)

# 4893 at baseline
table1(
  ~ Sample + Warm.Muslims + lag_warm.muslims + Male + Age + EthCat +
    REGC_2022  + Rural_GCH2018 +  EthCat + Edu_cZ + Pol.Orient_cZ +  NZDep2013 + NZSEI13  +  as.numeric(Id) |
    Wave,
  data = dat_bayes
)

summary(lm (
  Warm.Muslims ~ as.numeric(Wave) * (
    Sample + Age + Male + EthCat_c + REGC_2022 + Rural_GCH2018 +
      Edu_cZ + Pol.Orient_cZ +  NZDep2013 + NZSEI13
  ),
  data = dat_bayes
))

length(unique(dat_bayes$Id)) #

dat_bayes[, c("lag_warm.muslims", "Warm.Muslims")]

# image
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = dat_bayes)


# check
x <- table1::table1( ~ Y_Warm.Muslims  |
                       factor(Wave) * factor(As),
                     data = dat_bayes,
                     overall = F)
x

kable(x, format = "latex", booktabs = TRUE)
t1kable(x, format = "latex")

# Missing data problem
t2 <- table1::table1( ~ Y_Warm.Muslims |
                        Wave * as.factor(As),
                      data = dat_bayes,
                      overall = F)

# data prep

# create new data set
dt_prep <- dat_bayes %>%
  group_by(Id) %>%
  mutate(As = (
    ifelse(
      Wave == "Time10" & Attack == 1 | Wave == "Time11" |
        Wave == "Time12" | Wave == "Time13",
      0,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4",
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
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4",
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
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4",
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
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4",
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
        Wave == "Time8" |
        Wave == "Time7" |
        Wave == "Time6" |
        Wave == "Time5" |
        Wave == "Time4",
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
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4",
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
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4",
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
        Wave == "Time8" |
        Wave == "Time7" |
        Wave == "Time6" |
        Wave == "Time5" |
        Wave == "Time4",
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
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4",
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
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4",
        NA,
        Warm.Pacific
      )
    )
  ) |>
  ungroup() %>%
  arrange(Id, Wave)

# check
length(unique(dt_prep$Id))


skimr::skim(dt_prep) %>%
  arrange(n_missing)

head(dt_prep)
colnames(dt_prep)

str(dt_prep)
hist(dt_prep$Age_cZ)


head(dt_prep)



# bind data
dt_bind <-  dat_bayes %>%
  bind_rows(dt_prep) %>%
  arrange(Id, Wave)


# Test NAs = Correct
table1::table1(
  ~ Y_Warm.Muslims +
    # Y_Warm.Chinese +
    # # Warm.Disabled, only in wave12
    # #  Y_Warm.Elderly +
    # Y_Warm.Immigrants +
    # Y_Warm.Indians +
    # Y_Warm.Maori +
    # #   Y_Warm.MentalIllness +  # not in 8
    # Y_Warm.Muslims +
    # Y_Warm.NZEuro +
    Y_Warm.Overweight #+
  #  Y_Warm.Pacific# +
  #   Y_Warm.Refugees
  | Wave * as.factor(As),
  data = dt_bind,
  overall = F
)


#  data wrangle
dt_bind
dt_bind$YearMeasured
# link dfs for zero estimate -----------------------------------------

# five data
head()

dt_temp <- dt_bind |>
  dplyr::filter((As == 0 & YearMeasured != -1)) |>
  # filter(Wave != "Time11" & Wave != "Time12") |>
  mutate(wave = as.numeric(Wave)) |>
  arrange(Id, Wave)

dt_ni <- dt_temp

max(dt_ni$wave)

dt_ni$wave  =  dt_temp$wave - 7

table(dt_ni$wave)

## no impute but with the years



# one data
dt_temp1 <- dt_bind |>
  dplyr::filter((As == 1 & Wave == "Time10") |
                  (As == 1 & Wave == "Time11") |
                  (As == 1 & Wave == "Time12") |
                  (As == 1 & Wave == "Time13")
  ) %>%
  mutate(wave = as.numeric(Wave)) |>
  arrange(Id, Wave) |>
  droplevels()

dt_ni1 <- dt_temp1
dt_ni1$Wave
dt_ni1$wave  <- dt_temp1$wave - 7

table(dt_ni1$wave) # Correct


str(dt_ni1)
dev.off()
# Check missing
library(naniar)
naniar::gg_miss_var(dt_ni)
naniar::gg_miss_var(dt_ni1)

dt_ni1$CONSCIENTIOUSNESS_cZ
## save data

arrow::write_parquet(dt_ni,
                     here::here(push_mods, "dt_ni-attacks-2012-6pre.rds"))
arrow::write_parquet(dt_ni1,
                     here::here(push_mods, "dt_ni1-attacks-2012-6pre.rds"))




# read-prepared-data ------------------------------------------------------

dt_ni <-
  arrow::read_parquet(here::here(push_mods, "dt_ni-attacks-2012-6pre.rds"))
dt_ni1 <-
  arrow::read_parquet(here::here(push_mods, "dt_ni1-attacks-2012-6pre.rds"))

# check
table(dt_ni$wave)
table(dt_ni1$wave)

# bayes models ------------------------------------------------------------



prior = c(
  set_prior('normal(0, 1)', class = "b"),
  set_prior(
    "student_t(3, 4, 1)",
    class = "Intercept",
    lb = 1,
    ub = 7
  )
)



## impute muslim
# ensure parrallel computations -------------------------------------------
# for future work, this might be helpful: https://discourse.mc-stan.org/t/hierarchical-brms-with-splines-limiting-prediction-below-threshold/28200

library("brms")
library("rstan")

rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
theme_set(theme_pubclean()) # nice theme
library(cmdstanr)





bform_mus_impute_1 <-
  bf(
    Y_Warm.Muslims | mi()  ~ wave * (
      Pol.Orient_cZ  +
        AGREEABLENESS_cZ +
        CONSCIENTIOUSNESS_cZ +
        EXTRAVERSION_cZ +
        HONESTY_HUMILITY_cZ +
        OPENNESS_cZ +
        NEUROTICISM_cZ +
        Age_cZ +
        BornNZ_c +
        Edu_cZ +
        Employed_c +
        EthCat_c +
        Male_c +
        NZDep2013_cZ +
        NZSEI13_cZ +
        Parent_c +
        Partner_c +
        RaceRejAnx_cZ +
        Relid_cZ+
        REGC_2022_c +
        Rural_GCH2018_c
    ) + (1 + wave|Id)

  )

table(is.na(dt_ni$AGREEABLENESS_cZ))




m_0a <- brm(
  backend = "cmdstanr",
  data = dt_ni,
  family = "gaussian",
  bform_mus_impute_1,
  prior = prior,
  init = 0,
  file =  here::here(  push_mods, "impute-2012-zero-MUS-attacks-use-interaction-2012-6all.rds" )
)


summary(m_0a)
summary(m_1a)


m_1a <- brm(
  backend = "cmdstanr",
  data = dt_ni1,
  family = "gaussian",
  bform_mus_impute_1,
  prior = prior,
  init = 0,
  file = here::here(
    push_mods,
    "impute-2012-one-MUS-attacks-use-interaction-2012-6all.rds"
  )
)
summary(m_1a)
4896
4865

table(is.na(dt_ni$AGREEABLENESS_cZ))







# preliminary-inspection-imputations --------------------------------------



p0a <-
  plot(ggeffects::ggpredict(m_0a, terms = c("wave [0:3]"))) + scale_y_continuous(limits = c(3, 5))
p1b <-
  plot(ggeffects::ggpredict(m_1a, terms = c("wave [0:3]"))) + scale_y_continuous(limits = c(3, 5))

p0a
p0a + p1b



# save-imputations --------------------------------------------------------
name_error = "sd"

# set N for id counts
id_0 <- m_0a$data$Id
id_1 <- m_1a$data$Id

standata(m_0a)

length(m_0a$data$Id)

# analysis
name <- "yfit_muslim"

fitted_values_0 <- predict(m_0a, ndraws = 100)
fitted_values_0


fitted_values_1 <- predict(m_1a,  ndraws = 100)


# make df
fitted_values_0 <- data.frame(fitted_values_0)
head(fitted_values_0)
head(fitted_values_0)
mean(fitted_values_0$Est.Error)

nrow(fitted_values_0)

# needs to be df
yfit <- as.data.frame(fitted_values_0$Estimate)
sd <- as.data.frame(fitted_values_0$Est.Error)
# rename
colnames(yfit) <- name
colnames(sd) <- name_error

length(id_0)



# data frame
dat_0 <-
  as.data.frame(cbind(Y_orig = standata(m_0a)$Y, standata(m_0a)$X, yfit, id_0, sd)) |>
  mutate(id = as.factor(id_0)) |>
  arrange(id, wave)

dat_0 <- dat_0 |>
  mutate(yfit_ORD = round(yfit_muslim, digits = 0)) |>
  mutate(as = as.factor(rep(0, nrow(dat_0)))) |>
  select(-id_0)


dat_0_wide = dat_0 |>
  mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                  yfit_muslim,
                                  Y_orig)) |>
  mutate(yimpute_muslim_lag = dplyr::lag(yimpute_muslim))


head(dat_0_wide$yimpute_muslim_lag)

table(dat_0$wave)
## Same for 1s

# make df
fitted_values_1 <- data.frame(fitted_values_1)

# needs to be df
yfit1 <- as.data.frame(fitted_values_1$Estimate)
sd <- as.data.frame(fitted_values_1$Est.Error)

# rename
colnames(yfit1) <- name
colnames(sd) <- name_error

# data frame
dat_1 <-
  as.data.frame(cbind(Y_orig = standata(m_1a)$Y, standata(m_1a)$X, yfit1, id_1, sd)) |>
  mutate(id = as.factor(id_1))

dat_1 <- dat_1 |>
  mutate(yfit_ORD = round(yfit_muslim, digits = 0)) |>
  mutate(as = as.factor(rep(1, nrow(dat_1)))) |>
  select(-id_1)

# combine data

dat_combined <- rbind(dat_0, dat_1) |>
  filter(wave == 0 | wave == 1 | wave == 2 | wave == 3) |>
  mutate(Wave = as.factor(wave),
         Condition = as)

str(dat_combined)

# # save processed data
saveRDS(dat_combined,
        here::here(push_mods, "g-comp-processed-muslims-attack-2012-6all"))

# # read processed data
# dat_combined_muslim <-
#   readRDS(here::here(push_mods, "g-comp-processed-muslims-attack"))
# table(dat_combined_muslim$wave)
#
# # read processed data
# head(dat_combined_muslim)

# wrangle for imputed values with errors
dat_combined_imputed_muslim_2012 <- dat_combined  |>
  mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                  yfit_muslim,
                                  Y_orig)) |>
  group_by(id) |>
  mutate(se = if_else(Y_orig != Inf, 0, sd)) |>
  ungroup() |>
  mutate(se = if_else(se <= 0, .01, se)) |>
  mutate(Wave = as.factor(Wave)) |>
  data.frame()

head(dat_combined_imputed_muslim_2012)

#save imputed values
arrow::write_parquet(
  dat_combined_imputed_muslim_2012,
  here::here(push_mods, "dat_combined_imputed_muslim-attack-2012-6all")
)



dat_combined_imputed_muslim_2012 <-
  arrow::read_parquet(here::here(push_mods, "dat_combined_imputed_muslim-attack-2012-6all"))



# prepare data -------------------------------------------------------------

d_muslim <- dat_combined_imputed_muslim_2012 |>
  mutate(se = if_else(se <= 0, .01, se)) |>
  mutate(Wave = as.factor(Wave)) |>
  mutate(Attack = as.factor(as))


arrow::write_parquet(d_muslim, here::here(push_mods, "d_muslim-6all"))


# READ DATA ---------------------------------------------------------------


# READ DATA
d_muslim <- arrow::read_parquet(here::here(push_mods, "d_muslim-6all"))


head(d_muslim)
# not used
#d_muslim_o <- panel_data(d_muslim, id = id, wave = Wave)

# gee ---------------------------------------------------------------------
library(geepack)



# MODEL Summaries ---------------------------------------------------------


models <- list(
  "GEE: imputed values outcome" = geeglm(
    data = d_muslim,
    formula = yimpute_muslim ~  Attack * Wave * Pol.Orient_cZ,
    id = id,
    wave = wave,
    corstr = "ar1"
  ),
  "GEE: ordinal outcome" = geeglm(
    data = d_muslim,
    formula = yfit_ORD ~  Attack * Wave * Pol.Orient_cZ,
    id = id,
    wave = wave,
    corstr = "ar1"
  ),
  "GEE: fitted values outcome" = geeglm(
    data = d_muslim,
    formula = yfit_muslim ~  Attack * Wave * Pol.Orient_cZ,
    id = id,
    wave = wave,
    corstr = "ar1"

  )

)


# format latex plain
options("modelsummary_format_numeric_latex" = "plain")

m_gee <- modelsummary::modelsummary(models,
                                    gof_omit = "^(?!.*[{conf.low}, {conf.high}])",
                                    statistic  = NULL, # "conf.int",
                                    estimate =  "{estimate} [{conf.low}, {conf.high}]",
                                    #  standardize = "posthoc",
                                  #  output = "latex",
                                    title = "Generalised Estimating Equations: comparison of imputations approaches")


m_gee


p_gee <- modelsummary::modelplot( models, coef_omit = 'Interc') + scale_color_okabe_ito()

p_gee

ggsave(
  p_gee,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "gee_coef_rev-6all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# GEE graphs --------------------------------------------------------------

g_mod <- geeglm(
  data = d_muslim,
 formula = yfit_ORD ~  Attack * Wave * Pol.Orient_cZ,
# formula = yfit_muslim ~  Attack * Wave * Pol.Orient_cZ,
#  formula = yimpute_muslim ~  Attack * Wave * Pol.Orient_cZ,
  #  yfit_muslim
  id = id,
  wave = wave,
  corstr = "ar1"
)

#
# plot_cap(
#   g_mod,
#   condition = c("Wave",
#                 "Attack",
#                 Pol.Orient_cZ = range),
#  # vcov =  FALSE,
# ) +  scale_color_okabe_ito()


plot ( ggeffects::ggpredict(g_mod, terms = c("Wave", "Attack", "Pol.Orient_cZ[-1,0,1]"))  )


plot_cco(
  g_mod,
  effect = "Attack",
  condition = list("Wave",
                   Pol.Orient_cZ = "minmax"),
  conf_level = 0.95,
  transform_pre = "difference",
  draw = TRUE
)



plot_cco(
  g_mod,
  effect = "Attack",
  # condition = c("Wave",
  #               "Pol.Orient_cZ"),
  condition = list("Wave",
                 Pol.Orient_cZ = "minmax"),
  conf_level = 0.95,
  transform_pre = "difference",
  draw = FALSE
)|>
 select(-id) |>
 mutate(condition2 = factor(condition2, labels = c("pol_conserve_low", "pol_conserve_high")))|>
 kbl(format = "markdown", booktabs = TRUE)
dev.off()


gee_mar_eff <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = c("Wave"),
  conf_level = 0.95,
  transform_pre = "difference",
) + scale_y_continuous(limits = c(-.02,.4)) +
  labs(title = "Counterfactual contrasts in effect magnitude by N =7,824",
       subtitle = "Contrast of attack effect by wave shows diminishing effects",
       y = "Conterfactual contrasts, difference scale") +
  scale_color_okabe_ito()




gee_mar_eff <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = c("Wave"),
  conf_level = 0.95,
  transform_pre = "difference",
) + scale_y_continuous(limits = c(-.02,.4)) +
  labs(title = "Counterfactual contrasts in effect magnitude by N =7,824",
       subtitle = "Contrast of attack effect by wave shows diminishing effects",
       y = "Conterfactual contrasts, difference scale") +
  scale_color_okabe_ito()


# difference scale
gee_con_eff <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = list("Wave",
                   Pol.Orient_cZ = range),
                 #  Pol.Orient_cZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference"
) +scale_y_continuous(limits = c(-.05,.5)) +
  labs(title = "Counterfactual contrasts in effect magnitude by N =7,824",
       subtitle = "Effect modification by political orientation (liberal/conservative, standardised units)",
       y = "Conterfactual contrasts, difference scale") +
  scale_color_okabe_ito()

gee_con_eff
gee_plots <- gee_mar_eff + gee_con_eff + plot_annotation(tag_levels = "A")

gee_plots

ggsave(
  gee_plots,
  path = here::here(here::here("figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gee_results_rev-6all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

#
# # This is good
# plot_cco(
#   model_gee_muslim,
#   effect = "Attack",
#   condition = c("Wave", "Pol.Orient_cZ"),
#   conf_level = 0.95,
#   transform_pre = "difference"
# ) +
#   labs(title = "Counterfactual contrasts in effect magnitude by N = 7,727",
#        subtitle = "Effect modification by political orientation\n(liberal/conservative, standardised units)",
#        y = "Conterfactual contrasts, difference scale") +
#   scale_color_okabe_ito()


ggsave(
  p_gee,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "gee_coef_rev-6all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# BAYES -------------------------------------------------------------------


prior_mus_cond  = c(
  set_prior("normal(0,1)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior(
    "student_t(3, 4, 2)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")  # only for raneffs
)


# ord
bform_mus_cond_ord  =   bf(yfit_ORD ~  Attack  *  Wave *  Pol.Orient_cZ + (1 | id),
                           sigma ~ 0 + as,
                           set_rescor(rescor = FALSE))


system.time(
  m_cond_mus <- brms::brm(
    backend = "cmdstanr",
    data = d_muslim,
    family = "gaussian",
    bform_mus_cond_ord,
    prior_mus_cond,
    init = 0,
    file = here::here(push_mods, "m_cond_mus-2012-use-ord-rev6.rds")
  )
)



summary(m_cond_mus)
# GRAPHS ------------------------------------------------------------------


options("modelsummary_format_numeric_latex" = "plain")

m_bayes_latex <- modelsummary::modelsummary(
  m_cond_mus,
  gof_omit = "^(?!.*[{conf.low}, {conf.high}])",
  statistic  = NULL,
  # "conf.int",
  ndraws = 100,
  estimate =  "{estimate} [{conf.low}, {conf.high}]",
  #  standardize = "posthoc",
  output = "latex",
  title = "Bayesian Multi-Level Model: 2012 Cohort"
)


# save table
saveRDS(m_bayes_latex,
        here::here(push_mods, "m_bayes-attacks-2012"))

saveRDS(m_bayes_latex,
        here::here(push_mods, "m_bayes_latex-attacks-2012"))



# posterior checks
pp_check(m_cond_mus)
plot(m_cond_mus)
summary(m_cond_mus)



# another table
parms <- model_parameters(m_cond_mus,   test = "pd")

parms |>
  kbl(format = "latex", booktabs = TRUE)






# BAYES-GRAPHS ------------------------------------------------------------




pred_draws <- predictions(
  m_cond_mus,
  newdata = datagrid("Attack" = c(0, 1),
                     "Wave" = c(0, 1, 2, 3)),
  ndraws = 100,
  re_formula = NA
)



pred_draws <- posteriordraws(pred_draws)

head(pred_draws)


pred_mar <- predictions(
  m_cond_mus,
  conf_level = 0.95,
  type = "response",
  newdata = datagrid("Attack" = 0:1,
                     "Wave" = 0:3),
  ndraws = 1000,
  re_formula = NA
) |>
  posteriordraws()

pred_cond <- predictions(
  m_cond_mus,
  conf_level = 0.95,
  type = "response",
  newdata = datagrid(
    "Attack" = 0:1,
    "Wave" = 0:3,
    "Pol.Orient_cZ" = c(-1.91,-1, 0, 1, 2.46)
  ),
  ndraws = 1000,
  re_formula = NA
) |>
  posteriordraws()



library(ggdist)

pl_a12<- ggplot(pred_mar, aes(x = Wave,
                             y = draw,
                             fill = Attack))  + # + scale_y_continuous(limits = c(3, 5.2)) +
  stat_halfeye(slab_alpha = .8) +
  labs(title = "Predicted Muslim Warmth by condition and wave (Bayesian)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N =7,824",
       y = "Warmth to Mulsims (1-7 (ordinal)")  +
  scale_fill_okabe_ito() + scale_y_continuous(limits = c(3.8, 4.5))

# check
pl_a12



ggsave(
  pl_a12,
  path = here::here(here::here("figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "bayes_ord_pred-12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# plot # use
pl_b12 <- ggplot(pred_cond, aes(
  x = Wave,
  y = draw,
  fill = Attack
))  + scale_y_continuous(limits = c(3, 5.2)) +
  stat_halfeye(slab_alpha = .8) +
  labs(title = "Predicted Muslim Warmth effect modification by political conservativism (Bayesian)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N =7,824",
       y = "Warmth to Mulsims (1-7 (ordinal)") +
  #  scale_y_continuous(limits = c(3.0,5.2)) +
  facet_grid(. ~ Pol.Orient_cZ,   shrink = T) +
  scale_fill_okabe_ito(alpha = 1)

pl_b12





# bayes marg

bayes_mar_eff12 <- plot_cco(
  m_cond_mus,
  effect = "Attack",
  condition = c("Wave"),
  conf_level = 0.95,
  transform_pre = "difference",
) +
  #  scale_y_continuous(limits = c(-.05, .41)) +
  labs(title = "Marginal contrasts (Bayesian) ",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N =7,824",
       y = "Difference in Warmth to Muslims") +
  scale_color_okabe_ito()

# difference scale
bayes_con_eff12 <- plot_cco(
  m_cond_mus,
  effect = "Attack",
  condition = list("Wave",
                   #    Pol.Orient_cZ = "fivenum"),
                   Pol.Orient_cZ = c(-1.91,-1, 0, 1, 2.46)),
  #  Pol.Orient_cZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  scale_y_continuous(limits = c(-.05, .41)) +
  labs(title = "Conditional contrasts by political conservativism (Bayesian)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N =7,824",
       y = "Difference in Warmth to Muslims") +
  scale_color_okabe_ito()

bayes_mar_eff12
bayes_con_eff12


# graph model
bayes_412 <-
  (pl_a12 + bayes_mar_eff12) / (pl_b12 + bayes_con_eff12) + plot_annotation(tag_levels = "A")

bayes_marg_2panel12 <-
  (pl_a12 / bayes_mar_eff12)  + plot_annotation(tag_levels = "A")

bayes_marg_2panel12


bayes_con_2panel12 <-
  (pl_b12 / bayes_con_eff12)  + plot_annotation(tag_levels = "A")

bayes_con_2panel12

ggsave(
  bayes_412,
  path = here::here(here::here("figs")),
  width = 16,
  height = 12,
  units = "in",
  filename = "bayes_412.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)




ggsave(
  bayes_marg_2panel12,
  path = here::here(here::here("figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "bayes_marg_2panel12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


ggsave(
  bayes_con_2panel12,
  path = here::here(here::here("figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "bayes_con_2panel12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


