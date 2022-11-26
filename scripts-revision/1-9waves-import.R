# 9 waves


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


table1::table1(~Warm.Overweight |Wave, data = dat)

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
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                   YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  dplyr::mutate(hold1 = mean(org2013, na.rm = TRUE)) %>%  # Hack
  filter(hold1 > 0) %>% # hack to enable repeate of baseline in 2019
  dplyr::mutate(hold2 = mean(org2014, na.rm = TRUE)) %>%  # Hack
  filter(hold2 > 0) %>% # hack to enable repeate of baseline in 2019
  dplyr::mutate(hold3 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  filter(hold4 > 0) %>% # hack to enable repeate of baseline in 2019
  dplyr::mutate(hold4 = mean(org2016, na.rm = TRUE)) %>%  # Hack
  filter(hold4 > 0) %>% # hack to enable repeate of baseline in 2019
  dplyr::mutate(hold5 = mean(org2017, na.rm = TRUE)) %>%  # Hack
  filter(hold5 > 0) %>% # hack to enable repeate of baseline in 2019
  dplyr::mutate(hold6 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold6 > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  # dplyr::mutate(TSCORE_b = ifelse(Wave == "2012", (TSCORE), NA_real_)) %>%
  # fill(TSCORE_b) %>%
  # dplyr::mutate(TSCORE_i = ifelse(
  #   YearMeasured == 0 & Wave == 2013,
  #   TSCORE_b + 365,
  #   ifelse(
  #     YearMeasured == 0 & Wave == 2014,
  #     TSCORE_b + 730,
  #     ifelse(
  #       YearMeasured == 0 & Wave == 2015,
  #       TSCORE_b + 1094,
  #       # leap
  #       ifelse(
  #         YearMeasured == 0 & Wave == 2016,
  #         TSCORE_b + 1459,
  #         ifelse(YearMeasured == 0 &
  #                  Wave == 2017, TSCORE_b + 1824, TSCORE)
  #       )
  #     )
  #   )
  # )) %>%
  # dplyr::mutate(Attack = as.numeric(ifelse(TSCORE_i >= 3545, 1, 0))) %>% # All 0
  # group_by(Id) %>%
  # dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i)),
  #               yrs = dys / 365) %>%
  # dplyr::mutate(Ys = Warm.Muslims,
  #               As = Attack) %>%
  # dplyr::mutate(yrs =  (dys / 365)) %>%
  # dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
  # droplevels() %>%
  # dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
  #                                   YearMeasured == 1, 1, 0)) %>%
  # group_by(Id) %>%
  # dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  # filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  # ungroup(Id) %>%
  # dplyr::mutate(Edu = as.numeric(Edu)) %>%
  # arrange(Wave, Id) %>%
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
  dplyr::mutate(Gender3_c = if_else(Wave == "2012", (as.numeric(Gender_3)) / 2, NA_real_)) %>%
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

# This works.
table1::table1(
  ~  Warm.Muslims
  |
    factor(Attack) * Wave ,
  data = test_temp,
  #$dt_raw_9,
  overall = FALSE,
  transpose = F
)

dt_raw_9 <- dt_raw |>
  dplyr::mutate(Gender3 = as.factor(ifelse(
    GendAll == 0,
    "Female",
    if_else(GendAll == 1, "Male", "GenderDiverse")
  )))  |>
  dplyr::select(
    Id,
    Wave,
    # CONSCIENTIOUSNESS,
    # OPENNESS,
    # HONESTY_HUMILITY,
    # EXTRAVERSION,
    # NEUROTICISM,
    # AGREEABLENESS,
    Age,
    Gender3,
    EthCat,
    Employed,
    Household.INC,
    BornNZ,
    #  ChildrenNum,
    #  BELONG,
    # SUPPORT,
    Urban,
    Edu,
    Pol.Orient,
    SDO,
    RWA,
    NZSEI13,
    NZdep,
    Religious,
    # GenCohort,
    Urban,
    TSCORE,
    Partner,
    Parent,
    # LIFESAT,
    # SFHEALTH,
    # HLTH.BMI,
    # Your.Future.Security,
    # Your.Personal.Relationships,
    # Your.Health,
    # Standard.Living,
    # CharityDonate,
    # HoursCharity,
    Hours.Work,
    # HLTH.SleepHours,
    # HLTH.Disability,
    # Hours.Exercise,
    Warm.Asians,
    Warm.Chinese,
    # Warm.Disabled, only in wave12
    #    Warm.Elderly,
    # not in 8
    Warm.Immigrants,
    Warm.Indians,
    Warm.Maori,
    #  Warm.MentalIllness,
    # not in 8
    Warm.Muslims,
    Warm.NZEuro,
    Warm.Overweight,
    Warm.Pacific,
    #   Warm.Refugees,
    # not in 8
    TSCORE,
    YearMeasured
  ) %>%
  dplyr::mutate(Employed = as.numeric(Employed)) |>
  dplyr::filter(
    Wave ==   2012 |
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
  dplyr::mutate(org_hold =  ifelse(Wave == 2012 &
                                     YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org_hold, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) |>
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
  dplyr::mutate(As = Attack) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
  dplyr::mutate(
    Y_Warm.Asians = Warm.Asians,
    Y_Warm.Chinese = Warm.Chinese,
    # Warm.Disabled, only in wave12
    #  Y_Warm.Elderly = Warm.Elderly,
    Y_Warm.Immigrants = Warm.Immigrants,
    Y_Warm.Indians = Warm.Indians,
    Y_Warm.Maori = Warm.Maori,
    #  Y_Warm.MentalIllness = Warm.MentalIllness,
    # not in 8
    Y_Warm.Muslims = Warm.Muslims,
    Y_Warm.NZEuro = Warm.NZEuro,
    Y_Warm.Overweight = Warm.Overweight,
    Y_Warm.Pacific = Warm.Pacific,
    #   Y_Warm.Refugees = Warm.Refugees,
    As = Attack
  ) %>%
  # dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) |>
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  group_by(Id) |>
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2012", (TSCORE), NA_real_)) %>%
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
  group_by(Id) %>%
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
  group_by(Id) %>% # need to fill this way
  dplyr::mutate(Age_c = if_else(Wave == "2012", (Age), NA_real_)) %>%
  fill(Age_c) %>%
  dplyr::mutate(Gender3_c = if_else(Wave == "2012", as.numeric(Gender3), NA_real_)) %>%
  fill(Gender3_c) %>%
  dplyr::mutate(EthCat_c = if_else(Wave == "2012", as.numeric(EthCat), NA_real_)) %>%
  fill(EthCat_c) %>%
  # dplyr::mutate(BornNZ_c = if_else(Wave == "2012", as.numeric(BornNZ), NA_real_)) %>%
  # fill(BornNZ_c) %>%
  dplyr::mutate(Pol.Orient_c = if_else(Wave == "2012", (Pol.Orient), NA_real_)) %>%
  fill(Pol.Orient_c) %>%
  dplyr::mutate(Religious_c = if_else(Wave == "2012", as.numeric(Religious), NA_real_)) %>%
  fill(Religious_c) %>%
  dplyr::mutate(Partner_c = if_else(Wave == "2012", (as.numeric(Partner)), NA_real_)) %>%
  fill(Partner_c) %>%
  dplyr::mutate(Parent_c = if_else(Wave == "2012", (as.numeric(Parent)), NA_real_)) %>%
  fill(Parent_c) %>%
  # dplyr::mutate(NZdep_c = if_else(Wave == "2012", (NZdep), NA_real_)) %>%
  # fill(NZdep_c) %>%
  dplyr::mutate(Gender3_c = if_else(Wave == "2012", (as.numeric(Gender3)) / 2, NA_real_)) %>%
  fill(Gender3_c) %>%
  dplyr::mutate(Employed_c = if_else(Wave == "2012", (as.numeric(Employed)), NA_real_)) %>%
  fill(Employed_c) %>%
  dplyr::mutate(Edu_c = if_else(Wave == "2012", (Edu), NA_real_)) %>%
  fill(Edu_c) %>%
  dplyr::mutate(Urban_c = if_else(Wave == "2012", (as.numeric(Urban)), NA_real_)) %>%
  fill(Urban_c) %>%
  dplyr::mutate(SDO_c = if_else(Wave == "2012", (as.numeric(SDO)), NA_real_)) %>%
  fill(SDO_c) %>%
  dplyr::mutate(RWA_c = if_else(Wave == "2012", (as.numeric(RWA)), NA_real_)) %>%
  fill(RWA_c) %>%
  # dplyr::mutate(NZSEI13_c = if_else(Wave == "2012", (as.numeric(NZSEI13)), NA_real_)) %>%
  # fill(NZSEI13_c) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) |>
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
      NZdep,
      Age,
      Religious,
      Partner,
      Parent,
      hold,
      Age,
      Gender3,
      EthCat,
      BornNZ,
      TSCORE,
      # hold3,
      # hold2,
      #  org2017,
      org2012
      #  org2018
    )
  ) |>
  dplyr::mutate(Gender3_c = as.factor(Gender3_c),
                EthCat_c = as.factor(EthCat_c))

|>
  dplyr::filter(
    !is.na(Age_c)
    ! is.na(Gender3_c),
    !is.na(Edu_c),
    !is.na(Employed_c),
    !is.na(EthCat_c),
    !is.na(Parent_c),
    !is.na(Partner_c),
    !is.na(Religious_c),
    !is.na(Pol.Orient_c),
    !is.na(Urban_c),
    !is.na(SDO_c),
    !is.na(RWA_c)#,
    # !is.na(NZSEI13_c)
  ) |>
  ungroup() |>
  arrange(Id, Wave)

levels(dt_raw_9$Wave) <-
  c("Time4" ,
    "Time5",
    "Time6",
    "Time7",
    "Time8",
    "Time9",
    "Time10",
    "Time11",
    "Time12")

dt_raw_9
length(unique(dt_raw_9$Id)) #12165

table1::table1(
  ~ Warm.Asians + Warm.Muslims + Warm.Chinese +
    Warm.Immigrants + Warm.Indians +
    Warm.Maori + Warm.Muslims +
    Warm.NZEuro + Warm.Overweight +
    Warm.Pacific
  |
    factor(Attack) * Wave ,
  data = test_temp,
  #$dt_raw_9,
  overall = FALSE,
  transpose = F
)


# tw<-km_all5%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check

time10 <- dt_raw_9 |>
  filter(Wave == "Time10")

table1::table1(
  ~ Warm.Asians + Warm.Muslims + Warm.Chinese +
    Warm.Immigrants + Warm.Indians +
    Warm.Maori + Warm.Muslims +
    Warm.NZEuro + Warm.Overweight +
    Warm.Pacific
  |
    factor(As) ,
  data = time10,
  overall = FALSE,
  transpose = TRUE
)

# correct
table1::table1(~ Warm.Muslims |
                 Wave ,
               data = dt_raw_9,
               overall = FALSE)

# latex summary
kable(x, format = "latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~ Warm.Muslims |
                 Wave * as.factor(Attack),
               data = dt_raw_9,
               overall = FALSE)

obtl <-
  table1::table1(~ Warm.Muslims |
                   as.factor(Attack) * Wave,
                 data = dt_raw_9,
                 overall = FALSE)


kable(obtl, format = "latex", booktabs = TRUE)

x <- table1::table1(
  ~  Age + Edu + Employed +
    EthCat + Gender3 + NZdep +
    Parent + Partner + Relid +
    Pol.Orient  + Urban | Wave,
  data = dt_raw_9,
  overall = FALSE
)
x
t1kable(x, format = "latex")


#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# tw<-km_all5%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check

# don't use these
table1::table1(
  ~ Warm.Refugees + Warm.MentalIllness
  + Warm.Elderly |
    Wave,
  data = dt_raw_9,
  overall = FALSE
)



# # latex summary
# kable(x, format = "latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~ Warm.Muslims |
                 Wave * as.factor(Attack),
               data = dt_raw_9,
               overall = FALSE)


# kable(obtl, format = "latex", booktabs = TRUE)

x <- table1::table1(
  ~  Age + Edu + Employed +
    EthCat  + Gender3 + NZdep +
    Parent + Partner + Relid +
    Pol.Orient  + Urban + Y_Warm.Muslims | Wave,
  data = dt_raw_9,
  overall = FALSE
)



# create new data set
dt_prep_9 <- dt_raw_9 %>%
  group_by(Id) %>%
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
    Y_Warm.Asians = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12",
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
    Y_Warm.Chinese = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12",
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
  #         Wave == "Time8" |
  #         Wave == "Time7" |
#         Wave == "Time6" |
#         Wave == "Time5" |
#         Wave == "Time4",
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
        Wave == "Time12",
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
        Wave == "Time12",
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
  #         Wave == "Time8" |
  #         Wave == "Time7" |
#         Wave == "Time6" |
#         Wave == "Time5" |
#         Wave == "Time4",
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
        Wave == "Time12",
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
    Y_Warm.Pacific = ifelse(
      Wave == "Time10" & Attack == 1 |
        Wave == "Time11" |
        Wave == "Time12",
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
  ) %>%
  # mutate(
  #   Y_Warm.Refugees = ifelse(
  #     Wave == "Time10" & Attack == 1 |
  #       Wave == "Time11" |
  #       Wave == "Time12",
  #     NA,
  #     ifelse(
  #       Wave == "Time10" & Attack == 0 |
  #         Wave == "Time9" |
  #         Wave == "Time8" |
  #         Wave == "Time7" |
#         Wave == "Time6" |
#         Wave == "Time5" |
#         Wave == "Time4",
#       NA,
#       Warm.Refugees
#     )
#   )
# ) %>%
ungroup() %>%
  select(-c(# not in 8
    YearMeasured)) |>
  ungroup() %>%
  arrange(Id, Wave)


#
# Warm.Asians,
# Warm.Chinese,
# # Warm.Disabled, only in wave12
# Warm.Elderly, # not in 8
# Warm.Immigrants,
# Warm.Indians,
# Warm.Maori,
# Warm.MentalIllness, # not in 8
# Warm.Muslims,
# Warm.NZEuro,
# Warm.Overweight,
# Warm.Pacific,
# Warm.Refugees,# not in 8
# YearMeasured

# dat_all N
length(unique(dt_prep_9$Id))
# correct

dt_raw$CONSCIENTIOUSNESS
# Missing data problem
t2 <-
  table1::table1( ~ Y_Warm.Muslims |
                    Wave * as.factor(As),
                  data = dt_prep_9,
                  overall = F)
t2

t1kable(t2, format = "latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")
#dt_prep_9
dt_bind_9 <-  dt_raw_9 %>%
  bind_rows(dt_prep_9) %>%
  arrange(Id, Wave) %>%
  # mutate(As = as.factor(As)) |>
  dplyr::select(
    -c(
      Warm.Asians,
      Warm.Chinese,
      # Warm.Disabled, only in wave12
      #  Warm.Elderly,
      # not in 8
      Warm.Immigrants,
      Warm.Indians,
      Warm.Maori,
      #  Warm.MentalIllness,
      # not in 8
      Warm.Muslims,
      Warm.NZEuro,
      Warm.Overweight,
      Warm.Pacific,
      #   Warm.Refugees,
      YearMeasured,
      TSCORE_i,
      TSCORE,
      Attack
    )
  )


# Test NAs = Correct
table1::table1(
  ~ Y_Warm.Muslims +
    Y_Warm.Chinese +
    # Warm.Disabled, only in wave12
    # Y_Warm.Elderly +
    Y_Warm.Immigrants +
    Y_Warm.Indians +
    Y_Warm.Maori +
    #  Y_Warm.MentalIllness +  # not in 8
    Y_Warm.Muslims +
    Y_Warm.NZEuro +
    Y_Warm.Overweight +
    Y_Warm.Pacific #+
  #   Y_Warm.Refugees
  |
    Wave * as.factor(As),
  data = dt_bind_9,
  overall = F
)

#dt_five_bind


# link dfs for zero estimate -----------------------------------------

dt_zero_noimpute_9 <- dt_bind_9 |>
  filter((As == 0)) |>
  arrange(Id, Wave)

str(dt_zero_noimpute_9$As)
head(dt_zero_noimpute_9)
dim(dt_zero_noimpute_9)
summary(dt_zero_noimpute_9$wave) # Correct

summary(dt_zero_noimpute_9)


# check
table1::table1(~ Y_Warm.Asians |
                 Wave * As, data = dt_zero_noimpute_9, overall = FALSE)

# check n missing
# skimr::skim(dt_zero_noimpute_9) |>
#   arrange(n_misssing)

library(naniar)
naniar::gg_miss_var(dt_zero_noimpute_9)
naniar::vis_miss(dt_zero_noimpute_9,
                 warn_large_data = FALSE)

# save data

saveRDS(dt_zero_noimpute_9,
        here::here("data_raw", "dt_zero_noimpute_9.rds"))


## do same for 1s

dt_one_noimpute_9 <- dt_bind_9 |>
  filter((As == 1)) |>
  arrange(Id, Wave)

str(dt_one_noimpute_9$As)
head(dt_one_noimpute_9)
dim(dt_one_noimpute_9)
summary(dt_one_noimpute_9$wave) # Correct


library(naniar)
naniar::gg_miss_var(dt_one_noimpute_9)
naniar::vis_miss(dt_one_noimpute_9,
                 warn_large_data = FALSE)


# check
table1::table1(~ Y_Warm.Asians |
                 Wave * As, data = dt_one_noimpute_9, overall = FALSE)

table(dt_one_noimpute_9$Wave)
# check
# check n missing
skimr::skim(dt_one_noimpute_9)

# save data
saveRDS(dt_one_noimpute_9,
        here::here("data_raw", "dt_one_noimpute_9.rds"))





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
saveRDS(imps_bind5, here::here("mods", "imps_bind5"))

# read
#imps_bind5 <- readRDS(here::here( "mods", "imps_bind5"))

imps_bind5 <-
  readRDS(here::here("mods", "imps_bind5"))

head(imps_bind5)

# make list for bayesian models
listbayes5 <- imps_bind5$imputations$imp

# save list for bayesian models
#saveRDS(listbayes5, here::here( "mods", "listbayes5"))

#readRDS
#listbayes5<- readRDS(here::here( "mods", "listbayes5"))

table((imps_bind5$imputations$imp[[1]]$Ys) == head(imps_bind5$imputations$imp[[2]]$Ys))

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
  path = here::here(here::here("mods")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_5.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)
