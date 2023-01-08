# Load cohort data
# revision conducted on 24 Nov 2022.
# joseph.bulbulia


# set digits
options(scipen = 999)

#libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# for saving models (bulbulia only - use own paths for simulated data)

# set paths for JB
# ** YOU WILL NEED TO SET PATHS TO YOUR OWN COMPUTER**
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/attacks/mods")

push_figs <-
  fs::path_expand(" /Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/attacks/figs")

pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# jb's workflow
# dat <- arrow::read_parquet(pull_path)


# These data are synthetic, and only for the purposes of understanding the code.  The data described in this study are part of the New Zealand Attitudes and Values Study
# (NZAVS). Full copies of the NZAVS data files are held by all members of the NZAVS management
# team and advisory board. A de-identified dataset containing the variables analysed in this
# manuscript is available upon request from the corresponding author, or any member of the
# NZAVS advisory board for the purposes of replication or checking of any published study using
# NZAVS data.

# for details, go to https://www.psych.auckland.ac.nz/en/about/new-zealand-attitudes-and-values-study.html



# note the synthetic dataset is about 20% the size of the full NZAVS dataset
dat <- arrow::read_parquet(here::here("data", "dat_synth"))
str(dat)



#dat <- dat %>% mutate_if(is.matrix, as.vector) |> mutate_if(is.factor, as.numeric) |>  mutate_if(is.double, as.integer) |>
 # data.frame()


# select waves from t3 to t13
nrow(dat_bayes)
dat_bayes <- dat |>
  arrange(Id, Wave) |>
  mutate(Male = ifelse(GendAll == 1, 1, 0)) |>
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
    #   Warm.Asians,
    #   Warm.Chinese,
    #Warm.Disabled, # begins wave12
    Warm.Elderly,
    # begins wave 10
    #   Warm.Immigrants,
    #  Warm.Indians,
    #  Warm.Maori,
    Warm.MentalIllness,
    #begins wave 9
    Warm.Muslims,
    #    Warm.NZEuro,
    Warm.Overweight,
    #    Warm.Pacific,
    RaceRejAnx,
    #   Warm.Refugees, begins wave9
    TSCORE,
    YearMeasured,
    COVID19.Timeline
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
  ) |>
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
                                    YearMeasured == 1, 1, 0)) |>
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
  mutate(pre_postCOVID = ifelse (COVID19.Timeline >  1.1, 1, 0)) |>
  arrange(Id, Wave)   %>%
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
          ifelse(YearMeasured == 0 &
                    Wave == 2021,   TSCORE_b + 1824,
                  TSCORE)
        )
      )
    )
  )) |>
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
    #   Y_Warm.Asians = Warm.Asians,
    #  Y_Warm.Chinese = Warm.Chinese,
    # Warm.Disabled, only in wave12
    Y_Warm.Elderly = Warm.Elderly,
    #  Y_Warm.Immigrants = Warm.Immigrants,
    #  Y_Warm.Indians = Warm.Indians,
    #  Y_Warm.Maori = Warm.Maori,
    Y_Warm.MentalIllness = Warm.MentalIllness,
    # not in 8
    Y_Warm.Muslims = Warm.Muslims,
    # Y_Warm.NZEuro = Warm.NZEuro,
    Y_Warm.Overweight = Warm.Overweight,
    #  Y_Warm.Pacific = Warm.Pacific,
    #  Y_Warm.Refugees = Warm.Refugees,
    As = Attack
  ) %>%
  dplyr::mutate(Warm.Muslims_b = ifelse(Wave == "2016", (Warm.Muslims), NA_real_)) %>%
  fill(Warm.Muslims_b, .direction = "downup") %>%
  dplyr::mutate(Warm.Overweight_c = ifelse(Wave == "2017", (Warm.Overweight), NA_real_)) %>%
  fill(Warm.Overweight_c, .direction = "downup") %>%
  dplyr::mutate(Warm.Elderly_c = ifelse(Wave == "2017", (Warm.Elderly), NA_real_)) %>%
  fill(Warm.Elderly_c, .direction = "downup") %>%
  dplyr::mutate(Warm.MentalIllness_c = ifelse(Wave == "2017", (Warm.MentalIllness), NA_real_)) %>%
  fill(Warm.MentalIllness_c, .direction = "downup") %>%
  dplyr::mutate(Warm.Overweight_b = ifelse(Wave == "2016", (Warm.Overweight), NA_real_)) %>%
  fill(Warm.Overweight_b, .direction = "downup") %>%
  dplyr::mutate(Warm.Muslims_c = ifelse(Wave == "2017", (Warm.Muslims), NA_real_)) %>%
  fill(Warm.Muslims_c, .direction = "downup") %>%
  dplyr::mutate(Warm.Overweight_c = ifelse(Wave == "2017", (Warm.Overweight), NA_real_)) %>%
  fill(Warm.Overweight_c, .direction = "downup") %>%
  dplyr::mutate(SampleOriginYear_b = ifelse(Wave == "2016", (SampleOriginYear - 1), NA_real_)) %>%
  fill(SampleOriginYear_b, .direction = "downup") %>%
  dplyr::mutate(REGC_2022_c = ifelse(Wave == "2017", as.numeric(REGC_2022), NA_real_)) %>%
  fill(REGC_2022_c, .direction = "downup") %>%
  dplyr::mutate(Rural_GCH2018_c = ifelse(Wave == "2017", as.numeric(Rural_GCH2018), NA_real_)) %>%
  fill(Rural_GCH2018_c, .direction = "downup") |>
  dplyr::mutate(Age_c = ifelse(Wave == "2017", (Age), NA_real_)) %>%
  fill(Age_c, .direction = "downup") %>%
  dplyr::mutate(HLTH.BMI_c = ifelse(Wave == "2017", (HLTH.BMI), NA_real_)) %>%
  fill(HLTH.BMI_c, .direction = "downup") %>%
  dplyr::mutate(CONSCIENTIOUSNESS_c = ifelse(Wave == "2017", (CONSCIENTIOUSNESS), NA_real_)) %>%
  fill(CONSCIENTIOUSNESS_c,  .direction = "downup") %>%
  dplyr::mutate(OPENNESS_c = ifelse(Wave == "2017", (OPENNESS), NA_real_)) %>%
  fill(OPENNESS_c,  .direction = "downup") |>
  dplyr::mutate(HONESTY_HUMILITY_c = ifelse(Wave == "2017", (HONESTY_HUMILITY), NA_real_)) %>%
  fill(HONESTY_HUMILITY_c,  .direction = "downup") |>
  dplyr::mutate(EXTRAVERSION_c = ifelse(Wave == "2017", (EXTRAVERSION), NA_real_)) %>%
  fill(EXTRAVERSION_c,  .direction = "downup") |>
  dplyr::mutate(NEUROTICISM_c = ifelse(Wave == "2017", (NEUROTICISM), NA_real_)) %>%
  fill(NEUROTICISM_c,  .direction = "downup") |>
  dplyr::mutate(AGREEABLENESS_c = ifelse(Wave == "2017", (AGREEABLENESS), NA_real_)) %>%
  fill(AGREEABLENESS_c,  .direction = "downup") |>
  dplyr::mutate(Male_c = ifelse(Wave == "2017", as.numeric(Male), NA_real_)) %>%
  fill(Male_c,  .direction = "downup") %>%
  dplyr::mutate(NZDep2013_c = ifelse(Wave == "2017", as.numeric(NZDep2013), NA_real_)) %>%
  fill(NZDep2013_c,  .direction = "downup")  %>%
  dplyr::mutate(RaceRejAnx_c = ifelse(Wave == "2017", as.numeric(RaceRejAnx), NA_real_)) %>%
  fill(RaceRejAnx_c,  .direction = "downup")  %>%
  dplyr::mutate(EthCat_c = ifelse(Wave == "2017", as.numeric(EthCat), NA_real_)) %>%
  fill(EthCat_c,  .direction = "downup") %>%
  dplyr::mutate(BornNZ_c = ifelse(Wave == "2017", as.numeric(BornNZ), NA_real_)) %>%
  fill(BornNZ_c,  .direction = "downup")  %>%
  dplyr::mutate(Pol.Orient_c = ifelse(Wave == "2017", (Pol.Orient), NA_real_)) %>%
  fill(Pol.Orient_c,  .direction = "downup") %>%
  dplyr::mutate(Relid_c = ifelse(Wave == "2017", as.numeric(Relid), NA_real_)) %>%
  fill(Relid_c,  .direction = "downup") %>%
  dplyr::mutate(Partner_c = ifelse(Wave == "2017", (as.numeric(Partner)), NA_real_)) %>%
  fill(Partner_c,  .direction = "downup") %>%
  dplyr::mutate(Parent_c = ifelse(Wave == "2017", (as.numeric(Parent)), NA_real_)) %>%
  fill(Parent_c,  .direction = "downup") %>%
  dplyr::mutate(Employed_c = ifelse(Wave == "2017", (as.numeric(Employed)), NA_real_)) %>%
  fill(Employed_c,  .direction = "downup") %>%
  dplyr::mutate(Edu_c = ifelse(Wave == "2017", (Edu), NA_real_)) %>%
  fill(Edu_c,  .direction = "downup") %>%
  dplyr::mutate(SDO_c = ifelse(Wave == "2017", (as.numeric(SDO)), NA_real_)) %>%
  fill(SDO_c,  .direction = "downup") %>%
  dplyr::mutate(RWA_c = ifelse(Wave == "2017", (as.numeric(RWA)), NA_real_)) %>%
  fill(RWA_c,  .direction = "downup") %>%
  dplyr::mutate(NZSEI13_c = ifelse(Wave == "2017", (as.numeric(NZSEI13)), NA_real_)) %>%
  fill(NZSEI13_c,  .direction = "downup") |>
  ungroup() |>
dplyr::mutate(EthCat_c = as.factor(EthCat_c)) |>
  dplyr::filter(
    !is.na(Age_c),!is.na(EthCat),!is.na(BornNZ_c),!is.na(Male_c),!is.na(Edu_c),!is.na(Employed_c),!is.na(EthCat_c),!is.na(Parent_c),!is.na(Partner_c),!is.na(Relid_c),!is.na(RaceRejAnx_c),!is.na(Pol.Orient_c),!is.na(NZDep2013_c),
    !is.na(REGC_2022_c),!is.na(NZSEI13_c),!is.na(Rural_GCH2018_c),!is.na(EthCat_c),!is.na(Pol.Orient_c),!is.na(AGREEABLENESS_c),
    !is.na(CONSCIENTIOUSNESS_c),!is.na(OPENNESS_c),!is.na(HONESTY_HUMILITY_c),!is.na(EXTRAVERSION_c),!is.na(NEUROTICISM_c)
  ) |>
  dplyr::mutate(
    Rural_GCH2018_c = as.factor(Rural_GCH2018_c),
    # SampleOriginYear = as.factor(SampleOriginYear),
    # BornTerritorialAuthority =  as.factor(BornTerritorialAuthority),
    REGC_2022_c = as.factor(REGC_2022_c),
    Age_cZ = scale(Age_c),
    HLTH.BMI_cZ = scale(HLTH.BMI_c),
    BornNZ_cZ = scale(BornNZ_c),
    Male_cZ = scale (Male_c),
    Edu_cZ = scale(Edu_c),
    Employed_cZ = scale(Employed_c),
    EthCat_c = EthCat_c,
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
  mutate(Sample = as.factor(ifelse(
    SampleOriginYear < 2,
    0,
    ifelse(
      SampleOriginYear >= 2  &
        SampleOriginYear < 4,
      1,
      ifelse(SampleOriginYear == 4, 2, 3)
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


# save data
arrow::write_parquet(dat_bayes, here::here(push_mods, "2012_cohort_attacks"))
