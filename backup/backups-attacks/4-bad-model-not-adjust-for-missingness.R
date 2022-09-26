

# packages ----------------------------------------------------------------
source(here::here("R", "libs.R"))


# import data -------------------------------------------------------------
#df <- readRDS(here::here("data_raw", "df"))# orig data

# jittered data for reproduction (NZAVS ethics does not permit original data deposited
# to internet. However, as per note, original data can be obtained, just email c.sibley@auckland.ac.nz or the chair of the U of Auckland ethics committee.

df <- readRDS(here::here("data", "df_s"))

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


# check
length(unique(km_all3$Id))


#table
t13 <-
  table1::table1(~ Warm.Muslims |
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
  path = here::here(here::here("figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "drop__graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


# table for study ----------------------------------
