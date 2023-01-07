source(here::here("R", "libs.R"))




# read data
df <- readRDS(here::here("data", "df_s"))

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
  dplyr::filter(Wave == 2017 |
                  Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2017 =  ifelse(Wave == 2017 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2017, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2017", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b) %>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave == 2018,
    TSCORE_b + 365,
    ifelse(
      YearMeasured == 0 & Wave == 2019,
      TSCORE_b + 730,
      ifelse(YearMeasured == 0 &
               Wave == 2020, TSCORE_b + 1095, TSCORE)
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
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
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
  dplyr::mutate(male_2z = if_else(Wave == "2017", (as.numeric(Male)) / 2, NA_real_)) %>%
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
  ungroup() %>%
  arrange(Id, Wave)

levels(km_all4$Wave) <- c("Time9", "Time10", "Time11", "Time12")


# tw<-km_all4%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check
sum(is.na(km_all4$TSCORE_i))



# correct
table1::table1(~ TSCORE_i | Wave, data = km_all4, overall = FALSE)

# latex summary
kable(x, format = "latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~ Warm.Muslims |
                 Wave * as.factor(Attack),
               data = km_all4,
               overall = FALSE)

obtl <-
  table1::table1(~ Warm.Muslims |
                   as.factor(Attack) * Wave,
                 data = km_all4,
                 overall = FALSE)


kable(obtl, format = "latex", booktabs = TRUE)

x <- table1::table1(
  ~  Age + Edu + Employed + EthnicCats  +
    Male + NZdep + Parent + Partner +
    Religious + Pol.Orient  + Urban |
    Wave,
  data = km_all4,
  overall = FALSE
)
x

t1kable(x, format = "latex")

# create new data set
library(tidyverse)
kf4  <- km_all4 %>%
  group_by(Id) %>%  # All ZEROS
  # filter(Wave != "Time9")%>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (
    ifelse(
      Wave == "Time10" & Attack == 1 | Wave == "Time11" |
        Wave == "Time12",
      0,
      ifelse(Wave == "Time10" &
               Attack == 0 | Wave == "Time9", 1, Attack)
    )
  )) %>%
  mutate(Ys = ifelse(
    Wave == "Time10" & Attack == 1 | Wave == "Time11" |
      Wave == "Time12",
    NA,
    ifelse(Wave == "Time10" &
             Attack == 0 | Wave == "Time9", NA, Warm.Muslims)
  )) %>%
  ungroup() %>%
  arrange(Id, Wave)

# dat_all N
length(unique(kf4$Id))
str(kf4$As)
# correct


# Test NAs = Correct
table1::table1(~ Ys |
                 Wave * as.factor(As), data = kf4, overall = F)



## Bind - double dataset to creat missing values
ka4 <- km_all4 %>%
  bind_rows(kf4) %>%
  arrange(Id, Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka4$Ys)
# Check

# Missing data problem
t2 <- table1::table1( ~ Ys | Wave * As, data = ka4, overall = F)
t2

t1kable(t2, format = "latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link 4 dfs for zero estimate -----------------------------------------

km_zero4 <- ka4 %>%
  filter((As == 0)) %>%
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
  ) %>%
  arrange(Wave, Id)

str(km_zero4$As)
head(km_zero4)
dim(km_zero4)
summary(km_zero4$wave)
summary(km_zero4$Wave)

summary(km_zero4$dys)

# works
table1::table1(~ Ys | Wave * As, data = km_zero4, overall = FALSE)

# correct
table1::table1(~ Ys | Wave * As, data = km_zero4, overall = FALSE)


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

imputed0_4 <- amelia(
  set.seed = 1234,
  bind_zero4,
  cs = c("Id"),
  ts = c("wave"),
  # use days
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero4)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_4, here::here("mods", "imputed0_4"))


# check means, looks good!

table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_4$imputations$imp10,
               overall = FALSE)


# impute 4 1s ---------------------------------------------------------------

km_one4 <- ka4 %>%
  filter((As == 1 & Wave != "Time9")) %>%
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
  ) %>%
  mutate(wave = wave - min(wave),
         dys = dys - min(dys)) %>%
  arrange(Wave, Id)

# check
length(unique(km_one4$Id))
summary(km_one4$wave)
summary(km_one4$dys)
hist(km_one4)


#check looks good
table1::table1(~ Ys | Wave * As, data = km_one4, overall = FALSE)

# make data frame
km_one4 <- as.data.frame(km_one4)

# create bounds for Ys
head(km_one4)

bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1_4 <- amelia(
  set.seed = 1234,
  km_one4,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(km_one4)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1_4, here::here("mods", "imputed1_4"))

# check means, looks good

table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_4$imputations$imp10,
               overall = FALSE)




# 4 make frames compatible --------------------------------------------------
imputed0_4 <-
  readRDS(here::here("mods", "imputed0_4"))
imputed1_4 <-
  readRDS(here::here("mods", "imputed1_4"))




# bind data frames --------------------------------------------------------
# levels_old <- c("Time4","Time5","Time6","Time7","Time8","Time9",
#                 "Time10","Time11","Time12")
# newlevels = c("Time10","Time11","Time12")

m <- 10
zero4 <- NULL
for (i in 1:m) {
  zero4$imputations$imp[[i]] <- imputed0_4$imputations[[i]] %>%
    dplyr::filter(Wave != "Time9") %>%
    droplevels() %>%
    arrange(Wave, Id)
}

one4 <- NULL

for (i in 1:m) {
  one4$imputations$imp[[i]] <- imputed1_4$imputations[[i]] %>%
    dplyr::filter(Wave != "Time9") %>%
    droplevels() %>%
    arrange(Wave, Id)
}

zero4
m <- 10
imps_bind4 <- NULL
for (i in 1:m) {
  imps_bind4$imputations$imp[[i]] <-
    dplyr::bind_rows(zero4$imputations$imp[[i]],
                     one4$imputations$imp[[i]]) %>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    dplyr::mutate(Wave = as.numeric(Wave) - 1) %>%
    dplyr::mutate(days_n = dys - min(dys)) %>% # new zero
    dplyr::mutate(yrs =  days_n / 365) %>% # new
    droplevels() %>%
    arrange(Wave, Id)
}



# Works
hist(imps_bind4$imputations$imp[[1]]$yrs)
table(imps_bind4$imputations$imp[[1]]$Wave)


# save
saveRDS(imps_bind4, here::here("mods", "imps_bind4"))
#imps_bind4 <- readRDS(here::here( "mods", "imps_bind4"))

# read
imps_bind4 <-
  readRDS(here::here("mods", "imps_bind4"))

# make list for bayesian models
listbayes4 <- imps_bind4$imputations$imp

# save list for bayesian models
saveRDS(listbayes4, here::here("mods", "listbayes4"))

#readRDS
#listbayes4<- readRDS(here::here( "mods", "listbayes4"))


# ML model 4 ----------------------------------------------------------------


# model
m <- 10
model_all4 <- NULL
for (i in 1:m) {
  model_all4$model[[i]] <- lmer(Ys ~ As * Wave + (1 | Id),
                                data = imps_bind4$imputations$imp[[i]])
}

# table
tab <- pool_parameters(model_all4$model)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)



# Kurz model

fitted_lines4 <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all4$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()


==  ==  ==  =


  # Kurz model

  fitted_lines4 <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all4$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()


>  >  >  >  >  >  > 9f5a16fc41e9927370908b86e724d0204590a0ed

plot_4 <- fitted_lines4 %>%
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
                                                                  c(4.0, 4.5)) + <  <  <  <  <  <  < HEAD
labs(subtitle = "Multiple imputation: sample from previous 1 wave prior to attack + 2 waves post-attack",
     y = "Muslim Warmth",
     x = "Years: 2017-2020/21; N = 17014") + scale_colour_okabe_ito(alpha =
                                                                      .5)
==  ==  ==  =
  labs(subtitle = "MI from one previous wave",
       y = "Muslim Warmth",
       x = "Years: 2017-2020/21; N = 17014") +   scale_colour_npg(alpha =
                                                                    .5) +  theme_classic() #+
#scale_colour_okabe_ito(alpha =.5)
>  >  >  >  >  >  > 9f5a16fc41e9927370908b86e724d0204590a0ed
plot_4


ggsave(
  plot_4,
  path = here::here(here::here("figs")),
  width = 12,
  height = 9,
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
  dplyr::filter(Wave == 2016 |
                  Wave == 2017 |
                  Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
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
  ungroup() %>%  # Create TSCORE for when people would have been observed
  group_by(Id) %>%
  dplyr::mutate(dys = (TSCORE_i - min(TSCORE_i))) %>%
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
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
  dplyr::mutate(male_2z = if_else(Wave == "2016", (as.numeric(Male)) / 2, NA_real_)) %>%
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
  ungroup() %>%
  arrange(Id, Wave)

levels(km_all5$Wave) <-
  c("Time8", "Time9", "Time10", "Time11", "Time12")


# tw<-km_all5%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check



# correct
table1::table1(~ Ys | Wave * As, data = km_all5, overall = FALSE)

# latex summary
kable(x, format = "latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~ Warm.Muslims |
                 Wave * as.factor(Attack),
               data = km_all5,
               overall = FALSE)

obtl <-
  table1::table1(~ Warm.Muslims |
                   as.factor(Attack) * Wave,
                 data = km_all5,
                 overall = FALSE)


kable(obtl, format = "latex", booktabs = TRUE)

x <- table1::table1(
  ~  Age + Edu + Employed +
    EthnicCats  + Male + NZdep +
    Parent + Partner + Religious +
    Pol.Orient  + Urban | Wave,
  data = km_all5,
  overall = FALSE
)
x

t1kable(x, format = "latex")

# create new data set
library(tidyverse)
kf5  <- km_all5 %>%
  group_by(Id) %>%  # All ZEROS
  # filter(Wave != "Time9")%>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
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
  mutate(Ys = ifelse(
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
  )) %>%
  ungroup() %>%
  arrange(Id, Wave)

# dat_all N
length(unique(kf5$Id))
str(kf5$As)
# correct


# Test NAs = Correct
table1::table1(~ Ys |
                 Wave * as.factor(As), data = kf5, overall = F)

## Bind - double dataset to create missing values
ka5 <- km_all5 %>%
  bind_rows(kf5) %>%
  arrange(Id, Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

head(ka5$Ys)
# Check

# Missing data problem
t2 <- table1::table1( ~ Ys | Wave * As, data = ka5, overall = F)
t2

t1kable(t2, format = "latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")




# link 5 dfs for zero estimate -----------------------------------------

km_zero5 <- ka5 %>%
  filter((As == 0)) %>%
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
  ) %>%
  arrange(Wave, Id)

str(km_zero5$As)
head(km_zero5)
dim(km_zero5)
summary(km_zero5$wave) # Correct
# works
table1::table1(~ Ys | Wave * As, data = km_zero5, overall = FALSE)

# correct
table1::table1(~ Ys | Wave * As, data = km_zero5, overall = FALSE)


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

imputed0_5 <- amelia(
  set.seed = 1234,
  bind_zero5,
  cs = c("Id"),
  ts = c("wave"),
  # use days
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero5)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_5, here::here("mods", "imputed0_5"))


# check means, looks good

table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_5$imputations$imp10,
               overall = FALSE)


# impute 5 1s ---------------------------------------------------------------

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
  ) %>%
  mutate(dys = dys - min(dys),
         wave = wave - min(wave)) %>%
  arrange(Wave, Id)

summary(km_one5$wave)
summary(km_one5$dys)

length(unique(km_one5$Id))

#check looks good
table1::table1(~ Ys | Wave * As, data = km_one5, overall = FALSE)

# make data frame
km_one5 <- as.data.frame(km_one5)
saveRDS(km_one5, here::here("data_raw", "km_one5"))

# create bounds for Ys
head(km_one5)
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1_5 <- amelia(
  set.seed = 1234,
  km_one5,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(km_one5)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1_5, here::here("mods", "imputed1_5"))

# check means, looks good

table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_5$imputations$imp10,
               overall = FALSE)




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



# bayesian model 2016-2021 ----------------------------------------------------------

## put data in list
imps_bind5$imputations$imp[[1]] %>%
  select(Id, As, Ys, Wave) %>%
  mutate(post_1 = lead(Ys, n = 1)) %>%
  mutate(post_2 = lead(Ys, n = 2)) %>%
  dplyr::filter(Wave == 0) %>%
  rename(baseline = Ys) %>%
  dplyr::select(-Wave) %>%
  head()

mutate(Wave = factor(Wave, labels = c("baseline", "post1", "post2"))) %>%
  pivot_wider(
    id_cols = "Id",
    values_from = c("Ys"),
    names_from = c("As", "Wave")
  ) %>%
  head()

imps_bind5$imputations$imp[[1]] %>%


  # make list
  imp1w <- as.data.frame(imps_bind5$imputations$imp[[1]])
imp2w <- as.data.frame(imps_bind5$imputations$imp[[2]])
imp3w <- as.data.frame(imps_bind5$imputations$imp[[3]])
imp4w <- as.data.frame(imps_bind5$imputations$imp[[4]])
imp5w <- as.data.frame(imps_bind5$imputations$imp[[5]])
imp6w <- as.data.frame(imps_bind5$imputations$imp[[6]])
imp7w <- as.data.frame(imps_bind5$imputations$imp[[7]])
imp8w <- as.data.frame(imps_bind5$imputations$imp[[8]])
imp9w <- as.data.frame(imps_bind5$imputations$imp[[9]])
imp10w <- as.data.frame(imps_bind5$imputations$imp[[10]])




ameliadataw <-
  list(imp1w,
       imp2w,
       imp3w,
       imp4w,
       imp5w,
       imp6w,
       imp7w,
       imp8w,
       imp9w,
       imp10w)




bform =   bf(Ys ~ As  *  Wave + (0 + As || Id),
             sigma ~ 0 + As, set_rescor(rescor = FALSE))

prior_strong_even2 = c(
  set_prior("normal(0, 0.25)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior(
    "student_t(3, 4, 1)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")
)


system.time(
  m_fivewaves  <- brms::brm_multiple(
    bform,
    family = gaussian,
    data = ameliadataw,
    prior = prior_strong_even2,
    #seed = 1234,
    init = 0,
    warmup = 1000,
    iter =  2000,
    chains = 2,
    backend = "cmdstan",
    file = here::here("mods", "m_fivewaves.rds")
  )
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
#   path = here::here(here::here( "figs")),
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
df$Warm.Muslims

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
  dplyr::filter(
    Wave == 2012 |
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
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
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
  dplyr::mutate(Ys = Warm.Muslims,
                As = Attack) %>%
  dplyr::mutate(yrs =  (dys / 365)) %>%
  dplyr::mutate(wave = as.numeric(Wave) - 1) %>%
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
  arrange(Wave, Id)
str(km_all9$Wave)
levels(km_all9$Wave) <-
  c("Time4",
    "Time5",
    "Time6",
    "Time7",
    "Time8",
    "Time9",
    "Time10",
    "Time11",
    "Time12")


# tw<-km_all4%>%
#   select(Id,YearMeasured,Wave,TSCORE,TSCORE_i)%>%
#   filter(is.na(TSCORE_i))
#
# check
sum(is.na(km_all9$TSCORE_i))



# correct
table1::table1(~ Ys | Wave, data = km_all9, overall = FALSE)

# latex summary
kable(x, format = "latex", booktabs = TRUE)

#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")

table1::table1(~ Warm.Muslims |
                 Wave * as.factor(Attack),
               data = km_all9,
               overall = FALSE)

table1::table1(~ Warm.Muslims |
                 Wave * as.factor(As),
               data = km_all9,
               overall = FALSE)

kable(obtl, format = "latex", booktabs = TRUE)


x <-
  table1::table1(
    ~  Age + Edu + Employed + EthnicCats  + Male + NZdep + Parent + Partner + Religious + Pol.Orient  + Urban |
      Wave,
    data = km_all9,
    overall = FALSE
  )
x

t1kable(x, format = "latex")

table1::table1(~  as.factor(Warm.Muslims) |
                 Wave * as.factor(As) , data = km_all9)
km_all9
# create new data set
library(tidyverse)
kf9  <- km_all9 %>%
  group_by(Id) %>%  # All ZEROS
  # filter(Wave != "Time9")%>%
  group_by(Id) %>%  # All ZEROS
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
    Ys = ifelse(
      Wave == "Time9" & Attack == 1 |
        Wave == "Time10" & Attack == 1 |
        Wave == "Time11" | Wave == "Time12",
      NA,
      ifelse(
        Wave == "Time10" & Attack == 0 |
          Wave == "Time9" |
          Wave == "Time8" |
          Wave == "Time7" |
          Wave == "Time6" |
          Wave == "Time5" |
          Wave == "Time4" ,
        NA,
        Warm.Muslims
      )
    )
  ) %>%
  ungroup() %>%
  arrange(Wave, Id)

# dat_all N
length(unique(kf9$Id))
str(kf9$As)
# correct


# Test NAs = Correct
table1::table1(~ (Ys) | Wave * as.factor(As),
               data = kf9,
               overall = F)



## Bind - double dataset to creat missing values
ka9 <- km_all9 %>%
  bind_rows(kf9) %>%
  arrange(Wave, Id) %>%
  mutate(As = as.factor(As)) %>%
  arrange(Wave, Id)
# select(-c(Warm.Muslims,Attack))

head(ka9$Ys)
# Check

# Missing data problem
t2 <-
  table1::table1( ~ Ys |
                    Wave * as.factor(As), data = ka9, overall = F)
t2

t1kable(t2, format = "latex")
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = km_all3) #output = "latex_tabular")



# link 9 dfs for zero estimate -----------------------------------------

km_zero9 <- ka9 %>%
  filter((As == 0)) %>%
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
  ) %>%
  arrange(Wave, Id)

str(km_zero9$As)
head(km_zero9)
dim(km_zero9)
summary(km_zero9$wave)
# works
table1::table1(~ Ys | Wave * As, data = km_zero9, overall = FALSE)

# correct
table1::table1(~ Ys | Wave * As, data = km_zero9, overall = FALSE)


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

imputed0_9 <- amelia(
  set.seed = 1234,
  bind_zero9,
  cs = c("Id"),
  ts = c("wave"),
  # use days
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial, if 3, reverts to pre w10 mean!
  intercs = F,
  # too many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero9)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_9, here::here("mods", "imputed0_9"))

# check means, looks good

table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed0_9$imputations$imp10,
               overall = FALSE)


# impute 9 1s ---------------------------------------------------------------
km_one9 <- ka9 %>%
  dplyr::filter(# (As == 1 & Wave =="Time4")|
    #  (As == 1 & Wave =="Time5")|
    # (As == 1 & Wave =="Time6")|
    # (As == 1 & Wave =="Time7")|
    # (As == 1 & Wave =="Time8")|
    #  (As == 1 & Wave =="Time9")|
    (As == 1 & Wave == "Time10") |
      (As == 1 & Wave == "Time11") |
      (As == 1 & Wave == "Time12")) %>%
  droplevels() %>%
  mutate(wave = wave - 6) %>%
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
  ) %>%
  arrange(Wave, Id)
str(km_one9)
table(ka9$Wave)
summary(km_one9$wave)


#check looks good
table1::table1(~ Ys | Wave * As, data = km_one9, overall = FALSE)

# make data frame
km_one9 <- as.data.frame(km_one9)
km_one9
# create bounds for Ys
head(km_one9)
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed1_9 <- amelia(
  set.seed = 1234,
  km_one9,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial, if 3, reverts to pre w10 mean!
  intercs = F,
  # too many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero9)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed1_9, here::here("mods", "imputed1_9"))


# check means, looks good

table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave * As,
               data = imputed1_9$imputations$imp10,
               overall = FALSE)




# 9 make frames compatible --------------------------------------------------
imputed0_9 <-
  readRDS(here::here("mods", "imputed0_9"))
imputed1_9 <-
  readRDS(here::here("mods", "imputed1_9"))



# bind data frames --------------------------------------------------------
# levels_old <- c("Time4","Time5","Time6","Time7","Time8","Time9",
#                 "Time10","Time11","Time12")
# newlevels = c("Time10","Time11","Time12")

m <- 10
zero9 <- NULL
for (i in 1:m) {
  zero9$imputations$imp[[i]] <- imputed0_9$imputations[[i]] %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    droplevels() %>%
    arrange(Wave, Id)
}

one9 <- NULL

for (i in 1:m) {
  one9$imputations$imp[[i]] <- imputed1_9$imputations[[i]] %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    droplevels() %>%
    arrange(Wave, Id)
}


m <- 10
imps_bind9 <- NULL
for (i in 1:m) {
  imps_bind9$imputations$imp[[i]] <-
    dplyr::bind_rows(zero9$imputations$imp[[i]],
                     one9$imputations$imp[[i]]) %>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    dplyr::mutate(Wave = as.numeric(Wave) - 1) %>%
    dplyr::mutate(days_n = dys - min(dys)) %>% # new zero
    dplyr::mutate(yrs =  days_n / 365) %>% # new yzer
    droplevels() %>%
    arrange(Wave, Id)
}



# Works
hist(imps_bind9$imputations$imp[[1]]$Wave)

# save
saveRDS(imps_bind9, here::here("mods", "imps_bind9"))

# read
imps_bind9 <-
  readRDS(here::here("mods", "imps_bind9"))

# make list for bayesian models
listbayes9 <- imps_bind9$imputations$imp

# save list for bayesian models
saveRDS(listbayes9, here::here("mods", "listbayes9"))

#readRDS
#listbayes9<- readRDS(here::here( "mods", "listbayes9"))


# ML model 9 ----------------------------------------------------------------
# model
m <- 10
model_all9 <- NULL
for (i in 1:m) {
  model_all9$model[[i]] <- lmer(Ys ~ As * Wave + (1 | Id),
                                data = imps_bind9$imputations$imp[[i]])
}

# table
tab <- pool_parameters(model_all9$model)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)

# Uncertainty graph

fitted_lines9 <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all9$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()


plot_9 <- fitted_lines9 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +
  scale_y_continuous(limits =  c(4.0, 4.5)) +
  labs(subtitle = "MI from previous six previous waves",
       y = "Muslim Warmth",
       x = "Waves: 2012-2020/21, N = 11799") +   scale_colour_npg(alpha = .5) + theme_classic() # +
scale_colour_okabe_ito(alpha = .5)


plot_9


ggsave(
  plot_9,
  path = here::here(here::here("figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_9.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)



# SENS sensitivity analysis delete known cases and recover with imputation method ---------------------------------

# link dataframe
library(tidyverse)
km_pre
nw_9 <- nw_9 %>%
  filter(As == 0) %>%
  select(-As)
km_pre

bind_zero_s <- full_join(km_pre, bind_zero5) %>%
  arrange(Wave, Id)

table(bind_zero_s$Wave)
# relevel
levels = c("Time4",
           "Time5",
           "Time6",
           "Time7",
           "Time8",
           "Time9",
           "Time10",
           "Time11",
           "Time12")

bind_zero_s$Wave <- fct_relevel(bind_zero_s$Wave, levels)
levels(bind_zero_s$Wave)
str(bind_zero_s$Wave)
head(bind_zero_s)


bind_zero1_s <- bind_zero_s %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  dplyr::select(-c(dys, yrs)) %>%  # not working
  arrange(Wave, Id) %>%
  group_by(Id) %>%
  mutate(dys = TSCORE_i - min(TSCORE_i)) %>% # Not used
  mutate(yrs = dys / 365) %>% # note used
  ungroup() %>%
  dplyr::filter(
    Wave == "Time4" |
      Wave == "Time5" |
      Wave == "Time6" |
      Wave == "Time7" |
      Wave == "Time8" |
      Wave == "Time9" |
      Wave == "Time10"
  ) %>%
  droplevels()


summary(bind_zero1_s$Wave)

table1::table1( ~ Ys | Wave, data = bind_zero_s)

## Make numeric var

# correct


# now create df with zeros
# check
table1::table1( ~ Ys | Wave, data = bind_zero1_s)

out <- lm(Ys ~ wave, bind_zero1_s)
summary(out)


new_s <- bind_zero1_s


# Replace obeserved values with NA
new_s$Ys[new_s$Wave == "Time9"] <- NA
new_s$Ys[new_s$Wave == "Time10"] <- NA


# check
table1::table1( ~ Ys | Wave, data = bind_zero1_s)
table1::table1( ~ Ys | Wave, data = new_s)

# make data frame'
nw_s <- new_s %>%
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
    yrs
  ) %>%
  arrange(Id, Wave)
nw_s <- as.data.frame(nw_s)

head(nw_s)
match("Ys", names(nw_s))

# create bounds for Ys
bds <- matrix(c(5, 1, 7), nrow = 1, ncol = 3)

imputed_new_s <- amelia(
  set.seed = 1234,
  nw_s,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys"),
  lags = "Ys",
  leads = "Ys",
  polytime = 2,
  # Allow polynomial, if 3,
  intercs = F,
  # too many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(nw_s)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed_new_s, here::here("mods", "imputed_new_s.rds"))
imputed_new_s <- readRDS(here::here("mods", "imputed_new_s.rds"))


# easier for brms
nine_dat_s <- miceadds::datlist2mids(imputed_new_s$imputations)
rm(nine_dat_s)

table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp1,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp2,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp3,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp4,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp5,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp6,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp7,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp8,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp9,
               overall = FALSE)
table1::table1(~ Ys |
                 Wave ,
               data = imputed_new_s$imputations$imp10,
               overall = FALSE)

# original -- VERY CLOSE!!
table1::table1(~ Ys |
                 Wave ,
               data = bind_zero1_s,
               overall = FALSE)



# prepare data
bind_zero1_sz <-
  bind_zero1_s %>% mutate(
    pol_bz = as.numeric(scale(pol_bz)),
    rel_bz = as.numeric(scale(rel_bz)),
    partner_bz = as.numeric(scale(partner_bz)),
    parent_bz = as.numeric(scale(parent_bz)),
    nzdep_bz = as.numeric(scale(nzdep_bz)),
    male_2z = as.numeric(scale(male_2z)),
    employed_bz = as.numeric(scale(employed_bz)),
    edu_bz = as.numeric(scale(edu_bz)) ,
    ubran_bz = as.numeric(scale(ubran_bz)) ,
    EthnicCats_b = as.numeric(factor(EthnicCats_b)),
    GenCohort = as.numeric(factor(GenCohort))
  )



imp_r <-
  brm(
    Ys ~  wave + EthnicCats_b + edu_bz + employed_bz +  pol_bz + male_2z +  nzdep_bz +
      parent_bz + partner_bz +  rel_bz + ubran_bz + GenCohort + (1 |
                                                                   Id),
    data = bind_zero1_sz,
    backend = "cmdstanr",
    file = here::here("mods", "imp_r.rds")
  )


imp_s <-
  brm_multiple(
    Ys ~  wave + (1 | Id),
    data = nine_dat_s,
    backend = "cmdstanr",
    file = here::here("mods", "imp_s.rds")
  )


summary(imp_r)
summary(imp_s)

tab_imp_r <-
  lazerhawk::brms_SummaryTable(imp_r, panderize = F)


# table in latex
tab_imp_r [1:2,] %>%
  kable(booktabs = T,
        "latex",
        caption =  "Sensitivity anlysis: Marginal effect of attack on warmth to Muslims, observed",
        digits = 2) %>%
  print()


tab_imp_s <-
  lazerhawk::brms_SummaryTable(imp_s, panderize = F)


# table in latex
tab_imp_s %>%
  kable(booktabs = T,
        "latex",
        caption =  "Sensitivity anlysis: Marginal effect of attack on warmth to Muslims, imputed",
        digits = 2) %>%
  print()


sens_origA  <- conditional_effects(imp_r,
                                   "wave",
                                   ndraws = 100,
                                   spaghetti = T)#,

plot(sens_origA)[[1]] + scale_color_grey() +
  scale_fill_grey()

#points = T, alpha = .01,
#point_args = list(alpha = .005, width = .1))

saveRDS(sens_orig, here::here("mods", "sens_orig.rds"))
sens_orig <- readRDS(sens_orig, here::here("mods", "sens_orig.rds"))




sens_imp  <-
  plot(conditional_effects(imp_s, "wave", ndraws = 100,  spaghetti = T))




saveRDS(sens_imp, here::here("mods", "sens_imp.rds"))
sens_imp.rds <-
  readRDS(sens_orig, here::here("mods", "sens_imp.rds"))

#points = T, alpha = .01,
#point_args = list(alpha = .005, width = .1))
sessionInfo()
saveRDS(sens_imp, here::here("mods", "sens_imp.rds"))

# graph
sens_i_9  <-
  sens_imp$`wave` +   scale_y_continuous(limits = c(3.5, 4.5)) + #scale_fill_manual(values=c("#CC6666")) +
  labs(
    title = "Deleted and fully imputed model: b = .06",
    subtitle = "Years 2012-2018, N = 33,735 (2012 & 2016 Cohorts)",
    y = "Muslim Warmth",
    x = "Years: 2012-2018 (pre-attack)"
  ) + theme_classic() + scale_colour_okabe_ito(alpha = .5) +
  annotate(
    "rect",
    xmin = 4,
    xmax = 6,
    ymin = 3.5,
    ymax = 4.25,
    alpha = .1,
    fill = "red"
  ) +
  annotate("text",
           x = 5,
           y = 4.35,
           label = "Deleted & then\nmultiply-imputed")

sens_i_9
# Graph
library(scales)
sens_i_9a  <-
  sens_orig$`wave` +   scale_y_continuous(limits = c(3.5, 4.5)) + #scale_fill_manual(values=c("#CC6666")) +
  labs(
    title = "No imputation model: b = .06",
    subtitle = "Years 2012-2018, N = 33,735 (2012 & 2016 Cohorts)",
    y = "Muslim Warmth",
    x = "Years: 2012-2018 (pre-attack)"
  ) + theme_classic() + scale_colour_okabe_ito(alpha = .5)

sens_i_9a

comb_sens_graph <-
  sens_i_9a  + sens_i_9 + plot_annotation(tag_levels = "A")





comb_sens_graph

comb_sens_graph
ggsave(
  comb_sens_graph,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "comb_sens_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


## additional graphs
posterior_r <- as.matrix(imp_r)
posterior <- as.matrix(imp_s)

p1 <- mcmc_areas(imp_r,
                 pars = c("b_wave"),
                 prob = 0.99)




saveRDS(p2, here::here("mods", "p2"))

p1 <- mcmc_intervals(imp_r, pars = c("b_wave", "sigma"))
p1

p1a <-
  mcmc_hist(posterior_r, pars = c("intercept",  "wave", "sigma"))

p2 <- mcmc_intervals(posterior,
                     pars = c("b_wave", "sigma"))


(sens_i_9a  + sens_i_9) + plot_annotation(tag_levels = "A")

