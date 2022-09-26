# Analyisis
# author Joseph Bulbulia: joseph.bulbulia@gmail.com
# created by assembling materials from original analysis into one script.
# packages ----------------------------------------------------------------
#libraries required for analysis
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


# clone data to enable imputation of missing outcomes  --------------------

library(tidyverse)
kf3  <- km_all3 %>%
  group_by(Id) %>%  # All ZEROS
  # mutate(Attack == as.numeric(Attack))%>%
  mutate(As = (
    ifelse(
      Wave == "Time10" & Attack == 1 | Wave == "Time11" |
        Wave == "Time12",
      0,
      ifelse(Wave == "Time10" &
               Attack == 0, 1, Attack)
    )
  )) %>%
  mutate(Ys = ifelse(
    Wave == "Time10" & Attack == 1 | Wave == "Time11" |
      Wave == "Time12",
    NA,
    ifelse(Wave == "Time10" &
             Attack == 0, NA, Warm.Muslims)
  )) %>%
  ungroup() %>%
  arrange(Wave, Id)


# dat_all N
# correct

# CHeck NAs = Correct
table1::table1( ~ Ys |
                  Wave * as.factor(As), data = kf3, overall = F)




## Bind - double dataset to creat missing values
ka3 <- km_all3 %>%
  bind_rows(kf3) %>%
  arrange(Id, Wave) %>%
  mutate(As = as.factor(As)) #%>%
# select(-c(Warm.Muslims,Attack))

saveRDS(ka3, here::here( "mods", "ka3"))

#head(ka3$Ys)
# Check

# Missing data problem
t2 <- table1::table1(~ Ys | Wave * As, data = ka3, overall = F)
t2
t2
t1kable(t2, format = "latex")



# link dfs for zero estimate -----------------------------------------
km_zero <- ka3 %>%
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
    yrs,
    TSCORE_i,
  ) %>%
  arrange(Wave, Id)

str(km_zero$As)
summary(km_zero$yrs)


# remove wave 10 from the pre-attack sample (above)
km_pre <- all_d_selected %>%
  # dplyr::filter(Wave != "Time10")%>%
  # droplevels() %>%
  dplyr::select(c(-wave)) %>% # wrong
  dplyr::mutate(As = as.factor(As)) %>%
  group_by(Id) %>%
  arrange(Wave, Id)

# cehck
table1::table1(~ Ys | Wave * As, data = km_pre, overall = FALSE)



# zero only dataset
bind_zero <- full_join(km_pre, km_zero) %>%
  arrange(Wave, Id)

# check
table(bind_zero$Wave)
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

bind_zero$Wave <- fct_relevel(bind_zero$Wave, levels)
levels(bind_zero$Wave)
str(bind_zero$Wave)
head(bind_zero)
## Make numeric var

bind_zero1 <- bind_zero %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  dplyr::select(-c(dys, yrs)) %>%  # not working
  arrange(Wave, Id) %>%
  group_by(Id) %>%
  mutate(dys = TSCORE_i - min(TSCORE_i)) %>% # Not used
  mutate(yrs = dys / 365) %>% # note used
  ungroup()

summary(bind_zero1$wave)



# zero table --------------------------------------------------------------


zt <-
  table1::table1(~ Ys |
                   As * Wave,
                 data = bind_zero1,
                 overall = F,
                 transpose = F)
zt
t1kable(zt, format = "latex")

# Impute 0s ------------------------------------------------------------

bind_zero1 <- as.data.frame(bind_zero1)


# impute
library(Amelia)


# We impute the entire distribution of Attack = 0, on the assumption that there 2018 and 2019 have an identical distribution.
# assume Y^0|A=2018 = Y^0 2019

# limit Ys to range 1 to 7

# find col number
# create bounds

match("Ys", names(bind_zero1))  # for obtaining bounds for Muslim outcome

bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

# cannot get dys/years easily, use wave
imputed0 <- amelia(
  set.seed = 1234,
  bind_zero1,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  #ords = "Ys",  #*** NOTE THAT IN ORIGINAL ANLYSIS WE USE ORDS
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "TSCORE_i", "dys", "yrs"),
  # yrs not working
  lags = "Ys",
  #leads="Ys",
  polytime = 2,
  # Allow polynomial
  intercs = F,
  # to many vars
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(bind_zero1)
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


