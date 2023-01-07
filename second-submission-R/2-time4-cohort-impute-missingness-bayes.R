# 2 - Impute missing data


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




# read data
dat_bayes <-
  arrow::read_parquet(here::here(push_mods, "2012_cohort_attacks"))


# check data
table1(
  ~ Sample + Warm.Muslims + Male + Age + EthCat +
    REGC_2022  + Rural_GCH2018 +  EthCat + Edu_cZ + Pol.Orient_cZ +  NZDep2013 + NZSEI13  +  as.numeric(Id) |
    Wave,
  data = dat_bayes
)

# image
#modelsummary::datasummary_crosstab(mean(Warm.Muslims) ~ Wave * as.factor(Attack), data = dat_bayes)


# create table
dat_bayes_9 <- dat_bayes |>
  filter(Wave == "Time9")


x <- table1::table1(
  ~
    Pol.Orient  +
    AGREEABLENESS +
    CONSCIENTIOUSNESS +
    EXTRAVERSION +
    HONESTY_HUMILITY +
    OPENNESS +
    NEUROTICISM +
    Age +
    as.factor(BornNZ) +
    Edu +
    as.factor(Employed) +
    as.factor(EthCat) +
    as.factor(Male) +
    NZDep2013  +
    NZSEI13  +
    as.factor(Parent) +
    as.factor(Partner) +
    RaceRejAnx +
    Relid +
    #  as.factor(REGC_2022)
    as.factor(Rural_GCH2018) |
    factor(Wave) * factor(As),
  data = dat_bayes_9,
  overall = F
)
x

kable(x, format = "latex", booktabs = TRUE)
t1kable(x, format = "latex")

# Missing data problem table
t2 <- table1::table1(~ Y_Warm.Muslims |
                       Wave * as.factor(As),
                     data = dat_bayes,
                     overall = F)


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


# bind data
dt_bind <-  dat_bayes %>%
  bind_rows(dt_prep) %>%
  arrange(Id, Wave)


dt_temp <- dt_bind |>
  filter(Wave == "Time10" |
           Wave == "Time11" |
           Wave == "Time12" |
           Wave == "Time13")
# Test NAs = Correct
x <- table1::table1(
  ~ Y_Warm.Muslims +
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
    Relid_cZ +
    REGC_2022_c | Wave * as.factor(As),
  data = dt_temp,
  overall = F
)

x

kable(x, format = "latex", booktabs = TRUE)
t1kable(x, format = "latex")


# link dfs for zero estimate -----------------------------------------

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



# one data frame
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
        Relid_cZ +
        REGC_2022_c +
        Rural_GCH2018_c
    ) + (1 + wave | Id)

  )


# impute data for zeros
m_0a <- brm(
  backend = "cmdstanr",
  data = dt_ni,
  family = "gaussian",
  bform_mus_impute_1,
  prior = prior,
  init = 0,
  file =  here::here(
    push_mods,
    "impute-2012-zero-MUS-attacks-use-interaction-2012-6all.rds"
  )
)


summary(m_0a)


# impute data for 1s
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


dat_0 = dat_0 |>
  mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                  yfit_muslim,
                                  Y_orig)) |>
  mutate(yimpute_muslim_lag = dplyr::lag(yimpute_muslim))
#
# dat_0 = dat_0 |>
#   mutate(yimpute_muslim = if_else(Y_orig == Inf,
#                                   yfit_muslim,
#                                   Y_orig)) |>
#   mutate(yimpute_muslim_lag = dplyr::lag(yimpute_muslim))
#

head(dat_0$yimpute_muslim_lag)

table(dat_0$wave)

arrow::write_parquet(dat_0, here::here(push_mods, "dat_0-2012"))
dat_0 <- arrow::read_parquet(here::here(push_mods, "dat_0-2012"))



## Same for condition = A1

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
  mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                  yfit_muslim,
                                  Y_orig)) |>
  mutate(yimpute_muslim_lag = dplyr::lag(yimpute_muslim)) |>
  select(-id_1)


# combine data

dat_combined <- rbind(dat_0, dat_1) |>
  filter(wave == 0 | wave == 1 | wave == 2 | wave == 3) |>
  mutate(Wave = as.factor(wave),
         Condition = as)

str(dat_combined)


# test
dat_temp <- dat_combined |>
  filter(Y_orig != Inf) |>
  droplevels()

dat_temp$yimpute_muslim


summary(t1 <- lm(Y_orig ~ yimpute_muslim , data = dat_temp))
summary(t2 <- lm(Y_orig ~ yfit_ORD , data = dat_temp))
summary(t3 <- lm(Y_orig ~ yfit_muslim  , data = dat_temp))

plot(ggeffects::ggeffect(t3),
     dot.alpha = .1,
     add.data = TRUE)
plot(ggeffects::ggeffect(t2),
     dot.alpha = .1,
     add.data = TRUE)
plot(ggeffects::ggeffect(t1),
     dot.alpha = .1,
     add.data = TRUE)


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

