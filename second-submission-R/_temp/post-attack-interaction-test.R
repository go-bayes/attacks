# set digits
options(scipen = 999)

#libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

library("modelsummary")
options("modelsummary_format_numeric_latex" = "plain")

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("lead", "dplyr")

# additional (add to libs later)
library(ggdist)
library(geepack)

# read functions (most not used here)
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


# bayesian models set up
library("brms")
library("rstan")
rstan_options(auto_write = TRUE) # bayesian estimation
options(mc.cores = parallel::detectCores ()) # use all course
theme_set(theme_pubclean()) # nice theme
library(cmdstanr)

# for saving models (again for JB only - set paths for your own directory)

# set paths for JB** YOU NEED TO SET YOUR OWN **
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/attacks/mods")

push_figs <-
  fs::path_expand(" /Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/attacks/figs")

pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )


# Read data
dat <- arrow::read_parquet(pull_path)


# select post attack group
w18 <- dat |>
  filter(
  (Wave ==  2018 & YearMeasured == 1) |
  (Wave ==  2019 & YearMeasured != -1) |
  (Wave ==  2020 & YearMeasured != -1) |
  (Wave == 2021 & YearMeasured != -1)
) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) |>
  droplevels() |>
  arrange(Id, Wave) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  dplyr::mutate(org18 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold18 = mean(org18, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>%
  dplyr::mutate(Attack = as.numeric((ifelse(
    TSCORE >= 3545 &
      (Wave == 2018 |
         Wave == 2019 |
         Wave == 2020 |
         Wave == 2021),
    1,
    0
  )))) |>
  # dplyr::mutate(
  #   #   Y_Warm.Asians = Warm.Asians,
  #   #   Y_Warm.Chinese = Warm.Chinese,
  #   # Warm.Disabled, only in wave12
  #   Y_Warm.Elderly = Warm.Elderly,
  #   #    Y_Warm.Immigrants = Warm.Immigrants,
  #   #    Y_Warm.Indians = Warm.Indians,
  #   #    Y_Warm.Maori = Warm.Maori,
  #   Y_Warm.MentalIllness = Warm.MentalIllness,
  #   # not in 8
  #   Y_Warm.Muslims = Warm.Muslims,
  #   #    Y_Warm.NZEuro = Warm.NZEuro,
  #   #    Y_Warm.Overweight = Warm.Overweight,
  #   #    Y_Warm.Pacific = Warm.Pacific,
  #   #  Y_Warm.Refugees = Warm.Refugees,
  #   As = Attack
  # ) %>%
  dplyr::mutate(Pol.Orient_b = if_else(Wave == "2018", (Pol.Orient), NA_real_)) %>%
  fill(Pol.Orient_b, .direction = "downup") |>
  ungroup() |>
  mutate(Pol.Orient_bZ = scale(Pol.Orient_b)) |>
  mutate(Pol.Orient_Z = scale(Pol.Orient)) |>
  mutate(wave = as.numeric(Wave)-1) |>
  select(Warm.Muslims, Pol.Orient_bZ, Pol.Orient_Z, Id, wave, Wave, Attack) |>
  filter(Attack == 1) |>
  arrange(Id, Wave)

# check wave
table(w18$wave)
table(w18$Attack)
# n = 47948
length(unique(w18$Id))


# Baseline model
m1 <- lme4::lmer(Warm.Muslims ~ Pol.Orient_bZ * Wave  + (1|Id) , data = w18 )

# allow for political change
m2 <- lme4::lmer(Warm.Muslims ~ Pol.Orient_Z * Wave  + (1|Id) , data = w18 )


model_parameters(m1) |>
  kbl(format = "latex", booktabs = TRUE, digits = 3)


post_cco <- plot_cco(
  m1,
  effect = "Wave",
  condition = list(
#    Pol.Orient_bZ = c(-1.91, -1, 0, 1, 2.46)),
  Pol.Orient_bZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
#  scale_y_continuous(limits = c(-.05, .41)) +
  labs(title = "Conditional counterfactual contrasts by political conservativism",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,854",
       y = "Conterfactual contrasts: Warmth to Muslims on difference scale") +
  scale_color_okabe_ito() + theme_pubclean()

post_cco


post_cco2 <- plot_cco(
  m2,
  effect = "Wave",
  condition = list(
    Pol.Orient_Z = c(-1.91, -1, 0, 1, 2.46)),
  # #  Pol.Orient_cZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  #  scale_y_continuous(limits = c(-.05, .41)) +
  labs(title = "Conditional counterfactual contrasts by political conservativism",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,854",
       y = "Conterfactual contrasts: Warmth to Muslims on difference scale") +
  scale_color_okabe_ito() + theme_pubclean()

post_cco


pl_polst <- plot (ggeffects::ggpredict(
  m1,
  terms = c("Wave",
            "Pol.Orient_bZ[-1.91, -1,  0, 1, 2.46]")
),  one.plot = TRUE) +
  scale_color_okabe_ito()  +
  labs(title = "Predicted Muslim Warmth effect modification by political conservativism (lmer)",
       subtitle = "NZAVS 2018 Cohort, Times 10 - 13, N = 47,948",
       y = "Warmth to Mulsims (1-7 (ordinal)") +
  # scale_y_continuous(limits = c(3.0, 5.2)) +
  scale_fill_okabe_ito(alpha = 1)   + theme_pubclean()

pl_polst + pl_polst2

pl_polst

# allow repeated measures of Political Orientation  ( Not used, no difference)
pl_polst2 <- plot (ggeffects::ggpredict(
  m2,
  terms = c("Wave",
            "Pol.Orient_Z[-1.91, -1,  0, 1, 2.46]")
),  one.plot = TRUE) +
  scale_color_okabe_ito()  +
  labs(title = "Expected Muslim Warmth by political conservativism: post-attack only (lmer)",
       subtitle = "NZAVS 2018 Cohort, Times 10 - 13, N = 47,948",
       y = "Warmth to Mulsims 1-7 (ordinal)") +
 # scale_y_continuous(limits = c(3.0, 5.2)) +
  scale_fill_okabe_ito(alpha = 1)   + theme_pubclean()

pl_polst2




ggsave(
  pl_polst,
  path = here::here(here::here("figs")),
  width = 8,
  height = 8,
  units = "in",
  filename = "pl_polst.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)





