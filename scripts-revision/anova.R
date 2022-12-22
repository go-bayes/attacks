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


df <- readRDS(here::here("data", "df_s"))


dt_2013 <- readRDS(here::here(push_mods, "2013_cohort_attacks"))


dat_anova <- dt_2013 |>
  filter(Wave == "Time10")


# check n
length(unique(dat_anova$Id))

m1 <- summary(lm (Warm.Elderly ~ Attack + Warm.Elderly_c, data = dat_anova))
summary(lm (Warm.Overweight~ Attack + Warm.Overweight_c, data = dat_anova))
summary(lm (Warm.MentalIllness ~ Attack + Warm.MentalIllness_c, data = dat_anova))
summary(lm (Warm.Muslims ~ Attack + Warm.Muslims_c, data = dat_anova))





models <- list(
  "Warmth to Elderly" = lm (Warm.Elderly ~ Attack + Warm.Elderly_c, data = dat_anova),
  "Warmth to Mentally Ill" = lm (Warm.MentalIllness ~ Attack + Warm.MentalIllness_c, data = dat_anova),
  "Warmth to Overweight" = lm (Warm.Overweight~ Attack + Warm.Overweight_c, data = dat_anova),
  "Warmth to Muslims"  = lm (Warm.Muslims ~ Attack + Warm.Muslims_c, data = dat_anova)
)


# format latex plain
options("modelsummary_format_numeric_latex" = "plain")


m_anova<- modelsummary::modelsummary(
  models,
  # shape = term : contrast ~ model,
  # vcov = "robust",
  coef_omit = c(1,3:6),
  gof_map = NA,
  gof_map = !c("nobs", "r.squared"),
  gof_omit = "^(?!.*[{conf.low}, {conf.high}])",
  statistic  = NULL,
  # "conf.int",
  estimate =  "{estimate} [{conf.low}, {conf.high}]",
  standardize = "refit",
  output = "latex",
  title = "Comparative effects of the attacks: negative controls and Muslims (standardised)",
  # escape = TRUE

)

m_anova_s <- modelsummary::modelsummary(
  models,
 # shape = term : contrast ~ model,
 # vcov = "robust",
  coef_omit = c(1,3:6),
  gof_map = NA,
  gof_map = !c("nobs", "r.squared"),
  gof_omit = "^(?!.*[{conf.low}, {conf.high}])",
  statistic  = NULL,
  # "conf.int",
  estimate =  "{estimate} [{conf.low}, {conf.high}]",
  standardize = "refit",
  output = "latex",
  title = "Comparative effects of the attacks: negative controls and Muslims (standardised)",
 # escape = TRUE

)

m_anova

#parameters::model_parameters(m1, standardize = "smart")
#"refit", "posthoc", "basic", "smart" or "pseudo".


coefplot <- modelsummary::modelplot(models,  coef_omit = c(1,3:6)) + scale_color_okabe_ito() + theme_minimal()
coefplot



#
# p_gee
#
# ggsave(
#   p_gee,
#   path = here::here(here::here("figs")),
#   width = 10,
#   height = 5,
#   units = "in",
#   filename = "gee_coef_rev.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )
#
#


