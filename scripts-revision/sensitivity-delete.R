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

push_mods

# Read data
# get zero data


dt_ni <-
  arrow::read_parquet(here::here(push_mods, "dt_ni-attacks-2012-6pre.rds"))

df_del <- dt_ni

table(df_del$Wave)

df_del <- df_del |> filter(
     Wave == "Time4" |
      Wave == "Time5" |
      Wave == "Time6" |
      Wave == "Time7" |
      Wave == "Time8" |
      Wave == "Time9" |
      Wave == "Time10"
  )

df_del$Y_Warm.Muslims


table1::table1(~ Y_Warm.Muslims | Wave, data = df_del)
lm(data = df_del, Y_Warm.Muslims ~ as.numeric(Wave))

# create data deleting waves 9, 10
# Replace obeserved values with NA
df_del2 <- df_del

df_del2$Y_Warm.Muslims[df_del2$Wave == "Time7"] <- NA
df_del2$Y_Warm.Muslims[df_del2$Wave == "Time8"] <- NA
df_del2$Y_Warm.Muslims[df_del2$Wave == "Time9"] <- NA
df_del2$Y_Warm.Muslims[df_del2$Wave == "Time10"] <- NA

lm(data = df_del2, Y_Warm.Muslims ~ (Wave))




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



# model for zeros
m_delet <- brm(
  backend = "cmdstanr",
  data = df_del2,
  family = "gaussian",
  bform_mus_impute_1,
  prior = prior,
  init = 0,
  file =  here::here(push_mods,
                     "impute-2013-zero-deleted.rds")
)




# set N for id counts
id_0 <- m_delet$data$Id

length(unique(id_0))


#standata(m_delet)


# analysis
name <- "yfit_muslim"

fitted_values_0 <- fitted(m_delet, ndraws = 100)


# make df
fitted_values_0 <- data.frame(fitted_values_0)
head(fitted_values_0)

mean(fitted_values_0$Est.Error)

nrow(fitted_values_0)
# needs to be df
yfit <- as.data.frame(fitted_values_0$Estimate)
sd <- as.data.frame(fitted_values_0$Est.Error)
# rename
colnames(yfit) <- name
#colnames(sd) <- name_error

length(id_0)



# data frame
dat_0del <-
  as.data.frame(cbind(Y_orig = standata(m_delet)$Y, standata(m_delet)$X, yfit, id_0, sd)) |>
  mutate(id = as.factor(id_0)) |>
  arrange(id, wave)



 dat_0del <- dat_0del |>
  mutate(yfit_ORD = round(yfit_muslim, digits = 0)) |>
  mutate(as = as.factor(rep(0, nrow(dat_0del)))) |>
  select(-id_0) |> mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                                   yfit_muslim,
                                                   Y_orig)) |>
  # group_by(id) |>
  # mutate(se = if_else(Y_orig != Inf, 0, sd)) |>
  # ungroup() |>
  # mutate(se = if_else(se <= 0, .01, se)) |>
  mutate(wave = wave + 6) |>
  mutate(Wave = as.factor(wave))


table(dat_0del$wave)

#save Zeros
arrow::write_parquet(dat_0, here::here(push_mods, "dat_0del"))


# import model for fitted data - no delitions

#
dat_0 <- arrow::read_parquet(here::here(push_mods, "dat_0-2012"))






dat_0_org <- dat_0 |>
  mutate(wave = wave + 6) |>
  mutate(Wave = as.factor(wave)) |>
  filter(wave < 7 ) |>
  droplevels()

table(dat_0del$wave)

table(dat_0_org$wave)



# rename levels:
levels(dat_0_org$Wave) <-
  c("Time4",
    "Time5",
    "Time6",
    "Time7",
    "Time8",
    "Time9",
    "Time10")

levels(dat_0del$Wave) <-
  c("Time4",
    "Time5",
    "Time6",
    "Time7",
    "Time8",
    "Time9",
    "Time10")


# check
x <- table1::table1(~ yfit_ORD  + yfit_muslim |
                      factor(Wave),
                    data = dat_0del,
                    overall = F)
x

x <- table1::table1(~ yfit_ORD  + yfit_muslim |
                      factor(Wave),
                    data = dat_0_org,
                    overall = F)
x







# Pol.Orient_cZ  +
#   AGREEABLENESS_cZ +
#   CONSCIENTIOUSNESS_cZ +
#   EXTRAVERSION_cZ +
#   HONESTY_HUMILITY_cZ +
#   OPENNESS_cZ +
#   NEUROTICISM_cZ +
#   Age_cZ +
#   BornNZ_c +
#   Edu_cZ +
#   Employed_c +
#   #  EthCat_c +
#   Male_c +
#   NZDep2013_cZ +
#   NZSEI13_cZ +
#   Parent_c +
#   Partner_c +
#   RaceRejAnx_cZ +
#   Relid_cZ #+
#   REGC_2022_c +
# Rural_GCH2018_c

models <- list(
  "GEE: deleted values: fitted" = geeglm(data = dat_0del,
                                         formula = yfit_muslim ~  Wave * Pol.Orient_cZ,
  id = id,
  wave = wave,
  corstr = "ar1"
),
"GEE: deleted values: ordinal" = geeglm(data = dat_0del,
                                        formula = yfit_ORD ~  Wave * Pol.Orient_cZ,,
id = id,
wave = wave,
corstr = "ar1"
),
"GEE: original fitted" = geeglm(data = dat_0_org,
                                       formula = yfit_muslim ~   Wave * Pol.Orient_cZ,,
                                       id = id,
                                       wave = wave,
                                       corstr = "ar1"
),
"GEE: original values: ordinal" = geeglm(data = dat_0_org,
                                        formula = yfit_ORD ~  Wave * Pol.Orient_cZ,,
                                        id = id,
                                        wave = wave,
                                        corstr = "ar1"
)
)


# format latex plain
options("modelsummary_format_numeric_latex" = "plain")

m_del_fits <- modelsummary::modelsummary(
  models,
  gof_omit = "^(?!.*[{conf.low}, {conf.high}])",
  statistic  = NULL,
  # "conf.int",
  estimate =  "{estimate} [{conf.low}, {conf.high}]",
  #  standardize = "posthoc",
  # output = "latex",
  title = "Generalised Estimating Equations: comparison of imputations approaches"
)





m_del_fits


p_gee_sensitivity <-
  modelsummary::modelplot(models, coef_omit = 'Interc') + scale_color_okabe_ito()

p_gee_sensitivity

ggsave(
  p_gee_sensitivity,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "p_gee_sensitivity.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# GEE graphs --------------------------------------------------------------


# GEE graphs --------------------------------------------------------------

g_model_del <- geeglm(
  data = dat_0del,
#  formula = yfit_ORD ~ Wave * Pol.Orient_cZ,
   formula = yfit_muslim ~ Wave * Pol.Orient_cZ,
  # formula = yimpute_muslim ~  Attack * Wave * Pol.Orient_cZ,
  id = id,
  wave = wave,
  corstr = "ar1"
)

summary(g_model1)

g_model2 <- geeglm(
  data = dat_0_org,
  #formula = yfit_ORD ~ Wave * Pol.Orient_cZ,
  formula = yfit_muslim ~   Wave * Pol.Orient_cZ,
  # formula = yimpute_muslim ~  Attack * Wave * Pol.Orient_cZ,
  id = id,
  wave = wave,
  corstr = "ar1"
)

p0a1 <-
  plot(ggeffects::ggpredict(g_model1, terms = c("Wave", "Pol.Orient_cZ[-1.91, -1,  0, 1, 2.46]"))) + scale_y_continuous(limits = c(2.6, 4.8)) + scale_y_continuous(limits = c(2.6, 4.8)) +
  labs(title = "Imputed data",
       subtitle = "",
       #y = "Conterfactual contrasts: Warmth to Muslims on difference scale"
  ) +
  scale_color_okabe_ito() +  theme_pubclean()

p0a2 <-
  plot(ggeffects::ggpredict(g_model2, terms = c("Wave",  "Pol.Orient_cZ[-1.91, -1,  0, 1, 2.46]"))) + scale_y_continuous(limits = c(2.6, 4.8)) +
  labs(title = "Original Data",
       subtitle = "",
       #y = "Conterfactual contrasts: Warmth to Muslims on difference scale"
       ) +
  scale_color_okabe_ito() +  theme_pubclean()


# select only attack condition

p0a1 + p0a2





gee_mar_eff <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = c("Wave"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  scale_y_continuous(limits = c(-.05, .41)) +
  labs(title = "Marginal counterfactual contrasts in effect magnitude",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,854",
       y = "Conterfactual contrasts: Warmth to Muslims on difference scale") +
  scale_color_viridis_d() +  theme_pubclean()

gee_mar_effi <- plot_cco(
  g_modi,
  effect = "Attack",
  condition = c("Wave"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  scale_y_continuous(limits = c(-.05, .41)) +
  labs(title = "Marginal counterfactual contrasts in effect magnitude",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,854",
       y = "Conterfactual contrasts: Warmth to Muslims on difference scale") +
  scale_color_viridis_d() +  theme_pubclean()

gee_mar_eff + gee_mar_effi
# difference scale
gee_con_eff <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = list("Wave",
                   #                  #    Pol.Orient_cZ = "fivenum"),
                   Pol.Orient_cZ = c(-1.91, -1, 0, 1, 2.46)),
  # #  Pol.Orient_cZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  scale_y_continuous(limits = c(-.05, .41)) +
  labs(title = "Conditional counterfactual contrasts by political conservativism",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,854",
       y = "Conterfactual contrasts: Warmth to Muslims on difference scale") +
  scale_color_okabe_ito() + theme_pubclean()

gee_con_eff

gee_con_effi <- plot_cco(
  g_modi,
  effect = "Attack",
  condition = list("Wave",
                   #                  #    Pol.Orient_cZ = "fivenum"),
                   Pol.Orient_cZ = c(-1.91, -1, 0, 1, 2.46)),
  # #  Pol.Orient_cZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  scale_y_continuous(limits = c(-.05, .41)) +
  labs(title = "Conditional counterfactual contrasts by political conservativism",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,854",
       y = "Conterfactual contrasts: Warmth to Muslims on difference scale") +
  scale_color_okabe_ito() + theme_pubclean()

gee_plots <-
  gee_mar_eff + gee_con_eff + plot_annotation(tag_levels = "A")

gee_plots

gee_plotsi <-
  gee_mar_effi + gee_con_effi + plot_annotation(tag_levels = "A")

gee_plots/gee_plotsi

ggsave(
  gee_plots,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "gee_results_rev-5all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



## GEE PLOTS

# gee
pl_a_gee <-
  plot (ggeffects::ggpredict(g_mod, terms = c("Wave", "Attack")))  +
  scale_color_okabe_ito()  +
  labs(title = "Predicted Muslim Warmth by condition and wave (GEE)",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,824",
       y = "Warmth to Mulsims (1-7 (ordinal)")  +
  scale_fill_okabe_ito() + theme_pubclean() +  scale_y_continuous(limits = c(4, 4.5))

# check
pl_a_gee

pl_a_geei <-
  plot (ggeffects::ggpredict(g_modi, terms = c("Wave", "Attack")))  +
  scale_color_okabe_ito()  +
  labs(title = "Predicted Muslim Warmth by condition and wave (GEE)",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,824",
       y = "Warmth to Mulsims (1-7 (ordinal)")  +
  scale_fill_okabe_ito() + theme_pubclean() +  scale_y_continuous(limits = c(4, 4.5))


# mot much diff
pl_a_gee + pl_a_geei


ggsave(
  pl_a_gee,
  path = here::here(here::here("figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gee_ord_pred.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



pl_b_gee <- plot (ggeffects::ggpredict(
  g_mod,
  terms = c("Wave",
            "Attack",
            "Pol.Orient_cZ[-1.91, -1,  0, 1, 2.46]")
),  one.plot = TRUE) +
  scale_color_okabe_ito()  +
  labs(title = "Predicted Muslim Warmth effect modification by political conservativism (GEE)",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,824",
       y = "Warmth to Mulsims (1-7 (ordinal)") +
  scale_y_continuous(limits = c(3.0, 5.2)) +
  scale_fill_okabe_ito(alpha = 1)  +
  facet_wrap(~ facet, ncol = 5) + theme_pubclean()

pl_b_gee

gee_mar_eff + gee_con_eff
# graph model
gee_4 <-
  (pl_a_gee + gee_mar_eff) / (pl_b_gee + gee_con_eff) + plot_annotation(tag_levels = "A")
gee_4

dev.off()

ggsave(
  gee_4,
  path = here::here(here::here("figs")),
  width = 16,
  height = 12,
  units = "in",
  filename = "gee_4.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

