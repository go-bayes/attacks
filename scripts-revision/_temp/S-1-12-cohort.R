

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



# READ DATA
d_muslim <-
  arrow::read_parquet(here::here(push_mods, "d_muslim-6all"))


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

m_gee <- modelsummary::modelsummary(
  models,
  gof_omit = "^(?!.*[{conf.low}, {conf.high}])",
  statistic  = NULL,
  # "conf.int",
  estimate =  "{estimate} [{conf.low}, {conf.high}]",
  #  standardize = "posthoc",
  #  output = "latex",
  title = "Generalised Estimating Equations: comparison of imputations approaches"
)


m_gee


p_gee <-
  modelsummary::modelplot(models, coef_omit = 'Interc') + scale_color_okabe_ito()


ggsave(
  p_gee,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "COMPARE-OUTCOME-6-gee_coef_rev-6all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# GEE graphs --------------------------------------------------------------


g_mod_marg <- geeglm(
  data = d_muslim,
  # formula = yfit_ORD ~  Attack * Wave * Pol.Orient_cZ,
  formula = yfit_muslim ~  -1 +  Attack  *  Wave,
  #  formula = yimpute_muslim ~  Attack * Wave * Pol.Orient_cZ,
  #  yfit_muslim
  id = id,
  wave = wave,
  corstr = "ar1"
)
p1

p1 <-
  plot(ggeffects::ggeffect(g_mod_marg, terms = c("Wave", "Attack"))) #, add.data = TRUE, dot.alpha =.01, jitter = 1)
summary(g_mod_marg)

g_mod <- geeglm(
  data = d_muslim,
  # formula = yfit_ORD ~  Attack * Wave * Pol.Orient_cZ,
  formula = yfit_muslim ~  Attack * Wave * Pol.Orient_cZ,
  #  formula = yimpute_muslim ~  Attack * Wave * Pol.Orient_cZ,
  #  yfit_muslim
  id = id,
  wave = wave,
  corstr = "ar1"
)

model_parameters(g_mod) |>
  select(-c(Chi2, df_error, p)) |>
  kbl(format = "latex", booktabs = TRUE)

# Quite a bit of variability
p2 <-
  plot(ggeffects::ggeffect(g_mod, terms = c("Wave", "Attack"))) #, add.data = TRUE, dot.alpha =.01, jitter = 1)

p1 + p2
dev.off()

equatiomatic::extract_eq(g_mod, wrap = TRUE, terms_per_line = 1)

p_gee1 <-
  modelsummary::modelplot(g_mod, coef_omit = 'Interc') + scale_color_okabe_ito()

p_gee1


model_parameters(g_mod) |>
  select(-c(Chi2, df_error, p)) |>
  kbl(format = "latex", booktabs = TRUE)


gee_mar_eff12 <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = c("Wave"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  scale_y_continuous(limits = c(-.05, .35)) +
  labs(title = "Marginal contrasts",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Conterfactual contrasts: Warmth to Muslims on difference scale") +
  scale_color_viridis_d() +  theme_pubclean()

gee_mar_eff12






# GEE TABLE CON EFFECTS ---------------------------------------------------


# table
tab <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = list("Wave",
                   Pol.Orient_cZ = c(-1.92, -1, 0, 1, 2)),
  conf_level = 0.95,
  transform_pre = "difference",
  draw = FALSE
)

tab

tab |>
  select(
    c(
      rowid,
      contrast,
      comparison,
      std.error,
      conf.low,
      conf.high,
      Attack,
      condition1,
      condition2
    )
  ) |>
  rename(estimand = rowid,
         Wave = condition1,
         Pol_Orient_SD = condition2) |>
  kbl("markdown", booktabs = TRUE, digits = 3)



# difference scale
gee_con_eff12 <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = list("Wave",
                   Pol.Orient_cZ = c(-1.92, -1, 0, 1, 2)),
  conf_level = 0.95,
  transform_pre = "difference"
) + scale_y_continuous(limits = c(-.05, .5)) +
  labs(title = "Counterfactual contrasts by political liberal/conservative orientation (SD)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Conterfactual contrasts, difference scale") +
  scale_color_okabe_ito()

gee_con_eff12
gee_plots12 <-
  gee_mar_eff12 + gee_con_eff12 + plot_annotation(tag_levels = "A")

gee_plots12

ggsave(
  gee_plots12,
  path = here::here(here::here("figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gee_results_rev-6all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)





# graphs ------------------------------------------------------------------
# gee
pl_a_gee12 <-
  plot (ggeffects::ggpredict(g_mod, terms = c("Wave", "Attack")))  +
  scale_color_okabe_ito()  +
  labs(title = "Predicted Muslim Warmth by condition and wave (GEE)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Warmth to Mulsims 1-7 (ordinal)")  +
  scale_fill_okabe_ito() + theme_pubclean() #+  scale_y_continuous(limits = c(3.8, 4.5))

# check
pl_a_gee12


ggsave(
  pl_a_gee12,
  path = here::here(here::here("figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gee_ord_pred-12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



pl_b_gee12 <- plot (ggeffects::ggpredict(
  g_mod,
  terms = c("Wave",
            "Attack",
            "Pol.Orient_cZ[-1.92, -1, 0, 1, 2]")
),  one.plot = TRUE) +
  scale_color_okabe_ito()  +
  labs(title = "Predicted Muslim Warmth: effect modification by political conservativism (GEE)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Warmth to Mulsims 1-7 (ordinal)") +
  # scale_y_continuous(limits = c(3.0, 5.2)) +
  scale_fill_okabe_ito(alpha = 1)  +
  facet_wrap(~ facet, ncol = 5) + theme_pubclean()

pl_b_gee12



ggsave(
  gee_412,
  path = here::here(here::here("figs")),
  width = 16,
  height = 12,
  units = "in",
  filename = "gee_4-12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

gee_marg <-
  pl_a_gee12 / gee_mar_eff12 + plot_annotation(tag_levels = "A")
gee_marg
gee_con <-
  pl_b_gee12 / gee_con_eff12 + plot_annotation(tag_levels = "A")
gee_con

ggsave(
  gee_marg,
  path = here::here(here::here("figs")),
  width = 10,
  height = 12,
  units = "in",
  filename = "gee_marg.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



ggsave(
  gee_con,
  path = here::here(here::here("figs")),
  width = 10,
  height = 12,
  units = "in",
  filename = "gee_con.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)
# graph model
gee_412 <-
  (pl_a_gee12 + gee_mar_eff12) / (pl_b_gee12 + gee_con_eff12) + plot_annotation(tag_levels = "A")
gee_412

ggsave(
  gee_412,
  path = here::here(here::here("figs")),
  width = 16,
  height = 12,
  units = "in",
  filename = "gee_4-12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# BAYES -------------------------------------------------------------------


# prior_mus_cond  = c(
#   set_prior("normal(0,1)",  class = "b"),
#   set_prior("normal(0,1)", class = "b", dpar = "sigma"),
#   set_prior(
#     "student_t(3, 4, 3)",
#     class = "Intercept",
#     lb = 1,
#     ub = 7
#   ),
#   set_prior("exponential(1)", class = "sd")  # only for raneffs
# )



#USED
prior_fit  = c(
  set_prior("normal(0,1)",  class = "b"),
  set_prior(
    "student_t(3, 4, 3)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")  # only for raneffs
)



# bform_mus_cond_fit  =   bf(
#   yfit_muslim ~  Attack  *  Wave *  Pol.Orient_cZ + (1 | id),
#   sigma ~ 0 + as,
#   set_rescor(rescor = FALSE)
# )



#USED
bform_fit  =   bf(yfit_muslim ~  Attack  *  Wave *  Pol.Orient_cZ + (1 |
                                                                       id))



#
# bform_mus_cond_ord  =   bf(yfit_ORD ~  Attack  *  Wave *  Pol.Orient_cZ + (1 |
#                                                                              id),
#                            sigma ~ 0 + as,
#                            set_rescor(rescor = FALSE))

# ord
# bform_mus_cond_ord2  =   bf(yfit_ORD ~  Attack  *  Wave *  Pol.Orient_cZ + (1 ||
#                                                                               id),
#                             sigma ~ 0 + as,
#                             set_rescor(rescor = FALSE))



# system.time(
#   m_cond_mus <- brms::brm(
#     backend = "cmdstanr",
#     data = d_muslim,
#     family = "gaussian",
#     bform_mus_cond_fit,
#     prior_mus_cond,
#     init = 0,
#     iter = 12000,
#     file = here::here(push_mods, "m_cond_mus-2012-use-fit-rev6-n.rds")
#   )
# )



### USE ###
rm(m_cond_mus)
system.time(
  m_cond_mus <- brms::brm(
    backend = "cmdstanr",
    data = d_muslim,
    family = "gaussian",
    bform_fit,
    prior_fit,
    init = 0,
    iter = 12000,
    file = here::here(push_mods, "m_cond_mus-2012-SIMPLE-FIT-REALLY-USE-rev6.rds")
  )
)

summary(m_cond_mus)


parms_fit <-
  model_parameters(
    m_cond_mus,
    centrality = "mean",
    test = "pd",
    diagnostic =  "Rhat"
  )
parms_fit |>
  print_html()
parms_fit

parms_fit |>
  select(-Component) |>
  kbl("latex", booktabs = TRUE)



# coeff plots -------------------------------------------------------------


# areas plot
color_scheme_set("brightblue")


posterior <- as.array(m_cond_mus)
dimnames(posterior)
pars = c(
  #"b_Intercept",
  "b_Attack1" ,
  "b_Wave1" ,
  "b_Wave2" ,
  "b_Wave3",
  "b_Pol.Orient_cZ" ,
  "b_Attack1:Wave1",
  "b_Attack1:Wave2",
  "b_Attack1:Wave3",
  "b_Attack1:Pol.Orient_cZ",
  "b_Wave1:Pol.Orient_cZ",
  "b_Wave2:Pol.Orient_cZ",
  "b_Wave3:Pol.Orient_cZ",
  "b_Attack1:Wave1:Pol.Orient_cZ",
  "b_Attack1:Wave2:Pol.Orient_cZ" ,
  "b_Attack1:Wave3:Pol.Orient_cZ"
)#"b_sigma_as0" ,  "b_sigma_as1", "sd_id__Intercept")


coef_plots  <- mcmc_plot(m_cond_mus,
                         #  pars =
                         variable = pars,
                         #regex_pars = "beta",
                         type = 'areas',
                         prob = 0.95)

coef_plots


ggsave(
  coef_plots,
  path = here::here(here::here("figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "bayes_coef_plots-12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

# ordinal model -----------------------------------------------------------


bform_ord  =   bf(yfit_ORD ~  Attack  *  Wave *  Pol.Orient_cZ)


bform_mus_cond_ord2  =   bf(yfit_ORD ~  Attack  *  Wave *  Pol.Orient_cZ + (1 ||
                                                                              id),
                            sigma ~ 0 + as,
                            set_rescor(rescor = FALSE))



# over-imputed 8 returns to 7

d_muslim_2 <- d_muslim |>
  mutate(yfit_ORD = if_else(yfit_ORD > 7, 7, yfit_ORD))

system.time(
  m_ord <- brms::brm(
    backend = "cmdstanr",
    data = d_muslim_2,
    bform_ord,
    family = cumulative(link = 'logit'),
    init = 0,
    file = here::here(push_mods, "m_cond_mus-2012-use-ordinal-simple-2.rds")
  )
)


# LOOKS GOOD
pp_check(m_ord)

summary(m_ord)

conditional_effects(m_ord, c("Wave", "Pol.Orient_cZ", "Attack"), categorical = TRUE)


make_conditions
conditions <-  make_conditions(d_muslim, vars = c("Wave", "Attack"))


head(conditions)


conditions <- conditions |>
  filter(Wave == 0 | Wave == 3)

pl_ord1 <-
  conditional_effects(
    m_ord,
    effects = "Wave:Attack",
    # plot= FALSE,
    conditions = conditions,
    categorical = T,
    points = TRUE
  )
pl_ord1

# just the interactions

# create labels
# New facet label names for dose variable
# New facet label names for dose variable
Attack.labs <- c("No Attack", "Attack")
names(Attack.labs) <- c("0", "1")

Wave.labs <- c("Wave 0",
               #"Wave +1",
               #"Wave +2",
               "Wave +3")
names(Wave.labs) <- c("0",
                      #"1",
                      #"2",
                      "3")

ord_plot_pol  <-
  plot(pl_ord1, plot = FALSE)[[3]] + facet_wrap(
    Attack ~ Wave,
    ncol = 2,
    labeller = labeller(Attack = Attack.labs,
                        Wave = Wave.labs)
  )  +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito()  + theme(
    legend.title = element_text(color = "black", size = 15),
    legend.text = element_text(color = "black")
  ) + labs(
    fill = "Warm Muslims",
    colour = "Warm Muslims",
    labels = "Warm Muslims",
    title = "Muslim acceptance scores by attack condition, wave and political conservativism (sd)",
    subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
    x = "Political Conservativism (SD)"
  )


ord_plot_pol

# save plot

ggsave(
  ord_plot_pol,
  path = here::here(here::here("figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "ord_plot_pol-12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

# make continuous plot (verification)

conditions2 <-
  make_conditions(d_muslim,
                  vars = c("Pol.Orient_cZ" = c(-1.92, -1, 0, 1, 2)))

conditions2 <-
  data.frame(Pol.Orient_cZ = c(-2, -1, 0, 1, 2))




pl_ord2 <-
  conditional_effects(
    m_ord,
    effects = "Wave:Attack",
    # plot= FALSE,
    conditions = conditions2,
    categorical = F
  )
pl_ord2
#+ labs(
title = "Expected ordinal warmth ratings by Wave and  Political Orientation (Predictions treated as continuous variables)",
subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
x = "Political Conservativism (SD)")

pl_ord2


# just the interactions
p2 <-
  plot(pl_ord2, plot = FALSE)[[1]] + facet_wrap( ~ Pol.Orient_cZ, nrow = 1) + scale_color_okabe_ito()


p2 / ord_plot_pol


summary(m_cond_mus)



# BAYES ORD TABLE CONTRASTS -----------------------------------------------
tab_ord <- plot_cco(
  m_ord,
  effect = "Attack",
  condition = list("Wave",
                   Pol.Orient_cZ = c(-1.92, -1, 0, 1, 2)),
  conf_level = 0.95,
  transform_pre = "difference",
  draw = FALSE
)

tab_ord

tab_ord |>
  select(c(
    rowid,
    contrast,
    comparison,
    conf.low,
    conf.high,
    Attack,
    condition1,
    condition2
  )) |>
  rename(estimand = rowid,
         Wave = condition1,
         Pol_Orient_SD = condition2) |>
  filter(Wave == 0 | Wave == 3) |>
  #  arrange(Wave, estimand) |>
  kbl("markdown", booktabs = TRUE, digits = 3)


#summary(m_cond_mus2)
# GRAPHS ------------------------------------------------------------------


options("modelsummary_format_numeric_latex" = "plain")



#
# m_bayes_latex <- modelsummary::modelsummary(
#   m_cond_mus,
#   gof_omit = "^(?!.*[{conf.low}, {conf.high}])",
#   statistic  = NULL,
#   # "conf.int",
#   ndraws = 100,
#   estimate =  "{estimate} [{conf.low}, {conf.high}]",
#   #  standardize = "posthoc",
#   output = "latex",
#   title = "Bayesian Multi-Level Model: 2012 Cohort"
# )
#
#
# # save table
# saveRDS(m_bayes_latex,
#         here::here(push_mods, "m_bayes-attacks-2012"))
#

# comparisons





# posterior checks
pp_check(m_cond_mus)
plot(m_cond_mus)
summary(m_cond_mus)



# another table
parms <-
  model_parameters(m_cond_mus,   test = "pd")

parms |>
  kbl(format = "latex", booktabs = TRUE)


parms



# BAYES-GRAPHS ------------------------------------------------------------

pred_draws <- predictions(
  m_cond_mus,
  newdata = datagrid("Attack" = c(0, 1),
                     "Wave" = c(0, 1, 2, 3)),
  ndraws = 100,
  re_formula = NULL
)



pred_draws <-
  posteriordraws(pred_draws)

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
    "Pol.Orient_cZ" = c(-1.92, -1, 0, 1, 2)
  ),
  ndraws = 1000,
  re_formula = NA
) |>
  posteriordraws()


pred_mar
pred_mar
library(ggdist)

pl_a12 <-
  ggplot(pred_mar, aes(x = Wave,
                       y = draw,
                       fill = Attack))  + # + scale_y_continuous(limits = c(3, 5.2)) +
  stat_halfeye(slab_alpha = .8) +
  labs(title = "Predicted Muslim Warmth by condition and wave (Bayesian)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Warmth to Mulsims 1-7 (ordinal)")  +
  scale_fill_okabe_ito() + scale_y_continuous(limits = c(3.9, 4.5))

# check
pl_a12



ggsave(
  pl_a12,
  path = here::here(here::here("figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "bayes_ord_pred-12-rev6.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# plot # use
pl_b12 <-
  ggplot(pred_cond, aes(x = Wave,
                        y = draw,
                        fill = Attack))  + scale_y_continuous(limits = c(3, 5.2)) +
  stat_halfeye(slab_alpha = .8) +
  labs(title = "Predicted Muslim Warmth effect modification by political conservativism (Bayesian)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Warmth to Mulsims (1-7 (ordinal)") +
  #  scale_y_continuous(limits = c(3.0,5.2)) +
  facet_grid(. ~ Pol.Orient_cZ,   shrink = T) +
  scale_fill_okabe_ito(alpha = 1)

pl_b12






#
#
# # reproduce graphs --------------------------------------------------------
#
#
# pred <- predictions(m_cond_mus,
#                     newdata = datagrid(Attack = 0:1,
#                                        Wave = c(0:3),
#                                        Pol.Orient_cZ = c(-1.92,-1, 0, 1, 2)))
#
# mod <- posteriordraws(pred)
# mod |>
#
# plot_cme(pred, effect = "Attack", condition = list("Wave" = c(0:3),
#                                                    Pol.Orient_cZ = c(-1.92,-1, 0, 1, 2)))
# pred
#
# ggplot(mod, aes(x = draw, fill = Attack)) +
#   geom_density(alpha = .2, color = "red") +
#   labs(x = "S",
#        y = "S",
#        fill = "")
#


# comp <- comparisons(m_cond_mus, ndraws = 100,  condition = list("Attack" = c(0:1),
#   "Wave" = c(0:3),
#                                                      Pol.Orient_cZ = c(-1.92,-1, 0, 1, 2)))
#
# comp |> tidy()
# comp
#                                                     #                                                    Pol.Orient_cZ = c(-1.92,-1, 0, 1, 2))ndraws = 100) |> tidy()


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
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Difference in Warmth to Muslims") +
  scale_color_okabe_ito()

# difference scale
bayes_con_eff12 <- plot_cco(
  m_cond_mus,
  effect = "Attack",
  condition = list("Wave",
                   #    Pol.Orient_cZ = "fivenum"),
                   Pol.Orient_cZ = c(-1.92, -1, 0, 1, 2)),
  #  Pol.Orient_cZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  scale_y_continuous(limits = c(-.05, .41)) +
  labs(title = "Conditional contrasts by political conservativism (Bayesian)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Difference in Warmth to Muslims") +
  scale_color_okabe_ito()

# numbers
plot_tab <- plot_cco(
  m_cond_mus,
  effect = "Attack",
  condition = list("Wave",
                   #    Pol.Orient_cZ = "fivenum"),
                   Pol.Orient_cZ = c(-1.92, -1, 0, 1, 2)),
  #  Pol.Orient_cZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference",
  draw = FALSE
)

plot_tab |>
  select(-c(rowid, term, type, id, yfit_muslim)) |>
  rename(Wave = condition1,
         Pol_Orient_SD = condition2) |>
  kbl("latex", booktabs = TRUE)


bayes_mar_eff12
bayes_con_eff12



plot_tab <- plot_cco(
  m_ord,
  effect = "Attack",
  condition = list("Wave",
                   #    Pol.Orient_cZ = "fivenum"),
                   Pol.Orient_cZ = c(-1.92, -1, 0, 1, 2)),
  #  Pol.Orient_cZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference",
  draw = FALSE
)

plot_tab |>
  select(-c(rowid, term, type)) |>
  rename(Wave = condition1,
         Pol_Orient_SD = condition2) |>
  kbl("latex", booktabs = TRUE)


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


bayes_marg_2panel12

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

bayes_con_2panel12

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

test <- plot_cme(m_cond_mus,
                 effect = "Attack",
                 condition = list("Wave",
                                  #    Pol.Orient_cZ = "fivenum"),
                                  Pol.Orient_cZ = c(-1.92, -1, 0, 1, 2))) +
  scale_y_continuous(limits = c(0, .4))

test + pl_b12



test2
# compare gee bayes -------------------------------------------------------
(pl_b_gee12 + pl_b12)  + plot_annotation(tag_levels = "A")

pl_b12



# GEE graphs --------------------------------------------------------------



g_mod <- geeglm(
  data = d_muslim,
  # formula = yfit_ORD ~  Attack * Wave * Pol.Orient_cZ,
  formula = yfit_muslim ~  Attack * Wave * Pol.Orient_cZ,
  # formula = yimpute_muslim ~  Attack * Wave * Pol.Orient_cZ,
  id = id,
  wave = wave,
  corstr = "ar1"
)


# conditional predictions
theme_set(theme_pubclean()) # nice theme





gee_mar_eff12 <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = c("Wave"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  scale_y_continuous(limits = c(-.05, .5)) +
  labs(title = "Marginal counterfactual contrasts in effect magnitude",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Conterfactual contrasts: Warmth to Muslims on difference scale") +
  scale_color_viridis_d() +  theme_pubclean()


gee_mar_eff12
# difference scale
gee_con_eff12 <- plot_cco(
  g_mod,
  effect = "Attack",
  condition = list("Wave",
                   #                  #    Pol.Orient_cZ = "fivenum"),
                   Pol.Orient_cZ = c(-1.92,-1, 0, 1, 2)),
  # #  Pol.Orient_cZ = "threenum"),
  conf_level = 0.95,
  transform_pre = "difference"
) +
  scale_y_continuous(limits = c(-.05, .5)) +
  labs(title = "Conditional counterfactual contrasts by political conservativism",
       subtitle = "NZAVS 2012 Cohort, Times 10- 13, N = 4865",
       y = "Conterfactual contrasts: Warmth to Muslims on difference scale") +
  scale_color_okabe_ito() + theme_pubclean()

gee_con_eff12

gee_plots12 <-
  gee_mar_eff12 + gee_con_eff12 + plot_annotation(tag_levels = "A")

gee_plots12


ggsave(
  gee_plots12,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "gee_results_rev-6all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



## GEE PLOTS

# gee
pl_a_gee12 <-
  plot (ggeffects::ggpredict(g_mod, terms = c("Wave", "Attack")))  +
  scale_color_okabe_ito()  +
  labs(title = "Predicted Muslim Warmth by condition and wave (GEE)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Warmth to Mulsims (1-7 (ordinal)")  +
  scale_fill_okabe_ito() + theme_pubclean() +  scale_y_continuous(limits = c(3.9, 4.5))

# check
pl_a_gee12



ggsave(
  pl_a_gee12,
  path = here::here(here::here("figs")),
  width = 12,
  height = 8,
  units = "in",
  filename = "gee_ord_pred12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

pl_a_gee12

pl_b_gee12 <-
  plot(ggeffects::ggpredict(
    g_mod,
    terms = c("Wave",
              "Attack",
              "Pol.Orient_cZ[-1.92,-1, 0, 1, 2]")
  ),  one.plot = TRUE) +
  scale_color_okabe_ito()  +
  labs(title = "Predicted Muslim Warmth effect modification by political conservativism (GEE)",
       subtitle = "NZAVS 2012 Cohort, Times 10 - 13, N = 4865",
       y = "Warmth to Mulsims 1-7 (ordinal)") +
  scale_y_continuous(limits = c(3.0, 5.2)) +
  scale_fill_okabe_ito(alpha = 1)  +
  facet_wrap( ~ facet, ncol = 5) + theme_pubclean()

pl_b_gee12

gee_mar_eff12 + gee_con_eff12
# graph model
gee_412 <-
  (pl_a_gee12 + gee_mar_eff12) / (pl_b_gee12 + gee_con_eff12) + plot_annotation(tag_levels = "A")
gee_412

dev.off()

ggsave(
  gee_412,
  path = here::here(here::here("figs")),
  width = 16,
  height = 12,
  units = "in",
  filename = "gee_412.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)






# ANCOVA ------------------------------------------------------------------

# save-imputations --------------------------------------------------------
name_error = "sd"

# set N for id counts
id_0 <- m_0a$data$Id
id_1 <- m_1a$data$Id

standata(m_0a)

length(m_0a$data$Id)

# analysis
name <- "yfit_muslim"

fitted_values_0 <-
  predict(m_0a, ndraws = 100)
fitted_values_0


fitted_values_1 <-
  predict(m_1a,  ndraws = 100)


# make df
fitted_values_0 <-
  data.frame(fitted_values_0)
head(fitted_values_0)
head(fitted_values_0)
mean(fitted_values_0$Est.Error)

nrow(fitted_values_0)

fitted_values_0 <-
  data.frame(fitted_values_0)
head(fitted_values_0)
head(fitted_values_0)
mean(fitted_values_0$Est.Error)

# needs to be df
yfit <-
  as.data.frame(fitted_values_0$Estimate)
sd <-
  as.data.frame(fitted_values_0$Est.Error)
# rename
colnames(yfit) <- name
colnames(sd) <- name_error

# data frame
dat_0 <-
  as.data.frame(cbind(Y_orig = standata(m_0a)$Y, standata(m_0a)$X, yfit, id_0, sd)) |>
  mutate(id = as.factor(id_0)) |>
  select(-id_0) |>
  arrange(id, wave)



head(dat_0_wide)
dat_0_wide <- dat_0 |>
  # note changes
  mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                  yfit_muslim,
                                  Y_orig)) |>
  mutate(yfit_ORD = round(yfit_muslim, digits = 0)) |>
  mutate(as = as.factor(rep(0, nrow(dat_0)))) |>
  arrange(id_0, wave) |>
  mutate(
    yfit_ORD_lagS = scale (dplyr::lag(yfit_ORD)),
    yfit_ORD_lead1 =  dplyr::lead(yfit_ORD, n = 1),
    yfit_ORD_lead2 =  dplyr::lead(yfit_ORD, n = 2),
    yfit_ORD_lead3 =  dplyr::lead(yfit_ORD, n = 3)
  ) |>
  filter(wave == 0)

head(dat_0_wide)

length(unique(dat_0_wide$id))

yfit_ORD_lagS <-
  dat_0_wide |>
  dplyr::select(yfit_ORD_lagS, id)


#
# dat_0_wide_u <- dat_0_wide |>
#   select(-yimpute_muslim_lag) |>
#   select(-id_0)
#
# ## Same for 1s
#
# # make df
fitted_values_1 <-
  data.frame(fitted_values_1)

# needs to be df
yfit1 <-
  as.data.frame(fitted_values_1$Estimate)
sd <-
  as.data.frame(fitted_values_1$Est.Error)
#
# # rename
colnames(yfit1) <- name
colnames(sd) <- name_error
#
# # data frame
dat_1 <-
  as.data.frame(cbind(Y_orig = standata(m_1a)$Y, standata(m_1a)$X, yfit1, id_1, sd)) |>
  mutate(id = as.factor(id_1)) |>
  select(-id_1)




#
dat_1_wide <- dat_1 |>
  mutate(yimpute_muslim = if_else(Y_orig == Inf,
                                  yfit_muslim,
                                  Y_orig)) |>
  mutate(yfit_ORD = round(yfit_muslim, digits = 0)) |>
  mutate(as = as.factor(rep(1, nrow(dat_1)))) |>
  arrange(id, wave) |>
  mutate(
    yfit_ORD_lead1 =  dplyr::lead(yfit_ORD, n = 1),
    yfit_ORD_lead2 =  dplyr::lead(yfit_ORD, n = 2),
    yfit_ORD_lead3 =  dplyr::lead(yfit_ORD, n = 3)
  ) |>
  filter(wave == 0)

rm(temp_df)
temp_df  <-
  inner_join(dat_1_wide, yfit_ORD_lagS, by = "id") |> as.data.frame()


#
#
# dat_0_wide_u
# # combine data
#
names(dat_0_wide)
temp_df

temp1_df <-
  semi_join(dat_0_wide, temp_df)

dim(dat_0_wide)


dat_combined_u  <-
  rbind(dat_0_wide, temp_df) |>
  mutate(attack = as.factor(as))

str(dat_combined_u)


dat_combined_u <-
  dat_combined_u |>  mutate(yfit_ORD_lagS = as.numeric (yfit_ORD_lagS)) |> mutate_if(is.matrix, as.vector)



## Save data
arrow::write_parquet(dat_combined_u,
                     here::here(push_mods, "dat_combined_u-12.rds"))

# read data
dat_combined_u <-
  arrow::read_parquet(here::here(push_mods, "dat_combined_u-12.rds"))


anco_0 <-
  lm(yfit_ORD ~ attack * Pol.Orient_cZ  , data = dat_combined_u)
anco_0 |>
  model_parameters()

anco_1 <-
  lm(yfit_ORD_lead1 ~ attack * Pol.Orient_cZ  , data = dat_combined_u)

anco_1 |>
  model_parameters()

anco_2 <-
  lm(yfit_ORD_lead2 ~ attack * Pol.Orient_cZ  , data = dat_combined_u)

anco_2 |>
  model_parameters()

anco_3 <-
  lm(yfit_ORD_lead3 ~ attack * Pol.Orient_cZ  , data = dat_combined_u)

anco_3 |>
  model_parameters()

p_0 <-
  plot(ggeffects::ggpredict(anco_0, terms = "attack")) +  scale_y_continuous(limits = c(3.9, 4.5)) + scale_fill_okabe_ito(alpha = 1)

p_1 <-
  plot(ggeffects::ggpredict(anco_1, terms = "attack")) +  scale_y_continuous(limits = c(3.9, 4.5)) + scale_colour_okabe_ito(alpha = 1)
p_2 <-
  plot(ggeffects::ggpredict(anco_2, terms = "attack")) +  scale_y_continuous(limits = c(3.9, 4.5)) + scale_colour_okabe_ito(alpha = 1)
p_3 <-
  plot(ggeffects::ggpredict(anco_3, terms = "attack")) +  scale_y_continuous(limits = c(3.9, 4.5)) + scale_colour_okabe_ito(alpha = 1)


p_0 + p_1 + p_2 + p_3 + plot_layout(nrow = 1)



plot_cco(anco_0,
         effect = "attack",
         condition = list(Pol.Orient_cZ = c(-1.92,-1, 0, 1, 2.42)))


#  Pol.Orient_cZ = "threenum"),


d0 <-
  ggeffects::ggpredict(anco_0, terms = "attack") |>
  as.data.frame() |>
  mutate(condition = rep(0, 2))

d1 <-
  ggeffects::ggpredict(anco_1, terms = "attack") |>
  as.data.frame() |>
  mutate(condition = rep(1, 2))

d2 <-
  ggeffects::ggpredict(anco_2, terms = "attack") |>
  as.data.frame() |>
  mutate(condition = rep(2, 2))

d3 <-
  ggeffects::ggpredict(anco_3, terms = "attack") |>
  as.data.frame() |>
  mutate(condition = rep(3, 2))


bound <-
  rbind(d0, d1, d2, d3) |>
  mutate(
    lower = conf.low,
    upper = conf.high,
    wave = factor(condition),
    Attack =  factor(x),
    Muslim.Warmth = predicted
  )


bound
pl_anco12 <-
  ggplot(bound, aes(x = wave,
                    y = Muslim.Warmth,
                    color = Attack))  +
  geom_pointrange(
    data = bound,
    mapping = aes(
      x = wave,
      y = Muslim.Warmth,
      ymin = lower,
      ymax = upper
    )
  ) + scale_y_continuous(limits = c(3.9, 4.5)) +
  labs(title = "Predicted Muslim Warmth by condition and wave (ANCOVA)",
       subtitle = "NZAVS 2012 Cohort, Times 9 - 13, N = 4865",
       y = "Warmth to Mulsims 1-7 (ordinal)")  +
  scale_fill_okabe_ito() #+  facet_wrap(~ condition)

pl_anco12

# compare with other estimators



d0a <-
  ggeffects::ggpredict(anco_0, terms = c("attack", "Pol.Orient_cZ[-1.92,-1, 0, 1, 2.42]")) |>
  as.data.frame() |>
  mutate(condition = rep(0, 10))

d1a <-
  ggeffects::ggpredict(anco_1, terms = c("attack", "Pol.Orient_cZ[-1.92,-1, 0, 1, 2.42]")) |>
  as.data.frame() |>
  mutate(condition = rep(1, 10))

d2a <-
  ggeffects::ggpredict(anco_2, terms = c("attack", "Pol.Orient_cZ[-1.92,-1, 0, 1, 2.42]")) |>
  as.data.frame() |>
  mutate(condition = rep(2, 10))

d3a <-
  ggeffects::ggpredict(anco_3, terms = c("attack", "Pol.Orient_cZ[-1.92,-1, 0, 1, 2.42]")) |>
  as.data.frame() |>
  mutate(condition = rep(3, 10))


bounda <-
  rbind(d0a, d1a, d2a, d3a) |>
  mutate(
    lower = conf.low,
    upper = conf.high,
    wave = factor(condition),
    Attack =  factor(x),
    Muslim.Warmth = predicted,
    Pol.Orient_cZ = factor(group)
  )
bounda

pl_b_ancova12 <-
  ggplot(bounda, aes(x = wave,
                     y = Muslim.Warmth,
                     colour = Attack))  + scale_y_continuous(limits = c(3, 5.2)) +
  geom_pointrange(
    data = bounda,
    mapping = aes(
      x = wave,
      y = Muslim.Warmth,
      ymin = lower,
      ymax = upper
    )
  ) +
  labs(title = "Predicted Muslim Warmth effect modification by political orientation (ANCOVA)",
       subtitle = "NZAVS 2013 Cohort, Times 10 - 13, N =7,824",
       y = "Warmth to Mulsims (1-7 (ordinal)") +
  #  scale_y_continuous(limits = c(3.0,5.2)) +
  facet_grid(. ~ Pol.Orient_cZ,   shrink = T) #+
#  scale_colour_okabe_ito()

ancova_plot_cond12 <-
  pl_b_ancova12 + pl_b_gee12 + pl_b12 + plot_annotation(tag_levels = "A")




ancova_plot_marg12 <-
  pl_anco12 + pl_a_gee12 + pl_a12 + plot_annotation(tag_levels = "A")

ancova_plot_marg12

# save plot
ggsave(
  ancova_plot_marg12,
  path = here::here(here::here("figs")),
  width = 24,
  height = 12,
  units = "in",
  filename = "ancova_plot_marg12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


ggsave(
  ancova_plot_cond12,
  path = here::here(here::here("figs")),
  width = 24,
  height = 12,
  units = "in",
  filename = "ancova_plot_cond12.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

dat_combined_u
## Save data
arrow::write_parquet(dat_combined_u,
                     here::here(push_mods, "dat_combined_u-12.rds"))

# read data
dat_combined_u <-
  arrow::read_parquet(here::here(push_mods, "dat_combined_u-12.rds"))





# -------------------------------------------------------------------------



# NOT USED

# FITTED MODEL ------------------------------------------------------------
#
#
# prior_mus_cond  = c(
#   set_prior("normal(0,1)",  class = "b"),
#   set_prior("normal(0,1)", class = "b", dpar = "sigma"),
#   set_prior(
#     "student_t(3, 4, 3)",
#     class = "Intercept",
#     lb = 1,
#     ub = 7
#   ),
#   set_prior("exponential(1)", class = "sd")  # only for raneffs
# )
#
#
#
#
# bform_mus_cond_fit  =   bf(
#   yfit_muslim ~  Attack  *  Wave *  Pol.Orient_cZ + (1 | id),
#   sigma ~ 0 + as,
#   set_rescor(rescor = FALSE)
# )
#
#
#
# bform_mus_cond_ord  =   bf(yfit_ORD ~  Attack  *  Wave *  Pol.Orient_cZ + (1 |
#                                                                              id),
#                            sigma ~ 0 + as,
#                            set_rescor(rescor = FALSE))
#
#
#
# library(MASS)
# d_muslim$fyfit_Ord <- factor(d_muslim$yfit_ORD)
# ord <-
#   MASS::polr(fyfit_Ord ~  Attack  *  Wave *  Pol.Orient_cZ, data = d_muslim)
#
# dev.off()
# library(effects)
# plot(Effect(focal.predictors = c("Attack", "Pol.Orient_cZ"), ord))
#
# plot(Effect(focal.predictors = c("Attack", "Pol.Orient_cZ"), ord))
#
# plot(Effect(focal.predictors = c("quality", "coupon"), model))







#not enough iters
# system.time(
#   m_cond_mus <- brms::brm(
#     backend = "cmdstanr",
#     data = d_muslim,
#     family = "gaussian",
#     bform_mus_cond_ord,
#     prior_mus_cond,
#     init = 0,
#     iter = 10000,
#     file = here::here(push_mods, "m_cond_mus-2012-use-ord-rev6.rds")
#   )
# )

# much faster! won't work with plots
# system.time(
#   m_cond_mus2 <- brms::brm(
#     backend = "cmdstanr",
#     data = d_muslim,
#     family = "gaussian",
#     bform_mus_cond_ord2,
#     prior_mus_cond,
#     init = 0,
#     iter = 10000,
#     file = here::here(push_mods, "m_cond_mus-2012-use-ord-rev6-many-its.rds")
#   )
# # )
