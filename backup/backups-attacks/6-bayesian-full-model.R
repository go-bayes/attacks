# Analyisis
# author Joseph Bulbulia: joseph.bulbulia@gmail.com
# created by assembling materials from original analysis into one script.


# packages ----------------------------------------------------------------
source(here::here("R", "libs.R"))




# bayesian model data----------------------------------------------------------

imps_bind <-
  readRDS(here::here("mods", "imps_bind"))


bform =   bf(Ys ~ As  *  Wave + (0 + As || Id),
             sigma ~ 0 + As, set_rescor(rescor = FALSE))


# Data for testing model priors
# make list
imp1 <- as.data.frame(imps_bind$imputations$imp[[1]])
imp2 <- as.data.frame(imps_bind$imputations$imp[[2]])
imp3 <- as.data.frame(imps_bind$imputations$imp[[3]])
imp4 <- as.data.frame(imps_bind$imputations$imp[[4]])
imp5 <- as.data.frame(imps_bind$imputations$imp[[5]])
imp6 <- as.data.frame(imps_bind$imputations$imp[[6]])
imp7 <- as.data.frame(imps_bind$imputations$imp[[7]])
imp8 <- as.data.frame(imps_bind$imputations$imp[[8]])
imp9 <- as.data.frame(imps_bind$imputations$imp[[9]])
imp10 <- as.data.frame(imps_bind$imputations$imp[[10]])



# make test data w/ 22 Ids
i1 <- imp1 %>% arrange(Id, Wave) %>% slice(1:128)
i2 <- imp2 %>% arrange(Id, Wave) %>% slice(1:128)
i3 <- imp3 %>% arrange(Id, Wave) %>% slice(1:128)
i4 <- imp4 %>% arrange(Id, Wave) %>% slice(1:128)
i5 <- imp5 %>% arrange(Id, Wave) %>% slice(1:128)
i6 <- imp6 %>% arrange(Id, Wave) %>% slice(1:128)
i7 <- imp7 %>% arrange(Id, Wave) %>% slice(1:128)
i8 <- imp8 %>% arrange(Id, Wave) %>% slice(1:128)
i9 <- imp9 %>% arrange(Id, Wave) %>% slice(1:128)
i10 <- imp10 %>% arrange(Id, Wave) %>% slice(1:128)


ameliadata <-
  list(imp1, imp2, imp3, imp4, imp5, imp6, imp7, imp8, imp9, imp10)


# set prior
prior = c(
  prior(normal(0.2, 0.25), class = b, coef = "As1"),
  prior(normal(0.05, 0.25), class = b, coef = "Wave"),
  prior(normal(0, 0.25),  class = b, coef = "As1:Wave"),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As0",
    dpar = "sigma"
  ),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As1",
    dpar = "sigma"
  ),
  prior(student_t(3, 4.1, 1), class = Intercept),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As0",
    group = "Id"
  ),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As1",
    group = "Id"
  )
)


# test
system.time(
  m_cluster  <- brms::brm_multiple(
    bform,
    family = gaussian,
    data = ameliadata,
    prior = prior,
    #seed = 1234,
    init = 0,
    warmup = 1000,
    iter =  2000,
    chains = 2,
    future = TRUE
    ,
    file = here::here("mods", "m_cluster.rds")
  )
)


# get priors
prior_summary(m_cluster)

# get model
stancode(m_cluster)



# simulate priors ---------------------------------------------------------
prior = c(set_prior("normal(0, 1)", class = "b"))

bform =   bf(Ys ~ As  *  Wave + (0 + As || Id),
             sigma ~ 0 + As, set_rescor(rescor = FALSE))


sim_prior_strong  <- brm(
  bform,
  family = "gaussian",
  data = imp5,
  prior = c(
    set_prior('normal(0, 1)', class = 'b'),
    set_prior(
      'exponential(1)',
      class = "b",
      dpar = "sigma",
      coef = "As1"
    ),
    set_prior(
      'exponential(1)',
      class = "b",
      dpar = "sigma",
      coef = "As0"
    )
  ),
  sample_prior = "only",
  # only one dataset
  #seed = 1234,
  init = 0,
  warmup = 500,
  iter =  1000,
  chains = 1,
  # backend = "cmdstanr"
  ,
  file = here::here("mods", "sim_prior_strong.rds")
)


sim_prior_strong_plot <-
  plot(conditional_effects(
    sim_prior_strong,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

sim_prior_strong_plot2 <-
  sim_prior_strong_plot$`Wave:As`  + scale_y_continuous(limits = c(-10.0, 15)) +
  labs(subtitle = "Sampling from prior only: weakly regularised",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()


sim_prior_strong_plot2

ggsave(
  sim_prior_strong_plot2,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "sim_prior_strong_plot2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

prior_strong_even = c(
  set_prior('normal(0.2, 0.25)', class = 'b', coef = "As1"),
  set_prior('normal(0.05, 0.25)', class = "b", coef = "Wave"),
  set_prior("normal(0, 0.25)",  class = "b", coef = "As1:Wave"),
  set_prior(
    "normal(0,1)",
    class = "b",
    coef = "As0",
    dpar = "sigma"
  ),
  set_prior(
    "normal(0,1)",
    class = "b",
    coef = "As1",
    dpar = "sigma"
  ),
  set_prior("student_t(3, 4.1, 1)", class = "Intercept"),
  set_prior(
    "student_t(3, 0, 2.5)",
    class = "sd",
    coef = "As0",
    group = "Id"
  ),
  set_prior(
    "student_t(3, 0, 2.5)",
    class = "sd",
    coef = "As1",
    group = "Id"
  )
)

system.time(
  sim_prior_stronger_even  <- brms::brm(
    bform,
    prior = prior_strong_even,
    family = gaussian,
    data = imp5,
    #seed = 1234,
    init = 0,
    warmup = 500,
    iter =  1000,
    chains = 1,
    #backend = "cmdstanr",
    sample_prior = "only"
    ,
    file = here::here("mods", "sim_prior_stronger_even.rds")
  )
)

sim_prior_stronger_evenP <-
  plot(conditional_effects(
    sim_prior_stronger_even,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

sim_prior_stronger_evenPLOT <-
  sim_prior_stronger_evenP$`Wave:As`   + scale_y_continuous(limits = c(-10.0, 15)) +
  labs(subtitle = "Sampling from prior only: moderately regularised",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()


sim_prior_stronger_evenPLOT
ggsave(
  sim_prior_stronger_evenPLOT,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "sim_prior_stronger_evenPLOT.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

library(patchwork)
compare_priors <-
  sim_prior_strong_plot2 + sim_prior_stronger_evenPLOT +
  plot_annotation(tag_levels  = "i", title = "Comparision of wealkly regularised and strongly regularised priors")

compare_priors


ggsave(
  compare_priors,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "compare_priors.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


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
  sim_prior_stronger_even2  <- brms::brm(
    bform,
    prior = prior_strong_even2,
    family = gaussian,
    data = imp5,
    #seed = 1234,
    init = 0,
    warmup = 500,
    iter =  1000,
    chains = 1,
    #backend = "cmdstanr",
    sample_prior = "only"
    ,
    file = here::here("mods", "sim_prior_stronger_even2.rds")
  )
)

sim_prior_stronger_evenP2 <-
  plot(conditional_effects(
    sim_prior_stronger_even2,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

sim_prior_stronger_evenPLOT2 <-
  sim_prior_stronger_evenP2$`Wave:As`   + scale_y_continuous(limits = c(-10.0, 15)) +
  labs(subtitle = "Sampling from prior only: strongly regularised",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()


sim_prior_stronger_evenPLOT2
ggsave(
  sim_prior_stronger_evenPLOT2,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "sim_prior_stronger_evenPLOT.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

library(patchwork)
compare_priors <-
  sim_prior_strong_plot2 + sim_prior_stronger_evenPLOT + sim_prior_stronger_evenPLOT2 +
  plot_annotation(tag_levels  = "i",
                  title = "Comparision of regularised priors:",
                  subtitle = "sampled from the posterior only")

compare_priors

ggsave(
  compare_priors,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "compare_priors.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# model with strongly regularised priors ----------------------------------
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




# bayesian model that we used ---------------------------------------------

system.time(
  m_cluster_st  <- brms::brm_multiple(
    bform,
    family = gaussian,
    data = ameliadata,
    prior = prior_strong_even2,
    #seed = 1234,
    init = 0,
    warmup = 1000,
    iter =  2000,
    chains = 2,
    future = TRUE
    ,
    file = here::here("mods", "m_cluster_st.rds")
  )
)

# get priors for summaries in manuscript

prior_summary(m_cluster_st)

# alternatively
stancode(m_cluster_st)


# Table
tab_cluster_st <-
  lazerhawk::brms_SummaryTable(m_cluster_st, panderize = F)
tab_cluster_st

# table in latex
tab_cluster_st
tab_cluster_st %>%
  kable(booktabs = T,
        "latex",
        caption =  "Marginal effect of attack on warmth to Muslims",
        digits = 2) %>%
  print()

# graph  ------------------------------------------------------------------
pl_m_cluster_st <-
  plot(conditional_effects(
    m_cluster_st,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

library(ggsci)
plot_m_cluster_st <-
  pl_m_cluster_st$`Wave:As`  + scale_y_continuous(limits = c(4.1, 4.5)) +
  labs(subtitle = "Predicted marginal means for prejudice by attack condition",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()


plot_m_cluster_st


ggsave(
  plot_m_cluster_st,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "plot_m_cluster_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# make graphs for the stronger prior  --------------------------------

# bayesplot scheme
color_scheme_set("brightblue")

# graph for areas


## graph of expectation

m1_test_model_plot_st <-
  plot(conditional_effects(
    m_cluster_st,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))

# save graph
#saveRDS(m1_test_model_plot_st, here::here( "mods", "m1_test_model_plot_st"))

m1_test_model_plot_st <-
  readRDS(here::here("mods", "m1_test_model_plot_st"))


# mage graph
cluster_plot_st <-
  m1_test_model_plot_st$`Wave:As`  + scale_y_continuous(limits = c(4.0, 4.5)) +
  labs(subtitle = "",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  theme_classic()


cluster_plot_st <-
  cluster_plot_st + labs(title = "Predicted marginal means for prejudice by attack condition")

### used graph
cluster_plot_st

# sim compare graph
cluster_plot_st_sim <-
  cluster_plot_st +  scale_y_continuous(limits = c(4.05, 4.55)) +
  labs(subtitle = "",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()

cluster_plot_st_sim


ggsave(
  cluster_plot_st,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "cluster_plot_title_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


## compare graph to more weakly regulated priors
cluster_plot_st <-
  m1_test_model_plot_st$`Wave:As`  + scale_y_continuous(limits = c(4.0, 4.5)) +
  labs(subtitle = "Strongly regularising priors",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()

library(patchwork)

weak_strong_priors_prediction <-
  cluster_plot_st + cluster_plot_title + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "i",
                  title = "Comparison of weakly and strongly regularising priors shows little effect in choice of prior")

weak_strong_priors_prediction

ggsave(
  weak_strong_priors_prediction,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "weak_strong_priors_prediction.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# strong prior density overlay and combo plots ----------------------------
# density overlay strong prior
dens_overlay_strong_prior <-
  mcmc_dens_overlay(m_cluster_st, pars  = c("b_As1", "b_Wave", "b_As1:Wave"))

# save graph
saveRDS(dens_overlay_strong_prior,
        here::here("mods", "dens_overlay_strong_prior"))

dens_overlay_strong_prior <-
  readRDS(here::here("mods", "dens_overlay_strong_prior"))

# used
dens_overlay_strong_prior

# areas plot
color_scheme_set("brightblue")

areas_plot_st <- mcmc_plot(
  m_cluster_st,
  variable = c("b_As1", "b_Wave", "b_As1:Wave"),
  #regex_pars = "beta",
  type = 'areas',
  prob = 0.95
)

areas_plot_st

ggsave(
  areas_plot_st,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "areas_plot_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


ggsave(
  dens_overlay_strong_prior,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "dens_overlay_strong_prior.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# strong prior combo plots ------------------------------------------------
plot_post_1 <- plot(
  m_cluster_st,
  variable = c("b_Intercept", "b_As1", "b_Wave", "b_As1:Wave"),
  plot = F,
  ask = T
)

plot_post_2 <- plot(
  m_cluster_st,
  variable = c("b_sigma_As0", "b_sigma_As1", "sd_Id__As0", "sd_Id__As1"),
  plot = F,
  ask = T
)

p1 <- plot_post_1[[1]]
p2 <- plot_post_2[[1]]



pp1 <- p1$bayesplots[[1]] + p1$bayesplots[[2]]
pp2 <- p2$bayesplots[[1]] + p2$bayesplots[[2]]

plot_post_8_st <- plot(
  m_cluster_st,
  variable = c(
    "b_Intercept",
    "b_As1",
    "b_Wave",
    "b_As1:Wave",
    "b_sigma_As0",
    "b_sigma_As1",
    "sd_Id__As0",
    "sd_Id__As1"
  ),
  plot = F,
  newpage = F,
  N = 8
)
plot_post_8_st

# trace plots
trace_plotsS <- plot_post_8_st[[1]]
trace_plotsS


p_trace_plotsS <-
  trace_plotsS$bayesplots[[1]] + trace_plotsS$bayesplots[[2]]

u_trace_plotsS <-
  p_trace_plotsS + labs(title = "MCMC posterior locations + chains")
u_trace_plotsS



# bayesian hypothesis tests

hyp0 <- hypothesis(m_cluster_st, "As1 + As1:Wave > As1 - Wave")
p0 <- plot(hyp0, plot = F)
out_h0 <- p0[[1]] + labs(subtitle = "Post trajectory > 0") +
  #scale_fill_colorblind() +
  theme_classic()
out_h0

hyp1 <- hypothesis(m_cluster_st, "Wave + As1:Wave  =  Wave")
p1 <- plot(hyp1, plot = F)
out_h1 <- p1[[1]] + labs(subtitle = "Post < Pre trajectory") +
  #scale_fill_colorblind() +
  theme_classic()
out_h1



hyp2 <-
  hypothesis(m_cluster_st, "sigma_As0 = sigma_As1", class = "b")
p2 <- plot(hyp2, plot = F)


hyp3 <- hypothesis(m_cluster_st, "As1 >0")

p3 <- plot(hyp3, plot = F)
out_h3 <-
  p3[[1]] + labs(subtitle = "Attack causes acceptance > 0") +
  #scale_fill_colorblind() +
  theme_classic()
out_h3

# combine graph
bayes_hypothesis <- out_h3 +  out_h0 + out_h1
bayes_hypothesis <-
  bayes_hypothesis +   plot_layout(guides = 'collect')

ggsave(
  bayes_hypothesis,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "bayes_hypothesis.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)


#### fig 2
dev.off()

#  (out_h3/ out_h0 / out_h1  )
fig2 <-
  (dens_overlay_strong_prior) /  (areas_plot_st + cluster_plot_st) + plot_annotation(tag_levels = "i")
fig2

ggsave(
  fig2,
  path = here::here(here::here("figs")),
  width = 16,
  height = 09,
  units = "in",
  filename = "fig2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# used this

library(ggpubr)
fig3 <-
  ggarrange(
    areas_plot_st,
    dens_overlay_strong_prior,
    cluster_plot_st,
    bayes_hypothesis,
    labels = c("A", "B", "C", "D"),
    heights = c(2, 2),
    # widths = c(1),
    ncol = 2,
    nrow = 2
  )

fig3

ggsave(
  fig3,
  path = here::here(here::here("figs")),
  width = 15,
  height = 10,
  units = "in",
  filename = "fig3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

# show plot
results_main <-
  cluster_plot_st + (out_h3 / out_h0 / out_h1) + plot_annotation(tag_levels = "i") +
  plot_layout(ncol = 2, widths = c(2, 1))
results_main



ggsave(
  results_main,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results_main.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)



results_use <- areas_plot_st + dens_overlay_strong_prior  +
  plot_annotation(tag_levels = "i")

results_use

ggsave(
  results_use,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "results_use.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

# ppchecks # assumptions not met
pp_check(m_cluster_st, nsamples = 20)


## table
lazerhawk::brms_SummaryTable(m_cluster)

# mcmc chains
color_scheme_set("brightblue")
#color_scheme_set("brewer-Spectral")
#color_scheme_set("mix-blue-red")
#plot(m_cluster)

plot_post_1 <- plot(
  m_cluster_st,
  variable = c("b_Intercept", "b_As1", "b_Wave", "b_As1:Wave"),
  plot = F,
  ask = T
)

plot_post_2 <- plot(
  m_cluster_st,
  variable = c("b_sigma_As0", "b_sigma_As1", "sd_Id__As0", "sd_Id__As1"),
  plot = F,
  ask = T
)

p1 <- plot_post_1[[1]]
p2 <- plot_post_2[[1]]

<- plot_post_1[[1]]$bayesplots

pp1 <- p1$bayesplots[[1]] + p1$bayesplots[[2]]
pp2 <- p2$bayesplots[[1]] + p2$bayesplots[[2]]
pp2


plot_post_8 <- plot(
  m_cluster_st,
  variable = c(
    "b_Intercept",
    "b_As1",
    "b_Wave",
    "b_As1:Wave",
    "b_sigma_As0",
    "b_sigma_As1",
    "sd_Id__As0",
    "sd_Id__As1"
  ),
  plot = F,
  newpage = F,
  N = 8
)
plot_post_8


library(patchwork)
library(ggplot2)
trace_plots <- plot_post_8[[1]]
p_trace_plots <-
  trace_plots$bayesplots[[1]] + trace_plots$bayesplots[[2]]
u_trace_plots <-
  p_trace_plots + labs(title = "MCMC posterior locations + chains")
u_trace_plots

saveRDS(p_trace_plots,
        here::here("mods", "trace_plots"))

ggsave(
  u_trace_plots,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "u_trace_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# ppchecks # assumptions not met
pp_check(m_cluster_st, nsamples = 20)


# sensitivity anlaysis ----------------------------------------------------
# imagine strong time effect


priorS = c(
  prior(normal(0.2, 0.25), class = b, coef = "As1"),
  set_prior("constant(0.12)", class = "b", coef = "Wave"),
  prior(normal(0, 0.25),  class = b, coef = "As1:Wave"),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As0",
    dpar = "sigma"
  ),
  prior(
    normal(log(1), 1),
    class = b,
    coef = "As1",
    dpar = "sigma"
  ),
  prior(student_t(3, 4.1, 1), class = Intercept),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As0",
    group = "Id"
  ),
  prior(
    student_t(3, 0, 2.5),
    class = sd,
    coef = "As1",
    group = "Id"
  )
)

b_sens_cluster <- brms::brm(
  bform,
  data = ameliadata,
  prior = priorS,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 1,
  future = TRUE,
  # backend = "cmdstanr",
  #save_pars=save_pars(group=FALSE))
  file = here::here("mods", "b_sens_cluster")
)

prior_summary(b_sens)

summary_b_sens_cluster <- summary(b_sens_cluster)
summary(b_sens_cluster)



tab_bsense_cluster <-
  lazerhawk::brms_SummaryTable(b_sens_cluster, panderize = F)

tab_bsense_cluster %>%
  kable(booktabs = T,
        "latex",
        caption =  "Sensitivity for effect of attack on warmth to Muslims: baseline trajectory fixed 2 $\times$ stronger than estimate",
        digits = 2) %>%
  print()


b_sens_cluster <-
  plot(conditional_effects(
    b_sens_cluster,
    "Wave:As",
    ndraws = 200,
    spaghetti = T
  ))
#saveRDS(b_sens_cluster, here::here( "mods", "b_sens_cluster"))
b_sens_cluster <- readRDS(here::here("mods", "b_sens_cluster"))



sens_plot <-
  b_sens_cluster$`Wave:As`  + scale_y_continuous(limits = c(4.0, 4.5)) +
  labs(subtitle = "",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .5) +
  theme_classic()



# plot sensitivity --------------------------------------------------------


sens_plot_title <-
  cluster_plot_st + sens_plot + labs(title = "Sensitivity analysis: predicted marginal means for prejudice by attack condition",
                                     subtitle = "Assuming 2 x baseline acceptance growth rate does not diminish post-attack growth rate")
sens_plot_title

dev.off()

ggsave(
  sens_plot_title,
  path = here::here(here::here("mods")),
  width = 16,
  height = 9,
  units = "in",
  filename = "sens_plot_title.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

# comparative graph bayesian with REML ------------------------------------


plot_bayes_3  <-
  m1_test_model_plot$`Wave:As`  + scale_y_continuous(limits = c(4.0, 4.5)) +
  labs(subtitle = "Bayesian MI from six previous waves + full attack wave",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/21; N = 47948") +
  scale_color_npg(alpha = .8) +
  # scale_colour_fivethirtyeight(alpha = .5) +
  #scale_colour_okabe_ito(alpha =.5) +
  theme_classic()




