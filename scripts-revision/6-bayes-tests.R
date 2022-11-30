# Analyisis
# author Joseph Bulbulia: joseph.bulbulia@gmail.com
# created by assembling materials from original analysis into one script.

options(scipen = 999)
#libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

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


# packages ----------------------------------------------------------------
source(here::here("R", "libs.R"))

d_muslim <- arrow::read_parquet(
                     here::here(push_mods, "d_muslim.rds"))

# # overweight gcomp ------------------------------------------------------------
#

prior_muslim = c(
  set_prior("normal(0,.5)",  class = "b"),
  set_prior("normal(0,1)", class = "b", dpar = "sigma"),
  set_prior(
    "student_t(3, 4, 2)",
    class = "Intercept",
    lb = 1,
    ub = 7
  ),
  set_prior("exponential(1)", class = "sd")  # only for raneffs
)




bform_mus_marg  =   bf(yimpute_muslim |mi(se) ~  as  *  wave  + (1|id),
                       sigma ~ 0 + as, set_rescor(rescor = FALSE))



system.time(
  sample_prior <- brms::brm(
    backend = "cmdstanr",
    data = d_muslim,
    family = "gaussian",
    bform_mus_marg,
    prior_re,
    init = 0,
    warmup = 500,
    iter =  1000,
    chains = 1,
    sample_prior = "only",
    file = here::here(push_mods,"m_marg_mus-sample-prior.rds")
  )
)


# graph
plot_sample_prior <-
  plot(conditional_effects(
    sample_prior,
    "wave:as",
    ndraws = 500,
    spaghetti = T
  ))

plot_sample_prior <-
  plot_sample_prior  + scale_y_continuous(limits = c(-1.0, 10)) +
  labs(subtitle = "Sampling from prior only: moderately regularised",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/22; N = 13,403") +
  scale_color_okabe_ito(alpha = .2) +
  theme_classic()


plot_sample_prior

ggsave(
  plot_sample_prior,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "plot_sample_prior-rev.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)




# bayesian model used -----------------------------------------------------


system.time(
  m_cond_mus <- brms::brm(
    backend = "cmdstanr",
    data = d_muslim,
    family = "gaussian",
    bform_mus_cond,
    prior_re,
    init = 0,
    file = here::here(push_mods,"m_cond_mus.rds")
  )
)


prior_summary(m_cond_mus)

# alternatively
stancode(m_cond_mus)


# Table
tab_mod <-
  lazerhawk::brms_SummaryTable(m_cond_mus, panderize = F)

tab_mod

# table in latex

tab_mod %>%
  kable(booktabs = T,
        "latex",
        caption =  "Marginal effect of attack on warmth to Muslims",
        digits = 2) %>%
  print()






# graph  ------------------------------------------------------------------
plot_mod <-
  plot(conditional_effects(
    m_cond_mus,
    "Wave:Attack",
    ndraws = 200,
    spaghetti = T
  ))

library(ggsci)
plot_mod_st <-
  plot_mod$`Wave:Attack`  + scale_y_continuous(limits = c(4.0, 4.5)) +
  labs(subtitle = "Predicted marginal means for prejudice by attack condition",
       y = "Muslim Warmth",
       x = "Years: 2018-2020/22; N = 13,403") +
  scale_colour_okabe_ito(alpha =1) +
  theme_classic()


plot_mod_st


ggsave(
  plot_mod_st,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "coeff_simp-plot-rev.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



# make graphs for the stronger prior  --------------------------------

# bayesplot scheme
color_scheme_set("brightblue")

# graph for areas

# ppchecks # assumptions not met
pp_check(m_cond_mus, nsamples = 20)






