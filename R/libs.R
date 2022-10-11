# libraries
# packages ----------------------------------------------------------------
#libraries required for analysis
library("here") # file management
library("equatiomatic") # equations
library("lubridate") # working with dates
library("ggplot2") # graphs
library("ggthemes") #themes
library("Amelia") # missing data imputation
library("patchwork") # combine graphs
library("lme4") # multilevel estimation
library("ggpubr") # graphs
library("easystats") # reporting
library("kableExtra") # tables
library("broom") # working with  models
library("broom.mixed") # mixed effects models
library("brms") # bayesian estimation
library("rstan") # backend brms
library("rstanarm") # graphing
library("cmdstanr") # backend brms', not used
library("tidybayes") # workign with posterior probability distributions
library("bayesplot") # graphs
library("ggokabeito")   # color palette
library("gghalves")     #  half geoms, not used
library("ggbeeswarm")   # Special distribution-shaped point jittering, not used
library("emmeans") # estimate marginal means, not used
library("table1") # tables /now with latex
library("tidyverse") # data wrangling
library("sjstats") # stat summaries
library("magick") # images
library("simstudy") # simulation
library("future") #parrallel processing
library("brms") # bayesian estimation
library("ggpubr") # graphs
library("ggsci") # graphs
library("magick")  # manipulate dags
library("splines") ## rdd analysis
library("mgcv")# rdd analysis
library("rdrobust")# rdd analysis
library("stargazer")# rdd analysis



# rstan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores ())
