# Analyisis
# author Joseph Bulbulia: joseph.bulbulia@gmail.com
# created by assembling materials from original analysis into one script.


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
library("emmeans") # estimate marginal means
library("table1") # tables /now with latex
library("tidyverse") # data wrangling
library("sjstats") # stat summaries
library("magick") # images
library("simstudy") # simulation
library("future") #parrallel processing
library("brms") # bayesian estimation
library("ggpubr") # graphs
library("ggsci") # graphs

# if you need to increase vector limit:
# library(usethis)
# usethis::edit_r_environ()

# rstan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores ())

# import data -------------------------------------------------------------
#df <- readRDS(here::here("data_raw", "df"))# orig data

# jittered data for reproduction (NZAVS ethics does not permit original data deposited
# to internet. However, as per note, original data can be obtained, just email c.sibley@auckland.ac.nz or the chair of the U of Auckland ethics committee.

df <- readRDS(here::here("data", "df_s"))


### Analysis is now broken down into individual R scripts.  Best to run these sequentially.  Note that the key imputation is done in script #5 and the bayesian anlysis (and sensitivity analysis) is in script #6.


## To tidy further before publication
