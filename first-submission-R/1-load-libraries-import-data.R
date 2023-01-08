
# Analyisis
# author Joseph Bulbulia: joseph.bulbulia@gmail.com
# created by assembling materials from original analysis into one script.
# libraries
source(here::here("R", "libs.R"))
# if you need to increase vector limit:
# library(usethis)
# usethis::edit_r_environ()




# import data -------------------------------------------------------------
#df <- readRDS(here::here("data_raw", "df"))# orig data

# jittered data for reproduction (NZAVS ethics does not permit original data deposited
# to internet. However, as per note, original data can be obtained, just email c.sibley@auckland.ac.nz or the chair of the U of Auckland ethics committee.

df <- arrow::read_parquet(here::here("data", "dat_synth"))
