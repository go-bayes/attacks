# Simpulation with fake data

# SIMULATE data using simstudy --------------------------------------------

# libraries
source(here::here("R", "libs.R"))


# how many from year 4 in year 10
in10 <- df %>%
  dplyr::select(Id,
                Wave,
                TSCORE,
                Warm.Muslims,
                YearMeasured,) %>%
  dplyr::filter(
    Wave == 2012 & YearMeasured == 1 |
      Wave == 2013 & YearMeasured != -1 |
      Wave == 2014 & YearMeasured != -1 |
      Wave == 2015 & YearMeasured != -1 |
      Wave == 2016 & YearMeasured != -1 |
      Wave == 2017 & YearMeasured != -1 |
      Wave == 2018 & YearMeasured != -1
  ) %>%
  dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  droplevels() %>%
  dplyr::mutate(org2012 =  ifelse(Wave == 2012 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2012, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack to enable repeate of baseline in 2019
  ungroup(Id) %>%
  arrange(Id, Wave) %>%
  group_by(Id) %>%
  dplyr::mutate(TSCORE_b = ifelse(Wave == "2012", (TSCORE), NA_real_)) %>%
  fill(TSCORE_b) %>%
  dplyr::mutate(TSCORE_i = ifelse(
    YearMeasured == 0 & Wave == 2013,
    TSCORE_b + 365,
    ifelse(
      YearMeasured == 0 & Wave == 2014,
      TSCORE_b + 730,
      ifelse(
        YearMeasured == 0 & Wave == 2015,
        TSCORE_b + 1094,
        # leap
        ifelse(
          YearMeasured == 0 & Wave == 2016,
          TSCORE_b + 1459,
          ifelse(YearMeasured == 0 &
                   Wave == 2017, TSCORE_b + 1824, TSCORE)
        )
      )
    )
  )) %>%
  dplyr::mutate(Attack = as.numeric(ifelse(TSCORE_i >= 3545, 1, 0))) %>% # All 0
  ungroup(Id)

#
table1::table1( ~ Warm.Muslims | Wave * Attack, data = in10)
# 5142
#
6382 / (41566 + 1647) # % missing in Wave 10 ~ 15

library(simstudy)
set.seed(281726)
rm(tdat)
tdat <- km_all3 %>% filter(YearMeasured == 1)
# inspect long data
table1::table1( ~ Warm.Muslims | Wave , data = all_d)
table1::table1( ~ Warm.Muslims | Wave * Attack, data = tdat)


#tdef <- defData(varname = "m", dist = "binary", formula = 0.5)
rm(tdef) # remove in case object exists

tdef <-
  defData(
    varname = "Y0",
    dist = "normal",
    formula = 4.4,
    variance = 2,
    id = "id"
  )
tdef <-
  defData(
    tdef,
    varname = "Y1",
    dist = "normal",
    formula = "Y0 +  (Y0 * 0.01)",
    variance = 0,
    id = "id"
  )
tdef <-
  defData(
    tdef,
    varname = "Y2",
    dist = "normal",
    formula = "Y1 +  (Y1 * 0.01)",
    variance = 0,
    id = "id"
  )


dtTrial <- genData(47948, tdef)
head(dtTrial)

# check
table1::table1( ~ Y0 + Y1 + Y2, data = dtTrial)

# make long
dtTime <-
  addPeriods(
    dtTrial,
    nPeriods = 3,
    idvars = "id",
    timevars = c("Y0",
                 "Y1",
                 "Y2"),
    timevarName = "Y"
  )
dtTime



MCAR <-
  defMiss(
    varname = "Y",
    formula = "-1.3",
    # just over 20% attrition
    logit.link = TRUE,
    monotonic = TRUE
  )
dm <-
  genMiss(dtTime, MCAR, "id", repeated = TRUE, periodvar = "period")

dObs <- genObs(dtTime, dm, idvars = "id")

# check
dObs[, .(prop.missing = mean(is.na(Y))), keyby = period]

table1::table1( ~ Y | as.factor(period), data = dObs)

dObs
## Test imputation method for 1s
library(Amelia)

imputed_sim1 <- amelia(
  set.seed = 1234,
  dObs,
  cs = c("id"),
  ts = c("period"),
  m = 10,
  # number of imputations
  # idvars=c(""),
  lags = "Y",
  leads = "Y",
  polytime = 2,
  #  polynomials perhaps not sensible given
  intercs = F,
  # to many vars
  # bounds = bds, # lower upper bounds to Mus Prej
  empri = .05 * nrow(dObs)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed_sim1,
        here::here("mods", "imputed_sim1"))
imputed_sim1 <-
  readRDS(here::here("mods", "imputed_sim1"))

table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim1$imputations$imp1,
                overall = FALSE)

# test
imputed_sim1$imputations$imp1$Y


# model
m <- 10
model_sim1 <- NULL
for (i in 1:m) {
  model_sim1$model[[i]] <-
    lmer(Y ~ period + (1 |
                         id), data = imputed_sim1$imputations[[i]])
}

# table
tab_sim1 <- pool_parameters(model_sim1$model)
tab_sim1 # recover effect
tab_sim1 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab_sim1, show_labels = TRUE)



fitted_lines_sim1 <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_sim1$model[[.]], terms = c("period[0:2, by =.01]")
  ))) %>%
  data_frame() %>%
  unnest()

library(ggsci)

plot_sim1 <- fitted_lines_sim1 %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.0, 4.6)) +
  labs(subtitle = "MI from previous six previous waves",
       y = "Muslim Warmth",
       x = "Waves: 2012-2020/21, N = 11799") +   scale_colour_npg(alpha =
                                                                    .5) + theme_classic() # +
#scale_colour_okabe_ito(alpha=.5)


plot_sim1




## create data
m <- 10
self1 <- NULL
for (i in 1:m) {
  self1$imputations$imp[[i]] <- imputed_sim1$imputations[[i]] %>%
    dplyr::mutate(a = as.factor(rep(1, nrow(.)))) %>%
    dplyr::arrange(period, id)
}
str(self1$imputations$imp[[1]])
# SIMULATE ZEROS ----------------------------------------------------------

#Generate data with complete missingness for the zeros



set.seed(281726)

# move data (in case there)
rm(ztdef)

# define yearly outcomes
ztdef <-
  defData(
    varname = "Y0",
    dist = "normal",
    formula = 3.7,
    variance = 1
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y1",
    dist = "normal",
    formula = "Y0 +  (Y0 * 0.025)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y2",
    dist = "normal",
    formula = "Y1 +  (Y1 * 0.025)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y3",
    dist = "normal",
    formula = "Y2 +  (Y2 * 0.02)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y4",
    dist = "normal",
    formula = "Y3 +  (Y3 * 0.02)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y5",
    dist = "normal",
    formula = "Y4 +  (Y4 * 0.015)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y6",
    dist = "normal",
    formula = "Y5 +  (Y5 * 0.015)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y7",
    dist = "normal",
    formula = "Y6 +  (Y6 * 0.015)",
    variance = 0
  )
ztdef <-
  defData(
    ztdef,
    varname = "Y8",
    dist = "normal",
    formula = "Y7 +  (Y7 * 0.015)",
    variance = 0
  )



head(ztdef)

# Missing
#defM <- defMiss(varname = "Y7", formula = 1, logit.link = FALSE)
#defM <- defMiss(defM, varname = "Y8", formula = 1, logit.link = FALSE)


#5142/(5142 + 2020) 72 percent

# create data
zdtTrial <- genData(12179, ztdef)

# create NA columens

# zdtTrial$Y7 <- rep(NA, nrow(zdtTrial))
# zdtTrial$Y8 <- rep(NA, nrow(zdtTrial))

# make logical values numeric
zdtTrial <- zdtTrial %>%
  mutate(across(where(is.logical), as.numeric)) # make all numeric

# check
table1::table1( ~ Y0 + Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8, data = zdtTrial)

# save
saveRDS(zdtTrial, here::here("mods", "zdtTrial"))

# check




# zdtTrial$Y8 <- rep(NA, nrow(zdtTrial))




# Next crate create data for T10
id <- seq(12180, 41566 + 6382, by = 1)
n <- length(id)

Y0 <- rep(NA, n)
Y1 <- rep(NA, n)
Y2 <- rep(NA, n)
Y3 <- rep(NA, n)
Y4 <- rep(NA, n)
Y5 <- rep(NA, n)
Y6 <- rnorm(n = n, mean = 4.15, sd = 1.4)
Y7 <- rep(NA, n)
Y8 <- rep(NA, n)

dx0 <- data.frame(id, Y0, Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8)

str(dx0)
# create missing vals
d0 <- dx0 %>%
  mutate(missing = rbinom(n(), size = 1, prob = 0.15)) %>%
  mutate(Y7 = ifelse(missing == 1, NA, Y7)) %>%
  select(-missing) %>%
  mutate(across(where(is.logical), as.numeric)) # make all numeric

d0 <- data.table::as.data.table(d0)
str(d0)

tbig <- addPeriods(
  d0,
  nPeriods = 9,
  idvars = "id",
  timevars = c("Y0",
               "Y1",
               "Y2",
               "Y3",
               "Y4",
               "Y5",
               "Y6",
               "Y7",
               "Y8"),
  timevarName = "Y"
)


# make long
zdtTime <-
  addPeriods(
    zdtTrial,
    nPeriods = 9,
    idvars = "id",
    timevars = c("Y0",
                 "Y1",
                 "Y2",
                 "Y3",
                 "Y4",
                 "Y5",
                 "Y6",
                 "Y7",
                 "Y8"),
    timevarName = "Y"
  )

# merge data
zdtTime



MCAR <-
  defMiss(
    varname = "Y",
    formula = "-1.3",
    # just over 20% attrition
    logit.link = TRUE,
    monotonic = TRUE
  )


zdm <-
  genMiss(zdtTime, MCAR, "id", repeated = TRUE, periodvar = "period")



zdObs[, .(prop.missing = mean(is.na(Y))), keyby = period]

table1::table1( ~ Y | as.factor(period), data = zdObs)

tzdObs <- zdObs %>%
  arrange(Y, id) %>%
  group_by(period) %>%
  mutate(Y = ifelse(period == 7 |
                      period == 8, recode(Y, NA), Y))

table1::table1( ~ Y | as.factor(period), data = tzdObs)


om <- rbind(tzdObs, tbig)

head(om)
length(unique(om$id))


# Inspect data
table1::table1( ~ Y | as.factor(period), data = om)
str(om)
om <- om %>%
  select(id, period, Y)

saveRDS(om, here::here("mods", "om"))

## Test imputation method for 1s
library(Amelia)
str(om)
om <- as.data.frame(om)
imputed_sim0 <- amelia(
  set.seed = 1234,
  om,
  cs = c("id"),
  ts = c("period"),
  m = 10,
  # number of imputations
  # idvars=c(""),
  lags = "Y",
  leads = "Y",
  polytime = 2,
  #  polynomials otherwise regression to the mean
  intercs = F,
  # to many vars
  # bounds = bds, # lower upper bounds to Mus Prej
  empri = .05 * nrow(om)
) # ridge prior see: Amelia pdf documentation p.23

# save data
saveRDS(imputed_sim0,
        here::here("mods", "imputed_sim0"))

table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Y |
                  as.factor(period),
                data = imputed_sim0$imputations$imp1,
                overall = FALSE)

# compare
table1::table1( ~ Y | as.factor(period), data = zdObs)


# Select only final three waves
m <- 10
self0 <- NULL
for (i in 1:m) {
  self0$imputations$imp[[i]] <- imputed_sim0$imputations[[i]] %>%
    dplyr::filter(period == 6 |
                    period == 7 | period == 8) %>%
    dplyr::mutate(a = as.factor(rep(0, nrow(.)))) %>%
    dplyr::mutate(period = period - 6) %>%
    dplyr::arrange(period, id)
}



# model
m <- 10
model_sim0 <- NULL
for (i in 1:m) {
  model_sim0$model[[i]] <-
    lmer(Y ~ period + (1 |
                         id), data =  self0$imputations$imp[[i]])
}

# table
tab_simzero <- pool_parameters(model_sim0$model)
tab_simzero
tab_simzero [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab_simzero, show_labels = TRUE)


## create full data
m <- 10
model_sim_full <- NULL
for (i in 1:m) {
  model_sim_full$imputations$imp[[i]] <-
    dplyr::bind_rows(self0$imputations$imp[[i]],
                     self1$imputations$imp[[i]]) %>%
    dplyr::rename(wave = period) %>%
    arrange(wave, id)
}

str(model_sim_full$imputations$imp[[1]])
## replicate model

# save data
sim_list <- model_sim_full$imputations$imp
str(sim_list)
str(listbayes)

# saveRDS(self0, here::here("_posts","mus","mods", "self0"))
# saveRDS(self1, here::here("_posts","mus","mods", "self1"))
# saveRDS(model_sim_full, here::here("_posts","mus","mods", "model_sim_full"))
# saveRDS(sim_list, here::here("_posts","mus","mods", "sim_list"))
sim_list <-
  readRDS(here::here("mods", "sim_list"))
model_sim_full <-
  readRDS(here::here("mods", "model_sim_full"))

library(lme4)

m <- 10
model_sim_rep <- NULL
for (i in 1:m) {
  model_sim_rep$model[[i]] <-
    lmer(Y ~ a * wave + (1 |
                           id), data =  model_sim_full$imputations$imp[[i]])
}

# table
tab <- parameters::pool_parameters(model_sim_rep$model)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)

ggeffects::ggemmeans(model_sim_rep$model[[2]], terms = c("wave", "a"))

library(dplyr)
library(tidyr)
library(broom)


fitted_lines_simfull <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_sim_rep$model[[.]], terms = c("wave[0:2,by=.01]", "a")
  ))) %>%
  data_frame() %>%
  unnest()


library(ggsci)
plot_simall <- fitted_lines_simfull %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) + theme_clean() +  scale_y_continuous(limits =
                                                                  c(4.05, 4.55)) +
  labs(subtitle = "Multiple imputation with simulated data",
       y = "Muslim Warmth",
       x = "Waves: 2012-2020/21, Simulated N = 47,978") +   scale_colour_npg(alpha =
                                                                               .5) + theme_classic()  +
  scale_colour_npg(alpha = .5)



# compare with bayesian model

# cluster plot in "futures.R"
cluster_plot_st_sim2 <-
  cluster_plot_st_sim +   labs(subtitle = "Bayesian model with real data",
                               y = "Muslim Warmth",
                               x = "Waves: 2012-2020/21, N = 47,978")

sim_plot <- cluster_plot_st_sim2 + plot_simall +
  plot_annotation(tag_levels = "i",  title = "Simulated data that assumes even greater missingness nevertheless recovers approximate data model slopes")

sim_plot

ggsave(
  sim_plot,
  path = here::here(here::here("figs")),
  width = 16,
  height = 9,
  units = "in",
  filename = "sim_plot.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# compare with full data & expected values
table1::table1( ~ Y0 + Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8, data = zdtTrial)



ggsave(
  plot_simall,
  path = here::here(here::here("figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_simall.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)


compare_graph <-
  plot_3 + plot_bayes_3 + plot_annotation(tag_levels = "i",
                                          title = "No practical difference between frequentist and Bayesian estimation") + plot_layout(guides = 'collect')

ggsave(
  compare_graph,
  path = here::here(here::here("figs")),
  width = 12,
  height = 9,
  units = "in",
  filename = "compare_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)
