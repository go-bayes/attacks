# What if we assume, falsely, that muslim acceptance is not changing over time?
# Here we recreate that analysis, and show that it leads to biaas.

# libraries
source(here::here("R", "libs.R"))


df <- readRDS(here::here("data", "ka3"))



(ka3 <- readRDS(here::here( "mods", "ka3"))

# impute stationary  ---------------------------------------------------------------
km_zero_st18 <- ka3 %>%
  filter((As == 0 &  Wave == "Time10")) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs,
    TSCORE_i
  ) %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  arrange(Wave, Id)


summary(km_zero_st18$Wave)



#check looks good
table1::table1( ~ Ys |
                  Wave * As, data = km_zero_st18, overall = FALSE)
head(km_zero_st18)
dim(km_zero_st18)
# make data frame
km_zero_st18 <- as.data.frame(km_zero_st18)

# create bounds for Ys
head(km_zero_st18)
# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_st18 <- amelia(
  set.seed = 1234,
  km_zero_st18,
  # cs= c("Id"),
  # ts= c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys", "yrs", "TSCORE_i", "Id", "wave"),
  intercs = F,
  bounds = bds,
  empri = .05 * nrow(km_zero_st18)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_st18,
        here::here("mods", "imputed0_st18"))

table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st18$imputations$imp10,
                overall = FALSE)



km_zero_st19 <- ka3 %>%
  filter((As == 0 &
            Wave == "Time10") |
           (As == 0 & Wave == "Time11")) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs,
    TSCORE_i
  ) %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  arrange(Wave, Id)



summary(km_zero_st19$wave)
#check looks good
table1::table1( ~ Ys |
                  Wave * As, data = km_zero_st19, overall = FALSE)
head(km_zero_st19)
dim(km_zero_st19)


# make data frame
km_zero_st19 <- as.data.frame(km_zero_st19)

# create bounds for Ys
head(km_zero_st19)

# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_st19 <- amelia(
  set.seed = 1234,
  km_zero_st19,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys", "yrs", "TSCORE_i"),
  lags = "Ys",
  #leads="Ys",
  intercs = F,
  bounds = bds,
  empri = .05 * nrow(km_zero_st19)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_st19,
        here::here("mods", "imputed0_st19"))

table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st19$imputations$imp10,
                overall = FALSE)



km_zero_st20 <- ka3 %>%
  filter((As == 0 &
            Wave == "Time10") |
           (As == 0 & Wave == "Time12")) %>%
  droplevels() %>%
  dplyr::select(
    Id,
    Wave,
    As,
    Ys,
    pol_bz,
    rel_bz,
    partner_bz,
    parent_bz,
    nzdep_bz,
    male_2z,
    employed_bz,
    edu_bz,
    ubran_bz,
    EthnicCats_b,
    GenCohort,
    dys,
    yrs,
    TSCORE_i
  ) %>%
  mutate(wave = as.numeric(Wave) - 1) %>%
  arrange(Wave, Id)





summary(km_zero_st20$wave)
#check looks good
table1::table1(~ Ys |
                 Wave * As, data = km_zero_st20, overall = FALSE)
head(km_zero_st20)
dim(km_zero_st20)
# make data frame
km_zero_st20 <- as.data.frame(km_zero_st20)

# create bounds for Ys
head(km_zero_st20)
# bounds for Muslim Warm
bds <- matrix(c(4, 1, 7), nrow = 1, ncol = 3)

imputed0_st20 <- amelia(
  set.seed = 1234,
  km_zero_st20,
  cs = c("Id"),
  ts = c("wave"),
  m = 10,
  # number of imputations
  ords = "Ys",
  noms = c("EthnicCats_b", "GenCohort"),
  idvars = c("Wave", "As", "dys", "yrs", "TSCORE_i"),
  lags = "Ys",
  #leads="Ys",
  intercs = F,
  bounds = bds,
  # lower upper bounds to Mus Prej
  empri = .05 * nrow(km_zero_st20)
) # ridge prior see: Amelia pdf documentation p.23

saveRDS(imputed0_st20,
        here::here("mods", "imputed0_st20"))


table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp1,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp2,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp3,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp4,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp5,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp6,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp7,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp8,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp9,
                overall = FALSE)
table1::table1( ~ Ys |
                  Wave * As,
                data = imputed0_st20$imputations$imp10,
                overall = FALSE)


## Get ones from above

imp1 <- transform(imputed1, Wave = as.character(Wave))
imp18z <- transform(imputed0_st18, Wave = as.character(Wave))
imp19z <- transform(imputed0_st19, Wave = as.character(Wave))
imp20z <- transform(imputed0_st20, Wave = as.character(Wave))

str(imp20z$imputations$imp1$Wave)

# bind data frames --------------------------------------------------------


newlevels = c("Time10", "Time11", "Time12")


m <- 10
one_st <- NULL
for (i in 1:m) {
  one_st$imputations$imp[[i]] <- imp1$imputations[[i]] %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, newlevels)) %>%
    droplevels() %>%
    dplyr::group_by(Id) %>%
    arrange(Wave, Id)
}

zero_st <- NULL

for (i in 1:m) {
  zero_st$imputations$imp[[i]] <-
    dplyr::bind_rows(imp18z$imputations[[i]],
                     imp19z$imputations[[i]],
                     imp20z$imputations[[i]]) %>%
    dplyr::mutate(Wave = as.factor(Wave)) %>%
    dplyr::mutate(Wave = forcats::fct_relevel(Wave, newlevels)) %>%
    arrange(Wave, Id)

}

m <- 10
imps_bind_st <- NULL
for (i in 1:m) {
  imps_bind_st$imputations$imp[[i]] <-
    dplyr::bind_rows(zero_st$imputations$imp[[i]],
                     one_st$imputations$imp[[i]]) %>%
    dplyr::select(-wave) %>%
    dplyr::filter(Wave == "Time10" |
                    Wave == "Time11" | Wave == "Time12") %>%
    droplevels() %>%
    dplyr::mutate(Wave = as.numeric(Wave) - 1) %>%
    dplyr::arrange(Wave, Id)


}
imps_bind_st

# Works
summary(imps_bind_st$imputations$imp[[1]]$Wave)

# save
saveRDS(imps_bind_st,
        here::here("mods", "imps_bind_st"))


# read
imps_bind_st <-
  readRDS(here::here("mods", "imps_bind_st"))

# make list for bayesian models
listbayesST <- imps_bind_st$imputations$imp

# save list for bayesian models
saveRDS(listbayesST,
        here::here("mods", "listbayesST"))

#readRDS
listbayesST <-
  readRDS(here::here("mods", "listbayesST"))


# ML model  stationary ----------------------------------------------------------------

# model
m <- 10
model_all_st <- NULL
for (i in 1:m) {
  model_all_st$model[[i]] <-
    lmer(Ys ~ As * Wave + (1 |
                             Id), data = imps_bind_st$imputations$imp[[i]])
}

# table
tab <- pool_parameters(model_all_st)
tab
tab [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)

plot(tab, show_labels = TRUE)


# Average over uncertainty

fitted_linesST <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all_st$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()

plot_st <- fitted_linesST %>%
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
                                                                  c(4.0, 4.5)) +
  labs(subtitle = "Multiple imputation: no previous waves",
       y = "Muslim Warmth",
       x = "Years: 2012-2020/21, N = 47948") + scale_colour_okabe_ito(alpha = .5)


plot_st


fitted_linesST <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all_st$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()


plot_st <- fitted_linesST %>%
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
                                                                  c(4.0, 4.5)) +
  labs(subtitle = "MI: no previous waves",
       y = "Muslim Warmth",
       x = "Years: 2012-2020/21, N = 47948") +   scale_colour_npg(alpha =
                                                                    .5) +  theme_classic()#+
scale_colour_okabe_ito(alpha = .5)


plot_st



ggsave(
  plot_st,
  path = here::here(here::here("mods")),
  # too large
  width = 12,
  height = 9,
  units = "in",
  filename = "plot_st.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 800
)

# COMBINED GRAPH ----------------------------------------------------------


robust_waves <-
  ((plot_9 + plot_5) / (plot_4 +  plot_st)) + plot_annotation(title = "Postive trajectory in post-attack acceptance is robust to multiple imputation strategies",
                                                              subtitle = "Recovery of postive trajectory in counterfactual no-attack condition requires at least two waves prior to baseline",
                                                              tag_levels = "i") + plot_layout(guides = 'collect')


robust_waves

robust_waves2 <-
  plot_time + ((plot_9 + plot_5) / (plot_4 +  plot_st)) + plot_annotation(title = "Postive trajectory in post-attack acceptance is robust to multiple imputation strategies",
                                                                          subtitle = "Recovery of postive trajectory in counterfactual no-attack condition requires at least two waves prior to baseline",
                                                                          tag_levels = "i") + plot_layout(guides = 'collect')



ggsave(
  robust_waves,
  path = here::here(here::here("mods")),
  width = 10,
  height = 9,
  units = "in",
  filename = "robust_waves.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)
ggsave(
  robust_waves2,
  path = here::here(here::here("mods")),
  width = 16,
  height = 9,
  units = "in",
  filename = "robust_waves2.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# z plot method --------------------------------------------------------
#https://solomonkurz.netlify.app/post/2021-10-21-if-you-fit-a-model-with-multiply-imputed-data-you-can-still-plot-the-line/




fitted_lines <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()



fitted_lines %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) +
  ylab("Muslim Warmth") + theme_m


# z plot method --------------------------------------------------------

#https://solomonkurz.netlify.app/post/2021-10-21-if-you-fit-a-model-with-multiply-imputed-data-you-can-still-plot-the-line/

fitted_lines <-
  tibble(.imp = 1:10) %>%
  mutate(p = map(.imp, ~  ggeffects::ggpredict(
    model_all$model[[.]], terms = c("Wave[0:2,by=.01]", "As")
  ))) %>%
  data_frame() %>%
  unnest()



fitted_lines %>%
  ggplot(aes(x = x)) +
  geom_ribbon(aes(
    ymin = conf.low,
    ymax = conf.high,
    group = group,
    colour = group
  ),
  alpha = 1 / 10) +
  geom_line(aes(y = predicted, group = group),
            size = 1 / 4) +
  ylab("Muslim Warmth") + theme_m
