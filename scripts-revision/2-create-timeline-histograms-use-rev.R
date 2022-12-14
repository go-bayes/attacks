# Graphs for timeline
# first run "analysis.R"
# remove scientific notation
options(scipen = 999)
#libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")
library("lubridate") # working with dates
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


dat <- arrow::read_parquet(pull_path)

#source(here::here("R", "libs.R"))


# jittered data for reproduction (NZAVS ethics does not permit original data deposited
# to internet. However, as per note, original data can be obtained, just email c.sibley@auckland.ac.nz or the chair of the U of Auckland ethics committee.

df <- readRDS(here::here("data", "df_s"))


dt_2013 <- readRDS(here::here(push_mods, "2013_cohort_attacks"))


# create timeline for graph in study  ---------------------------------------------------------
#  ru
#rarep <- df %>%
rarep <- dt_2013 %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(Wave ==   "Time5" |
                Wave ==   "Time6" |
                  Wave == "Time7" |
                  Wave == "Time8" |
                  Wave == "Time9" |
                  Wave == "Time10" |
                  Wave == "Time11"|
                  Wave == "Time12" |
                  Wave == "Time13") %>%
  droplevels() %>%
  # dplyr::mutate(org2013 =  ifelse(Wave == "Time5" &
  #                                   YearMeasured == 1, 1, 0)) %>%
  # group_by(Id) %>%
  # dplyr::mutate(hold = mean(org2013, na.rm = TRUE)) %>%  # Hack
  # filter(hold > 0) %>% # hack!
 # ungroup(Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr:::count(day = floor_date(timeline, "day")) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      day >= "2013-09-18" & day < "2019-03-15",
      0,
      ifelse(
        day >= "2019-03-15" & day < "2019-06-18",
        1,
        ifelse(day >= "2019-06-18" &
                 day < "2020-10-15", 2,
               if_else( day >=  "2020-10-15" &
                          day <= "2021-10-15", 3, 4)
      )
    )),
    labels = c(
      "Baseline",
      "Post-attack",
      "Post-attack one year",
      "Post-attack two years",
      "Post-attack three years"
    )
  )) %>%
  arrange(day, Condition)


# get attack date
dates_vline2 <- as.Date("2019-03-15")
dates_vline3 <- as.Date("2019-06-18")
dates_vline4 <- as.Date("2021-06-18")
dates_vline5 <- as.Date("2022-06-18")
#3665 - min = 13 July 2019
# 4125 max = 15 October 2021

# for line in a graph
dates_vline2b <- which(rarep$day %in% dates_vline2)
dates_vline3b <- which(rarep$day %in% dates_vline3)
dates_vline4b <- which(rarep$day %in% dates_vline4)
dates_vline5b <- which(rarep$day %in% dates_vline5)

# graph

lds2 <- ggplot(rarep, aes(day, n)) +
  geom_col(aes(fill = Condition)) +
  scale_x_date(date_labels = "%b/%Y",
               limits = c(as.Date("2013-10-01"), as.Date("2022-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  xlab("NZAVS years 2013 - 2022 daily counts by condition") + ylab("Count of Responses") +
  # theme_classic()  +
  # annotate(
  #   "rect",
  #   xmin = dates_vline2,
  #   xmax = dates_vline3,
  #   ymin = 0,
  #   ymax = 1000,
  #   alpha = .3,
  #   fill = "darkred"
  # ) +
  # annotate(
  #   "rect",
  #   xmin = dates_vline3,
  #   xmax = as.Date("2020-10-15"),
  #   ymin = 0,
  #   ymax = 1000,
  #   alpha = .05,
  #   fill = "orange"
  # ) +
  # annotate(
  #   "rect",
  #   xmin = as.Date("2020-10-15"),
  #   xmax = as.Date("2021-10-15"),
  #   ymin = 0,
  #   ymax = 1000,
  #   alpha = .05,
  #   fill = "yellow"
  # ) +
  # annotate(
  #   "rect",
  #   xmin = as.Date("2021-10-15"),
  #   xmax = as.Date("2022-10-15"),
  #   ymin = 0,
  #   ymax = 1000,
  #   alpha = .05,
  #   fill = "Green"
  # ) +
  # annotate("text",
  #          x = as.Date("2018-10-15"),
  #          y = 2600,
  #          label = "Time 10\npre-attacks") +
  annotate("text",
           x = as.Date("2018-11-01"),
           y = 750,
           label = "Attack") +
  # annotate("text",
  #          x = as.Date("2019-06-15"),
  #          y = 2600,
  #          label = "Time 10\npost-attacks") +
  # annotate("text",
  #          x = as.Date("2020-03-01"),
  #          y = 2600,
  #          label = "Time 11 Year\nFollowing") +
  # annotate("text",
  #          x = as.Date("2021-03-01"),
  #          y = 2600,
  #          label = "Time 12 2 years \nFollowing") +
  # annotate("text",
  #          x = as.Date("2022-03-01"),
  #          y = 2600,
  #          label = "Time 12 3 years \nFollowing") +
  # annotate(
  #   geom = "curve",
  #   x = as.Date("2018-11-15"),
  #   y = 2000,
  #   xend = as.Date("2020-02-15"),
  #   yend = 2000,
  #   curvature = -.3,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(
  #   geom = "curve",
  #   x = as.Date("2018-02-15"),
  #   y = 2300,
  #   xend = as.Date("2018-5-15"),
  #   yend = 2300,
  #   curvature = -.3,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(
  #   geom = "curve",
  #   x = as.Date("2018-02-15"),
  #   y = 2000,
  #   xend = as.Date("2019-01-01"),
  #   yend = 2000,
  #   curvature = -.3,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(
  #   geom = "curve",
  #   x = as.Date("2019-01-01"),
  #   y = 2000,
  #   xend = as.Date("2020-10-15"),
  #   yend = 2000,
  #   curvature = -.3,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(
  #   geom = "curve",
  #   x = as.Date("2019-01-01"),
  #   y = 2000,
  #   xend = as.Date("2021-06-15"),
  #   yend = 2000,
  #   curvature = -.3,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(
  #   geom = "curve",
  #   x = as.Date("2019-01-01"),
  #   y = 2000,
  #   xend = as.Date("2022-06-15"),
  #   yend = 2000,
  #   curvature = -.3,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 6),
    legend.title = element_text(color = "Black", size = 8)
  ) +   scale_fill_okabe_ito()
#theme(legend.position="none")


# inspect graph
lds2


# save graph
ggsave(
  lds2,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "timeline_rev.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)





rarepA <- dt_2013 %>%
  # dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(Wave ==   "Time5" |
                  Wave ==   "Time6" |
                  Wave == "Time7" |
                  Wave == "Time8" |
                  Wave == "Time9" |
                  Wave == "Time10" |
                  Wave == "Time11"|
                  Wave == "Time12" |
                  Wave == "Time13") %>%
  droplevels() %>%
  #  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
  #                                    YearMeasured == 1, 1, 0)) %>%
  #  group_by(Id) %>%
  #  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  #  filter(hold > 0) %>% # hack!
  #  ungroup(Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr:::count(day = floor_date(timeline, "day")) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      day < "2019-03-19",
      0, 1 ),
    labels = c(
      "Pre-attack",
      "Post-attack"
    )
  )) %>%
  arrange(day, Condition)

rarepA  # min day 2012-09-19

max(rarepA$day, na.rm = TRUE)  # max day day 2022-09-20
# get  dates
dates_vline2 <- as.Date("2019-03-15")
#dates_vline3 <- as.Date("2019-06-18")
#dates_vline4 <- as.Date("2021-06-18")
#3665 - min = 13 July 2019

# for line in a graph
dates_vline2b <- which(rarepA$day %in% dates_vline2)
# dates_vline3b <- which(rarep2$day %in% dates_vline3)
# dates_vline4b <- which(rarep2$day %in% dates_vline4)

# graph

ldsA <- ggplot(rarepA, aes(day, n)) +
  geom_col(aes(fill = Condition)) +
  scale_x_date(date_labels = "%Y", #"%b/%Y",
               date_breaks = "1 year",
               limits = c(as.Date("2013-06-01"), as.Date("2022-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  # theme(
  #   legend.position = "top",
  #   legend.text = element_text(size = 6),
  #   legend.title = element_text(color = "Black", size = 8)
  # ) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  labs(title = "New Zealand Attitudes and Values Study (panel)", subtitle = "N = 7,727; years 2013-2021") +
  xlab("NZAVS years 2013- 2022 cohort (N = 7,727) : daily counts by condition") + ylab("Count of Responses") +
  theme_classic() +
  # annotate(
  #   "rect",
  #   xmin = dates_vline2,
  #   xmax = dates_vline3,
  #   ymin = 0,
  #   ymax = 2500,
  #   alpha = .3,
  #   fill = "darkred"
  # ) +
  #   annotate(
  #     "rect",
#     xmin = dates_vline3,
#     xmax = as.Date("2020-10-15"),
#     ymin = 0,
#     ymax = 2500,
#     alpha = .05,
#     fill = "orange"
#   ) +
#   annotate(
#     "rect",
#     xmin = as.Date("2020-10-15"),
#     xmax = as.Date("2021-10-15"),
#     ymin = 0,
#     ymax = 1500,
#     alpha = .05,
#     fill = "yellow"
#   ) +
#   annotate("text",
#            x = as.Date("2017-06-01"),
#            y = 2000,
#            label = "Time 8-10\npre-attacks waves") +
#   annotate("text",
#            x = as.Date("2019-01-01"),
#            y = 1700,
#            label = "**attack**") +
#   annotate("text",
#            x = as.Date("2019-06-15"),
#            y = 2000,
#            label = "Time 10\npost-attacks") +
#   annotate("text",
#            x = as.Date("2020-03-01"),
#            y = 2000,
#            label = "Time 11 Year\nFollowing") +
#   annotate("text",
#            x = as.Date("2021-03-01"),
#            y = 2000,
#            label = "Time 12 2 years \nFollowing") +
# #   annotate(
#     geom = "curve",
#     x = as.Date("2016-11-15"),
#     y = 2000,
#     xend = as.Date("2020-02-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2016-02-15"),
#     y = 2300,
#     xend = as.Date("2016-10-15"),
#     yend = 2300,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2016-02-15"),
#     y = 2000,
#     xend = as.Date("2019-01-01"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2019-01-01"),
#     y = 2000,
#     xend = as.Date("2020-10-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2020-03-15"),
#     y = 2000,
#     xend = as.Date("2021-06-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 6),
#     legend.title = element_text(color = "Black", size = 8)
#   )
scale_fill_okabe_ito() + theme_classic() +
theme(
  legend.position = "top",
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 14)
)



# inspect graph
lds2






tl <- dt_2013 %>%
  select(Warm.Muslims, TSCORE, Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr::filter(timeline > "2012-06-06") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(Attack_Condition = factor(
    ifelse(timeline < "2019-03-15", 0, 1),
    labels = c("Pre-Attack", "Post-attack")
  )) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      timeline >= "2012-09-18" & timeline < "2019-03-15",
      0,
      ifelse(
        timeline >= "2019-03-15" & timeline < "2019-06-18",
        1,
        ifelse(timeline >= "2019-06-18" &
                 timeline < "2020-10-15", 2,
               if_else( timeline >=  "2020-10-15" &
                          timeline <= "2021-10-15", 3, 4)
        )
      )),
    labels = c(
      "Baseline",
      "Post-attack",
      "Post-attack one year",
      "Post-attack two years",
      "Post-attack three years"
    )
  )) %>%
  arrange(timeline, Attack_Condition)
library(ggsci)


rdd <-
  ggplot(tl, aes(x = timeline, y = Warm.Muslims, color = Attack_Condition)) +
  geom_jitter(alpha = .05, width = 1) +
  stat_smooth(method = "gam") +
  labs(
    title = "Discontinuity at attacks (GAM)",
  #  subtitle = "Boost to Warmth increase in the years following the attacks",
    y = "Muslim Warmth",
    x = "NZAVS Time 5 - 13 Cohort (2013-2022), N = 7,727"
  ) +
  #theme(legend.position="none") +
  #scale_fill_discrete(name=NULL) + # not working
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10),
   legend.title = element_text(size = 10)
  )


rdd


lds2


rdd <-
  ggplot(tl, aes(x = timeline, y = Warm.Muslims, color = Attack_Condition)) +
  geom_jitter(alpha = .05, width = 1) +
  stat_smooth(method = "gam") +
  labs(
    title = "Discontinuity at attacks (GAM)",
    #  subtitle = "Boost to Warmth increase in the years following the attacks",
    y = "Muslim Warmth",
    x = "NZAVS Time 5 - 13 Cohort (2013-2022), N = 7,727"
  ) +
  #theme(legend.position="none") +
  #scale_fill_discrete(name=NULL) + # not working
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )


# rdd for 2013 cohort
rdd

# graph
fig_1_rev <- lds2/ rdd + plot_annotation(tag_levels = "A")


fig_1_rev

# save graph
ggsave(
  fig_1_rev,
  path = here::here(here::here("figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "fig_1_rev.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# HISTOGRAM ALL 2012 - 2021 -----------------------------------------------

dat <- arrow::read_parquet(pull_path)

rarep_all <- dat %>%
  dplyr::filter(
    Wave == 2012 | Wave == 2013 | Wave == 2014 |  Wave == 2015 | Wave == 2016 |  Wave == 2017 | Wave == 2018 | Wave == 2019 | Wave == 2020| Wave == 2021) %>%
  dplyr::filter(YearMeasured == 1) %>%
  droplevels()
  # dplyr::mutate(org2012 =  ifelse(Wave == 2013 &
  #                                   YearMeasured == 1, 1, 0)) %>%
  # group_by(Id) %>%
  # dplyr::mutate(hold = mean(org2013, na.rm = TRUE)) %>%  # Hack
  # filter(hold > 0) %>% # hack


length(unique(rarep_all$Id)) # 67409

rarep_all <- rarep_all |>
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr:::count(day = floor_date(timeline, "day")) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      day >= "2012-09-10" & day < "2019-03-15",
      0,
      ifelse(
        day >= "2019-03-15" & day < "2019-06-18", 1,
        ifelse(day >= "2019-06-18" & day < "2020-10-15", 2,
                  ifelse(day >= "2020-10-15" & day < "2021-10-15", 3,
                         4)))),
    labels = c(
      "Baseline",
      "Post-attack",
      "Post-attack one year",
      "Post-attack two years",
      "Post-attack three years"
    )
  )) %>%
  arrange(day, Condition)





# get  dates
dates_vline2 <- as.Date("2019-03-15")
dates_vline3 <- as.Date("2020-06-18")
dates_vline4 <- as.Date("2021-06-18")
dates_vline5 <- as.Date("2022-06-18")
#3665 - min = 13 July 2019

# for line in a graph
dates_vline_all <- which(rarep_all$day %in% dates_vline2)

dates_vline_all

# dates_vline3b <- which(rarep2$day %in% dates_vline3)
# dates_vline4b <- which(rarep2$day %in% dates_vline4)
# dates_vline5b <- which(rarep2$day %in% dates_vline4)

# graph

dates_vline_all

lds_all <- ggplot(rarep_all, aes(day, n)) +
  geom_col(aes(fill = Condition)) +
  scale_x_date(date_labels = "%b/%Y",
               date_breaks = "1 year",
               limits = c(as.Date("2012-09-10"), as.Date("2022-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep_all$day[dates_vline_all]),
             col = "red",
             linetype = "dashed") +
  xlab("NZAVS Waves years 2012 - 2022 daily counts by condition") + ylab("Count of Responses") +
  theme_classic() + scale_fill_okabe_ito() +
  labs(title = "New Zealand Attitudes and Values Study (panel)", subtitle = "N = 67,409; years 2012-2021") +
  xlab("NZAVS years 2012- 2022 cohort (N = 67,409) : daily counts by condition") + ylab("Count of Responses") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

lds_all





library(ggsci)

tl <- dat %>%
  select(Warm.Muslims, TSCORE, Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr::filter(timeline > "2012-06-06") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(Attack_Condition = factor(
    ifelse(timeline < "2019-03-15", 0, 1),
    labels = c("Pre-Attack", "Post-attack")
  )) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      timeline >= "2012-09-18" & timeline < "2019-03-15",
      0,
      ifelse(
        timeline >= "2019-03-15" & timeline < "2019-06-18",
        1,
        ifelse(timeline >= "2019-06-18" &
                 timeline < "2020-10-15", 2,
               if_else( timeline >=  "2020-10-15" &
                          timeline <= "2021-10-15", 3, 4)
        )
      )),
    labels = c(
      "Baseline",
      "Post-attack",
      "Post-attack one year",
      "Post-attack two years",
      "Post-attack three years"
    )
  )) %>%
  arrange(timeline, Attack_Condition)



rdd_all <-
  ggplot(tl, aes(x = timeline, y = Warm.Muslims, color = Attack_Condition)) +
  geom_jitter(alpha = .03, width = 1) +
  stat_smooth(method = "gam") +
  theme(legend.position = "bottom") +
  labs(
    title = "Discontinuity at attacks (GAM)",
    subtitle = "Boost to Warmth increase in the years following the attacks",
    y = "Muslim Warmth",
    x = "NZAVS Time 4 - 13 Cohort (2012-2022), (N = 67,409)"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic()

rdd_all

rdd_all2 <-
  ggplot(tl, aes(x = timeline, y = Warm.Muslims, color = Condition)) +
  geom_jitter(alpha = .03, width = 1) +
  stat_smooth(method = "gam") +
  theme(legend.position = "bottom") +
  labs(
    title = "Discontinuity at attacks (GAM)",
    subtitle = "Boost to Warmth increase in the years following the attacks",
    y = "Muslim Warmth",
    x = "NZAVS Time 4 - 13 Cohort (2012-2022), (N = 67,409)"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic()

# rdd all conditions
rdd_all2




# save graph
ggsave(
  rdd_all,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "s_timeline_all_rev.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)




rdd_all <-
  ggplot(tl, aes(x = timeline, y = Warm.Muslims, color = Attack_Condition)) +
  geom_jitter(alpha = .01, width = 1) +
  stat_smooth(method = "gam") +
  theme(legend.position = "bottom") +
  labs(
    title = "Discontinuity at attacks (GAM)",
    subtitle = "Boost to Warmth increase in the years following the attacks",
    y = "Muslim Warmth",
    x = "NZAVS Time 4 - 13 Cohort (2012-2022), (N = 67,409)"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic()

rdd_all

S_fig_1_rev <- lds_all/ rdd_all + plot_annotation(tag_levels = "A")
S_fig_1_rev

# save graph
ggsave(
  S_fig_1_rev,
  path = here::here(here::here("figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "s_timeline_all_rev.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

#
# lds_all +
#   annotate(
#     "rect",
#     xmin = dates_vline2,
#     xmax = dates_vline3,
#     ymin = 0,
#     ymax = 1500,
#     alpha = .3,
#     fill = "darkred"
#   ) +
#   annotate(
#     "rect",
#     xmin = dates_vline3,
#     xmax = as.Date("2020-10-15"),
#     ymin = 0,
#     ymax = 1500,
#     alpha = .05,
#     fill = "orange"
#   ) +
#   annotate(
#     "rect",
#     xmin = as.Date("2020-10-15"),
#     xmax = as.Date("2021-10-15"),
#     ymin = 0,
#     ymax = 1500,
#     alpha = .05,
#     fill = "yellow"
#   ) +
#   annotate(
#     "rect",
#     xmin = as.Date("2021-10-15"),
#     xmax = as.Date("2022-10-15"),
#     ymin = 0,
#     ymax = 1500,
#     alpha = .05,
#     fill = "green"
#   ) +
#   annotate("text",
#            x = as.Date("2014-06-01"),
#            y = 1900,
#            label = "Time 5-10\npre-attacks waves") +
#   annotate("text",
#            x = as.Date("2019-01-01"),
#            y = 1700,
#            label = "**attack**") +
#   annotate("text",
#            x = as.Date("2019-03-15"),
#            y = 1900,
#            label = "Time 10\npost-attacks") +
#   annotate("text",
#            x = as.Date("2020-03-01"),
#            y = 1900,
#            label = "Time 11 1 Year\nFollowing") +
#   annotate("text",
#            x = as.Date("2021-03-01"),
#            y = 1900,
#            label = "Time 12 2 years \nFollowing") +
  #   annotate(
  #     geom = "curve",
  #     x = as.Date("2016-11-15"),
  #     y = 2000,
  #     xend = as.Date("2020-02-15"),
  #     yend = 2000,
  #     curvature = -.3,
  #     arrow = arrow(length = unit(2, "mm"))
  #   ) +
  #   annotate(
  #     geom = "curve",
#     x = as.Date("2016-02-15"),
#     y = 2300,
#     xend = as.Date("2016-10-15"),
#     yend = 2300,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2016-02-15"),
#     y = 2000,
#     xend = as.Date("2019-01-01"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2019-01-01"),
#     y = 2000,
#     xend = as.Date("2020-10-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   annotate(
#     geom = "curve",
#     x = as.Date("2020-03-15"),
#     y = 2000,
#     xend = as.Date("2021-06-15"),
#     yend = 2000,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm"))
#   ) +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 6),
#     legend.title = element_text(color = "Black", size = 8)
#   )


#theme(legend.position="none")

#
# # inspect graph
# lds3
#
# # save graph
# ggsave(
#   lds3,
#   path = here::here(here::here("figs")),
#   width = 10,
#   height = 5,
#   units = "in",
#   filename = "timeline3.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1200
# )
#


