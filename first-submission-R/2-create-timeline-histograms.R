# Graphs for timeline
# first run "analysis.R"

source(here::here("R", "libs.R"))


# jittered data for reproduction (NZAVS ethics does not permit original data deposited
# to internet. However, as per note, original data can be obtained, just email c.sibley@auckland.ac.nz or the chair of the U of Auckland ethics committee.

df <- readRDS(here::here("data", "df_s"))
# create timeline for graph in study  ---------------------------------------------------------
#  ru
rarep <- df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  droplevels() %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2018, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack!
  ungroup(Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr:::count(day = floor_date(timeline, "day")) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      day >= "2018-06-18" & day < "2019-03-15",
      0,
      ifelse(
        day >= "2019-03-15" & day < "2019-06-18",
        1,
        ifelse(day >= "2019-06-18" &
                 day < "2020-10-15", 2, 3)
      )
    ),
    labels = c(
      "Baseline",
      "Post-attack",
      "Post-attack one year",
      "Post-attack two years"
    )
  )) %>%
  arrange(day, Condition)


# get attack date
dates_vline2 <- as.Date("2019-03-15")
dates_vline3 <- as.Date("2019-06-18")
dates_vline4 <- as.Date("2021-06-18")
#3665 - min = 13 July 2019
# 4125 max = 15 October 2021

# for line in a graph
dates_vline2b <- which(rarep$day %in% dates_vline2)
dates_vline3b <- which(rarep$day %in% dates_vline3)
dates_vline4b <- which(rarep$day %in% dates_vline4)

# graph

lds2 <- ggplot(rarep, aes(day, n)) +
  geom_col(aes(fill = Condition)) +
  scale_x_date(date_labels = "%b/%Y",
               limits = c(as.Date("2018-06-01"), as.Date("2021-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  xlab("NZAVS Waves years 2018 - 2021 daily counts by condition") + ylab("Count of Responses") +
  theme_classic()  +
  annotate(
    "rect",
    xmin = dates_vline2,
    xmax = dates_vline3,
    ymin = 0,
    ymax = 2000,
    alpha = .3,
    fill = "darkred"
  ) +
  annotate(
    "rect",
    xmin = dates_vline3,
    xmax = as.Date("2020-10-15"),
    ymin = 0,
    ymax = 2000,
    alpha = .05,
    fill = "orange"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2020-10-15"),
    xmax = as.Date("2021-10-15"),
    ymin = 0,
    ymax = 2000,
    alpha = .05,
    fill = "yellow"
  ) +
  annotate("text",
           x = as.Date("2018-10-15"),
           y = 2600,
           label = "Time 10\npre-attacks") +
  annotate("text",
           x = as.Date("2019-01-01"),
           y = 2950,
           label = "**attack**") +
  annotate("text",
           x = as.Date("2019-06-15"),
           y = 2600,
           label = "Time 10\npost-attacks") +
  annotate("text",
           x = as.Date("2020-03-01"),
           y = 2600,
           label = "Time 11 Year\nFollowing") +
  annotate("text",
           x = as.Date("2021-03-01"),
           y = 2600,
           label = "Time 12 2 years \nFollowing") +
  annotate(
    geom = "curve",
    x = as.Date("2018-11-15"),
    y = 2000,
    xend = as.Date("2020-02-15"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2018-02-15"),
    y = 2300,
    xend = as.Date("2018-10-15"),
    yend = 2300,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2018-02-15"),
    y = 2000,
    xend = as.Date("2019-01-01"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2019-01-01"),
    y = 2000,
    xend = as.Date("2020-10-15"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2020-03-15"),
    y = 2000,
    xend = as.Date("2021-06-15"),
    yend = 2000,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 6),
    legend.title = element_text(color = "Black", size = 8)
  ) +  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(limits = c(0, 2950))
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
  filename = "timeline.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# timeline for the 2016-2021 cohort

# create timeline 2016 - 2021 ---------------------------------------------------------


rarepA <- df %>%
  # dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(Wave == 2012 | Wave == 2013 | Wave == 2014 | Wave == 2015 |
                  Wave == 2016 | Wave == 2017 | Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
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

max(rarepA$day, na.rm = TRUE)  # max day day 2021-09-30
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
               limits = c(as.Date("2012-06-01"), as.Date("2021-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  labs(title = "New Zealand Attitudes and Values Study (panel)", subtitle = "N = 67,858; years 2012-2021") +
  xlab("NZAVS Waves years 2012 - 2021 daily counts by condition") + ylab("Count of All Responses") +
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
  # scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(limits = c(0, 2300))
#theme(legend.position="none")


# inspect graph
ldsA

# save graph
ggsave(
  ldsA,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "timeline_all.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)


# HISTOGRAM ALL 2012 - 2021 -----------------------------------------------

rarep2 <- df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(Wave == 2016 | Wave == 2017 | Wave == 2018 | Wave == 2019 | Wave == 2020) %>%
  droplevels() %>%
  dplyr::mutate(org2016 =  ifelse(Wave == 2016 &
                                    YearMeasured == 1, 1, 0)) %>%
  group_by(Id) %>%
  dplyr::mutate(hold = mean(org2016, na.rm = TRUE)) %>%  # Hack
  filter(hold > 0) %>% # hack!
  ungroup(Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr:::count(day = floor_date(timeline, "day")) %>%
  dplyr::mutate(Condition = factor(
    ifelse(
      day >= "2016-06-18" & day < "2019-03-15",
      0,
      ifelse(
        day >= "2019-03-15" & day < "2019-06-18",
        1,
        ifelse(day >= "2019-06-18" &
                 day < "2020-10-15", 2, 3)
      )
    ),
    labels = c(
      "Baseline",
      "Post-attack",
      "Post-attack one year",
      "Post-attack two years"
    )
  )) %>%
  arrange(day, Condition)

rarep2

# get  dates
dates_vline2 <- as.Date("2019-03-15")
dates_vline3 <- as.Date("2019-06-18")
dates_vline4 <- as.Date("2021-06-18")
#3665 - min = 13 July 2019

# for line in a graph
dates_vline2b <- which(rarep2$day %in% dates_vline2)
dates_vline3b <- which(rarep2$day %in% dates_vline3)
dates_vline4b <- which(rarep2$day %in% dates_vline4)

# graph

lds3 <- ggplot(rarep2, aes(day, n)) +
  geom_col(aes(fill = Condition)) +
  scale_x_date(date_labels = "%b/%Y",
               date_breaks = "1 year",
               limits = c(as.Date("2016-09-10"), as.Date("2021-10-16")))  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(rarep$day[dates_vline2b]),
             col = "red",
             linetype = "dashed") +
  xlab("NZAVS Waves years 2016 - 2021 daily counts by condition") + ylab("Count of Responses") +
  theme_classic() +
  annotate(
    "rect",
    xmin = dates_vline2,
    xmax = dates_vline3,
    ymin = 0,
    ymax = 1500,
    alpha = .3,
    fill = "darkred"
  ) +
  annotate(
    "rect",
    xmin = dates_vline3,
    xmax = as.Date("2020-10-15"),
    ymin = 0,
    ymax = 1500,
    alpha = .05,
    fill = "orange"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2020-10-15"),
    xmax = as.Date("2021-10-15"),
    ymin = 0,
    ymax = 1500,
    alpha = .05,
    fill = "yellow"
  ) +
  annotate("text",
           x = as.Date("2017-06-01"),
           y = 2000,
           label = "Time 8-10\npre-attacks waves") +
  annotate("text",
           x = as.Date("2019-01-01"),
           y = 1700,
           label = "**attack**") +
  annotate("text",
           x = as.Date("2019-06-15"),
           y = 2000,
           label = "Time 10\npost-attacks") +
  annotate("text",
           x = as.Date("2020-03-01"),
           y = 2000,
           label = "Time 11 Year\nFollowing") +
  annotate("text",
           x = as.Date("2021-03-01"),
           y = 2000,
           label = "Time 12 2 years \nFollowing") +
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
scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(limits = c(0, 2300))
#theme(legend.position="none")


# inspect graph
lds3

# save graph
ggsave(
  lds3,
  path = here::here(here::here("figs")),
  width = 10,
  height = 5,
  units = "in",
  filename = "timeline3.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)



