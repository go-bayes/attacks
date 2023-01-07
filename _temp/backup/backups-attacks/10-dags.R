# Dags made in overleaf
# code here is to take PDFS and make jpgs

# libraries
source(here::here("R", "libs.R"))

# DAG GRAPH ---------------------------------------------------------------

dag1 <-
  image_ggplot(image_read(here::here("mods", "missing.tiff")),
               interpolate = T)
dag2 <-
  image_ggplot(image_read(here::here("mods", "selection.tiff")),
               interpolate = T)
dag3 <-
  image_ggplot(image_read(here::here("mods", "impute.tiff")),
               interpolate = T)


dag4 <-
  image_ggplot(image_read(here::here("mods", "imputation-graph.tiff")),
               interpolate = T)

dag4


dag5 <-
  image_ggplot(image_read(here::here("mods", "swig-graph.tiff")),
               interpolate = T)

dag5 <-
  image_ggplot(image_read(here::here("mods", "swig-graph.tiff")),
               interpolate = T)
dag5


### NEW

a0 <-
  image_ggplot(image_read(here::here("figs", "a0.tiff")),
               interpolate = T)

a0

a1 <-
  image_ggplot(image_read(here::here("figs", "a1.tiff")),
               interpolate = T)

a1


imp0plot <-
  image_ggplot(image_read(here::here("mods", "amelia-00.jpeg")),
               interpolate = T)

imp1plot <-
  image_ggplot(image_read(here::here("mods", "amelia-01.jpg.jpeg")),
               interpolate = T)


library(patchwork)

fig1 <-
  (lds2 + dag5) / (dag4 + (imp1plot + imp0plot)) + plot_annotation(tag_levels = "i")

fig1 <-
  lds2 + (dag5  / dag4)  + (imp1plot / imp0plot) + plot_annotation(tag_levels = "i")

fig1 <- (lds2 / dag5)  | (dag4  / imp1plot / imp0plot) +
  plot_annotation(tag_levels = "i") #+  plot_layout(nrow = 2,byrow = FALSE)

fig1

library(ggpubr)
fig1 <-
  ggarrange(
    lds2,
    dag5,
    dag4,
    ggarrange(imp1plot , imp0plot,
              labels = c("  (i)", "(ii)")),
    labels = c("A", "B", "C", "D"),
    widths = c(1, 1),
    heights = c(2, 1),
    ncol = 2,
    nrow = 2
  )

fig1
ggsave(
  fig1,
  path = here::here("figs"),
  width = 12,
  height = 6,
  units = "in",
  filename = "fig1.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)


d
### USE
fig1 <- ggarrange(
  lds2,
  a1,
  a0,
  labels = c("A", "B", "C"),
  #  widths = c(1, 1),
  heights = c(2, 1, 1.25),
  # ncol = 2,
  nrow = 3
)



fig1
ggsave(
  fig1,
  path = here::here("figs"),
  width = 10,
  height = 10,
  units = "in",
  filename = "fig1.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)



fig1 <- ggarrange(
  lds2,
  a1,
  a0,
  labels = c("A", "B", "C"),
  #  widths = c(1, 1),
  heights = c(2, 1, 1.5),
  # ncol = 2,
  nrow = 3
)

onlydags <- ggarrange(
  a1,
  a0,
  labels = c("A", "B"),
  #  widths = c(1, 1),
  heights = c(1, 1.5),
  # ncol = 2,
  nrow = 2
)
onlydags


##
ggsave(
  onlydags,
  path = here::here("figs"),
  width = 10,
  height = 10,
  units = "in",
  filename = "onlydags.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)

#USED

fig2 <- ggarrange(
  dag4,
  ggarrange(imp1plot, imp0plot, labels = c("  (i)", "(ii)")),
  heights = c(2, 1.5),
  # ncol = 2,
  nrow = 2,
  labels = c("A", "B", "C")
)

fig2


ggsave(
  fig2,
  path = here::here("figs"),
  width = 10,
  height = 6.3,
  units = "in",
  filename = "fig2.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)


## Test
fig2 <- ggarrange(
  dag4,
  imp1plot,
  imp0plot,
  heights = c(1, 1.5, 1.5),
  # ncol = 2,
  nrow = 3,
  labels = c("A", "B", "C")
)

fig2


ggsave(
  fig2,
  path = here::here("figs"),
  width = 8,
  height = 8,
  units = "in",
  filename = "fig2.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 800
)


#graph for dags

daggraph <- (dag5)  +
  plot_annotation(title = "Missing potential outcomes and missing responses are both missing data problems")
daggraph

missgraph <- (dag4)  +
  plot_annotation(title = "Separate multiple imputation by condition for missing responses,\nleveraging six years of pre-attack data to recover no-exposure trajectory")

missgraph

# daggraph <- (dag2 /dag1) + dag3  +
#   plot_annotation(title = "Causal identification of missing potential outcomes\nand missing responses",
#                   tag_levels = "i")
# daggraph



dag5

ggsave(
  dag5,
  path = here::here("mods"),
  width = 14,
  height = 7,
  units = "in",
  filename = "daggraph.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 1200
)


dag5

ggsave(
  dag4,
  path = here::here("mods"),
  width = 14,
  height = 7,
  units = "in",
  filename = "missgraph.jpg",
  device = "jpeg",
  limitsize = FALSE,
  dpi = 1200
)
