# visualisations of changing satisfaction with democracy over time

# james goldie & michael joiner, 360info, dec 2021

library(tidyverse)
# remotes::install_github("360-info/themes360info")
library(themes360info)
library(ragg)
library(png)
library(svglite)
library(ggtext)
library(lubridate)
library(here)

# ---- a) by education level --------------------------------------------------


education <-
  read_csv(
    here("data", "bes-panel-education.csv"), skip = 1,
    col_names = c("date", "gcse_lte", "alevel", "university")) %>%
  # clean up the dates
  mutate(
    month = str_sub(date, 1, 3),
    year = str_sub(date, 6, 7),
    date = ymd(paste0(year, "-", month, "-15"))) %>%
  select(-month, -year) %>%
  # put the university levels into on column
  pivot_longer(-date, names_to = "edu_level", values_to = "satisfaction") %>%
  mutate(
    edu_level = factor(edu_level,
      levels = c("gcse_lte", "alevel", "university"),
      labels = c("GCSE or lower", "A-Levels", "University")))

ggplot(education) +
  aes(date, satisfaction, colour = edu_level) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_360info() +
  theme(
    legend.position = c(0.99, 0.9),
    legend.direction = "horizontal",
    legend.justification = c(1, 0.5),
    plot.caption = element_markdown(size = rel(0.9))) +
  labs(
    x = NULL, y = NULL,
    colour = "Education level",
    title = toupper("Satisfaction in UK Democracy"),
    subtitle = toupper("Subtitle here"),
    caption = "**SOURCE: X and Y**, test test test") ->
edu_chart

logo_360 <-
  here("360-logo.png") %>%
  readPNG() %>%
  rasterGrob(0.97, 0.03, just = c("right", "bottom"),
    height = unit(1, "cm"),
    interpolate = TRUE)

# composite and write to disk
agg_png(here("out", "uk-edu-chart.png"), width = 1200, height = 600,
  units = "px", res = 120)
grid.draw(gList(ggplotGrob(edu_chart), logo_360))
dev.off()

svglite(here("out", "uk-edu-chart.svg"), width = 6, height = 3, scaling = 0.7)
grid.draw(gList(ggplotGrob(edu_chart), logo_360))
dev.off()


# ---- a) by leave/remain -----------------------------------------------------

brexit <-
  read_csv(
    here("data", "bes-panel-brexit.csv"), skip = 1,
    col_names = c("date", "remian", "leave")) %>%
  # clean up the dates
  mutate(
    month = str_sub(date, 1, 3),
    year = str_sub(date, 6, 7),
    date = ymd(paste0(year, "-", month, "-15"))) %>%
  select(-month, -year) %>%
  # put the university levels into on column
  pivot_longer(-date, names_to = "position", values_to = "satisfaction") %>%
  mutate(
    position = factor(position,
      levels = c("remian", "leave"),
      labels = c("Remain", "Leave")))

ggplot(brexit) +
  aes(date, satisfaction, colour = position) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_360info() +
  theme(
    legend.position = c(0.99, 0.9),
    legend.direction = "horizontal",
    legend.justification = c(1, 0.5),
    plot.caption = element_markdown(size = rel(0.9))) +
  labs(
    x = NULL, y = NULL,
    colour = "Brexit position",
    title = toupper("Satisfaction in UK Democracy"),
    subtitle = toupper("Subtitle here"),
    caption = "**SOURCE: X and Y**, test test test") ->
brexit_chart

logo_360 <-
  here("360-logo.png") %>%
  readPNG() %>%
  rasterGrob(0.97, 0.03, just = c("right", "bottom"),
    height = unit(1, "cm"),
    interpolate = TRUE)

# composite and write to disk
agg_png(here("out", "uk-brexit-chart.png"), width = 1200, height = 600,
  units = "px", res = 120)
grid.draw(gList(ggplotGrob(brexit_chart), logo_360))
dev.off()

svglite(here("out", "uk-brexit-chart.svg"), width = 6, height = 3, scaling = 0.7)
grid.draw(gList(ggplotGrob(brexit_chart), logo_360))
dev.off()


