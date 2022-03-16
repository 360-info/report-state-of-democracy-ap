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
  theme_360() +
  theme(
    legend.position = c(0.99, 0.9),
    legend.direction = "horizontal",
    legend.justification = c(1, 0.5),
    plot.caption = element_markdown(size = rel(0.9))) +
  labs(
    x = NULL, y = NULL,
    colour = "Education level",
    title = toupper("Satisfaction in UK Democracy"),
    caption = "**SOURCE:** TrustGov survey, conducted by Ipsos MORI<br>in May 2020") ->
edu_chart

dir.create(here("out"))
register_360fonts("itc")
save_360plot(edu_chart, here("out", "uk-edu-chart.png"),
  shape = "sdtv-landscape")
register_360fonts("libre")
save_360plot(edu_chart, here("out", "uk-edu-chart.svg"),
  shape = "sdtv-landscape")

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
  theme_360() +
  theme(
    legend.position = c(0.99, 0.9),
    legend.direction = "horizontal",
    legend.justification = c(1, 0.5),
    plot.caption = element_markdown(size = rel(0.9))) +
  labs(
    x = NULL, y = NULL,
    colour = "Brexit position",
    title = toupper("Satisfaction in UK Democracy"),
    caption = "**SOURCE:** TrustGov survey, conducted by Ipsos MORI<br>in May 2020") ->
brexit_chart

dir.create(here("out"))
register_360fonts("itc")
save_360plot(brexit_chart, here("out", "uk-brexit-chart.png"),
  shape = "sdtv-landscape")
register_360fonts("libre")
save_360plot(brexit_chart, here("out", "uk-brexit-chart.svg"),
  shape = "sdtv-landscape")

