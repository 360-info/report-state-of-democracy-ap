# comparison of australia's GSOD democratic performance subattributes to the
# rest of asia-pacific in 2020

# james goldie, 360info, nov-dec 2021

library(tidyverse)
# remotes::install_github("360-info/themes360info")
library(themes360info)
library(ggforce)
library(ragg)
library(grid)
library(png)
library(svglite)
library(here)

# data adictionary available at:
# https://www.idea.int/gsod-indices/sites/default/files/
#   global-state-of-democracy-indices-codebook-v4.pdf

# load the data
read_csv(here("data", "idea-gsod-all.csv")) %>%
  select(
    country = ID_country_name,
    year = ID_year,
    everything(),
    -regime_status_name, -starts_with("democratic_performance")) ->
gsod

# scope down to asia-pacific (inc. australia) for 2020
gsod %>%
  rename(region = ID_region_name) %>%
  select(-starts_with("ID_")) %>%
  filter(region == "Asia/Pacific", year == 2020) %>%
  pivot_longer(
    cols = -c(country, region, year),
    names_to = "subattribute",
    values_to = "score") %>%
  # focus on subattributes, not attributes or individual indicators
  filter(str_ends(subattribute, "SD[0-9][0-9]")) %>%
  # pivot wider by statistic (central/upper/lower)
  separate(subattribute, into = c("statistic", "subattribute"), sep = "_") %>%
  mutate(
    statistic = recode(statistic, C = "central", U = "upper", L = "lower")) %>%
  pivot_wider(
    id_cols = c(country, year, subattribute),
    names_from = statistic,
    values_from = score) %>%
  # recode attributes and subattributes
  mutate(
    attribute = case_when(
      str_starts(subattribute, "SD1") ~ "Representative government",
      str_starts(subattribute, "SD2") ~ "Fundamental rights",
      str_starts(subattribute, "SD3") ~ "Checks on government",
      str_starts(subattribute, "SD4") ~ "Impartial administration",
      str_starts(subattribute, "SD5") ~ "Participatory engagement",
      TRUE ~ NA_character_),
    subattribute_long = recode(subattribute,
      # representative government: A1
      SD11 = "Clean elections",
      SD12 = "Inclusive suffrage",
      SD13 = "Free political parties",
      SD14 = "Elected government",
      # fundamental rights: A2
      SD21 = "Access to justice",
      SD22 = "Civil liberties",
      SD23 = "Social rights and equality",
      # checks on gov: A3
      SD31 = "Effective parliament",
      SD32 = "Judicial independence",
      SD33 = "Media integrity",
      # impartial administration: A4
      SD41 = "Absence of corruption",
      SD42 = "Predictable enforcement",
      # # participatory engagement: A5
      SD51 = "Civil society participation",
      SD52 = "Electoral participation",
      SD53 = "Direct democracy",
      SD54 = "Local democracy")) ->
gsod_ap

# break off australian data
gsod_ap %>%
  filter(country == "Australia") %>%
  select(-subattribute) %>%
  group_by(attribute) %>%
  mutate(central_colour = case_when(
    central >= 0.7  ~ "High",
    central >= 0.4  ~ "Medium",
    !is.na(central) ~ "Low")) ->
gsod_aus_tidy

# now summary data across asia-pacific
gsod_ap %>%
  group_by(subattribute_long, attribute) %>%
  summarise(
    central = mean(central, na.rm = TRUE)) %>%
  mutate(central_colour = case_when(
    central >= 0.7  ~ "High",
    central >= 0.4  ~ "Medium",
    !is.na(central) ~ "Low"))  ->
gsod_ap_avg

# plot the barchart
gsod_aus_tidy %>%
  {
    ggplot(.) +
      aes(x = subattribute_long, y = central) +
      geom_col(aes(fill = central_colour)) +
      # points for asia-pacific averages
      # geom_point(data = gsod_ap_avg) +
      geom_col(data = gsod_ap_avg, aes(fill = "black"), alpha = 0.3,
        position = "identity", show.legend = TRUE) +
      # break it up by attribute
      facet_col(vars(attribute), scales = "free_y", space = "free") +
      coord_flip() +
      scale_y_continuous(
        expand = expansion(0.01),
        labels = scales::percent) +
      scale_fill_manual(
        name = NULL,
        values = c(
          "High" = "#96ca4f",
          "Medium" = "#ffd000",
          "Low" = "#c00808")) +
      theme_360info() +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.text = element_text(face = "bold"),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = rel(1)),
        plot.subtitle = element_text(family = "Subhead 360info",
            size = rel(1.1), margin = margin(b = 14 * 1.25))
      ) +
      labs(
        x = NULL, y = NULL,
        title = toupper("Australia"),
        subtitle = toupper(
          "Democratic performance 2020 compared to Asia-Pacific"),
        caption = "Source: IDEA, The Global State of Democracy Indices 1975–2020, v5.1 (2021)\n<https://www.idea.int/gsod-indices>. Accessed 2021-12-01."
      )
  } -> gsod_aus_barchart

# print

# add the 360 logo
# (note: i haven't version controlled a logo yet)
logo_360 <-
  here("360-logo.png") %>%
  readPNG() %>%
  rasterGrob(0.97, 0.03, just = c("right", "bottom"),
    height = unit(1, "cm"),
    interpolate = TRUE)

# composite and write to disk
agg_png(here("out", "aus-barchart.png"), width = 1200, height = 1800,
  units = "px", res = 150)
grid.draw(gList(ggplotGrob(gsod_aus_barchart), logo_360))
dev.off()

# use theme_360info(base_size = 10) for svg output
svglite(here("out", "aus-barchart.svg"), width = 6, height = 9)
grid.draw(gList(ggplotGrob(gsod_aus_barchart), logo_360))
dev.off()

svglite(here("out", "aus-barchart-test.svg"), width = 6, height = 9, scaling = 0.7)
grid.draw(gList(ggplotGrob(gsod_aus_barchart), logo_360))
dev.off()




