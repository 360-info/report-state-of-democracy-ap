library(tidyverse)
# remotes::install_github("360-info/themes360info")
library(themes360info)
library(here)

# dats adictionary available at:
# https://www.idea.int/gsod-indices/sites/default/files/
#   global-state-of-democracy-indices-codebook-v4.pdf

read_csv(here("data", "idea-gsod-all.csv")) %>%
  select(
    country = ID_country_name,
    year = ID_year,
    everything(),
    -regime_status_name, -starts_with("democratic_performance")) ->
gsod

# australian data
gsod %>%
  select(-starts_with("ID_")) %>%
  filter(country == "Australia") %>%
  pivot_longer(
    cols = -c(country, year),
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
      SD51 = "Divil society participation",
      SD52 = "Electoral participation",
      SD53 = "Direct democracy",
      SD54 = "Local democracy")) ->
gsod_australia_tidy

# plot the barchart
gsod_australia_tidy %>%
  filter(year == 2020) %>%
  mutate(central_colour = case_when(
    central > 0.67  ~ "High",
    central > 0.33  ~ "Medium",
    !is.na(central) ~ "Low")) %>%
  {
    ggplot(.) +
      aes(x = subattribute_long, y = central, fill = central_colour) +
      geom_col() +
      facet_wrap(vars(attribute), ncol = 1, scales = "free_y") +
      coord_flip() +
      scale_y_continuous(
        expand = expansion(0.01),
        labels = scales::percent) +
      scale_fill_manual(
        name = NULL,
        values = c(
          "High" = "green",
          "Medium" = "yellow",
          "Low" = "red")) +
      theme_360info() +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "left",
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0)
      ) +
      labs(
        x = NULL, y = NULL,
        title = toupper("Australia"),
        subtitle = toupper("Democratic performance 2020"),
        caption = "Source: IDEA, The Global State of Democracy Indices 1975–2020, v5.1 (2021)\n<https://www.idea.int/gsod-indices>. Accessed 2021-12-01."
      )
  } %>%
  ggsave(here("out", "aus-barchart.png"), .)
