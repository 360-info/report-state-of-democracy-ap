library(tidyverse)
library(sf)
library(rgeoboundaries)
library(countrycode)
library(wbstats)
library(systemfonts)
library(ragg)
library(png)
library(here)

# 0. dictionary of country name overrides
# (to standradise names from different sources!)
name_subs <- c(
  "BIH" = "Bosnia and Herzegovina",
  "BOL" = "Bolivia",
  "BRN" = "Brunei",
  "CAF" = "Central African Republic",
  "CHN" = "China",
  "COD" = "Democratic Republic of the Congo",
  "KOR" = "South Korea",
  "LAO" = "Laos",
  "MMR" = "Myanmar",
  "NPL" = "Nepal",
  "PHL" = "Philippines",
  "PRK" = "North Korea",
  "TWN" = "Taiwan",
  "TZA" = "Tanzania",
  "VCT" = "Saint Vincent and Grenadines",
  "VEN" = "Venezuela")
standardise_countries <- partial(countrycode, origin = "iso3c",
    destination = "country.name", custom_match = name_subs)

# 1. get asia-pacific country boundaries 

all_countries <- 
  geoboundaries() %>%
  select(iso3 = shapeISO)

# asia-pacific countries (note: philippines' iso3 is incorrectly coded!)
phl <- geoboundaries() %>% filter(shapeGroup == "PHL")
phl$shapeISO <- "PHL"

geoboundaries() %>%
  filter(
    shapeGroup %in% c(
      "KAZ", "KGZ", "TJK", "TKM", "UZB", "CHN", "PRK", "JPN", "MNG", "KOR", "TWN", "AUS", "NZL", "PNG", "SLB", "AFG", "BGD", "IND", "NPL", "PAK", "LKA", "KHM", "IDN", "LAO", "MYS", "MMR", "SGP", "THA", "TLS", "VNM", "FJI")) %>%
  bind_rows(phl) %>%
  select(iso3 = shapeISO) %>%
  st_shift_longitude() -> ap
  # simplify boundaries
  # st_simplify(dTolerance = 10000)
  ->
ap

object.size(ap) %>% format(units = "MB")

# 2. get our regime classifications

read_csv(here("data", "idea-regime-types-2020.csv")) %>%
  filter(region == "Asia/Pacific") %>%
  # standardise country names
  mutate(iso3 = countryname(country, destination = "iso3c")) %>%
  select(iso3, regime_type) ->
regimes

# 3. get our population figures
# nb: world bank pops don't have taiwan separately!

pops <-
  wb_data("SP.POP.TOTL", start_date = 2020) %>%
  select(iso3 = iso3c, population = SP.POP.TOTL)

# 4. join it all by iso3
ap %>%
  left_join(regimes, by = "iso3") %>%
  left_join(pops, by = "iso3") %>%
  mutate(
    country = standardise_countries(iso3),
    regime_type = str_to_sentence(regime_type)) %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  select(iso3, country, everything(), geometry) ->
ap_bubbles

regime_colours <- c(
  "Authoritarian regime" = "red",
  "Hybrid regime" = "orange",
  "Weak democracy" = "#34d2eb",
  "Mid-range performing democracy" = "#1aa6d9",
  "High performing democracy" = "#0f67a6")

# split countries into big and small for labelling purposes
big_countries <- ap_bubbles %>% filter(country %in% c("India", "China"))
small_countries <- ap_bubbles %>% filter(!(country %in% c("India", "China")))

# register plot fonts
register_variant(
  name = "Franklin 360 Heavy", family = "Libre Franklin", weight = "heavy")
register_variant(
  name = "Franklin 360 Bold", family = "Libre Franklin", weight = "bold")
register_variant(
  name = "Franklin 360 Medium", family = "Libre Franklin", weight = "medium")

# map it!
bubble_map <- ggplot() +
  # country boundaries
  geom_sf(data = ap, fill = "#cccccc", colour = "white") +
  # bubbles
  geom_sf(aes(size = population, colour = regime_type), data = ap_bubbles) +
  geom_sf_text(
    aes(label = country, colour = regime_type), data = small_countries,
    family = "Franklin 360 Bold", size = 3, hjust = 0,
    vjust = 1, show.legend = FALSE) +
  # bubbles
  geom_sf_text(
    aes(label = country), data = big_countries, colour = "white",
    family = "Franklin 360 Bold", size = 3,
    show.legend = FALSE) +
  scale_radius(
    breaks = c(5e5, 1e6, 5e6, 1e7, 5e7, 1e8, 5e8, 1e9),
    range = c(1, 20),
    labels = scales::label_number_si(unit = ""),
    name = toupper("Population")) +
  scale_colour_manual(values = regime_colours, name = toupper("Regime type")) +
  coord_sf(crs = 3112) +
  theme_minimal(base_family = "Franklin 360 Medium", base_size = 16) +
  theme(
    plot.background = element_rect(fill = "white", color = NULL),
    plot.title =
      element_text(colour = "#36a7e9", family = "Franklin 360 Heavy",
        size = rel(2.5)),
    plot.subtitle =
      element_text(family = "Franklin 360 Bold", size = rel(1.5)),
    legend.title = element_text(family = "Franklin 360 Bold"),
    # legend.position = "top",
    # legend.direction = "horizontal",
    plot.title.position = "plot",
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.caption =
      element_text(hjust = 0, margin = margin(20, 0, 0, 0), size = rel(0.75),
        colour = "#6b767f"),
    plot.caption.position = "plot"
    ) +
  labs(
    x = NULL, y = NULL,
    title = toupper("Asia-Pacific regimes"),
    subtitle = toupper("Some subtitle here"),
    caption = "Source: https://www.idea.int/gsod-indices/democracy-indices")

# add the 360 logo
# (note: i haven't version controlled a logo yet)
logo_360 <-
  here("360-logo.png") %>%
  readPNG() %>%
  rasterGrob(1, 1, just = c("right", "top"),
    height = unit(72, 'points'),
    interpolate = TRUE)

# composite and write to disk
agg_png(here("out", "ap-map.png"), width = 16, height = 9, units = "in",
  res = 300)
grid.draw(gList(ggplotGrob(bubble_map), logo_360))
dev.off()
