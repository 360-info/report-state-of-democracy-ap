library(tidyverse)
library(here)

# tidy data on regime types for asia-pacific map
read_csv(
  paste0(
    "https://www.idea.int/gsod-indices/sites/default/files/inline-files/",
    "GSoDI%20v5.1%20%281975-2020%29.csv")) %>%
  filter(ID_year == 2020L, !is.na(democratic_performance_name)) %>%
  select(
    country = ID_country_name,
    region = ID_region_name,
    subregion = ID_subregion_name,
    regime_type = democratic_performance_name) %>%
  write_csv(here("data", "idea-regime-types-2020.csv"))
