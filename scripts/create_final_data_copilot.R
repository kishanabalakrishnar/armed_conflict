##############################################
# CHL5233: Create Final Analytical Data Set
# Coding-agent version
##############################################

# Packages ----------------------------------------------------------------------

library(tidyverse)
library(janitor)

# Helper function ----------------------------------------------------------------

# Convert each World Bank mortality file from its wide year format to a panel.
convert_wb_long <- function(file_path, variable_name) {
  read_csv(file_path, show_col_types = FALSE) |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = variable_name
    ) |>
    transmute(
      iso,
      year = as.integer(year),
      !!variable_name := .data[[variable_name]]
    )
}

# Prepare mortality data ---------------------------------------------------------

maternal_mortality <- convert_wb_long(
  "data/raw/maternal_mortality.csv",
  "maternal_mortality"
)

infant_mortality <- convert_wb_long(
  "data/raw/infant_mortality.csv",
  "infant_mortality"
)

neonatal_mortality <- convert_wb_long(
  "data/raw/neonatal_mortality.csv",
  "neonatal_mortality"
)

under5_mortality <- convert_wb_long(
  "data/raw/under5_mortality.csv",
  "under5_mortality"
)

# Prepare covariate data ---------------------------------------------------------

# This file is already a country-year panel, so it only needs to be imported.
covariates <- read_csv("data/raw/covariates.csv", show_col_types = FALSE)

# Prepare disaster data ----------------------------------------------------------

# One country can experience several disasters, including both types, in a year.
# Summarise to one country-year row; an indicator is 1 if that type occurred.
disasters <- read_csv("data/raw/disaster.csv", show_col_types = FALSE) |>
  clean_names() |>
  filter(
    year %in% 2000:2019,
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  group_by(iso, year) |>
  summarise(
    earthquake = as.integer(any(disaster_type == "Earthquake")),
    drought = as.integer(any(disaster_type == "Drought")),
    .groups = "drop"
  )

# Prepare conflict data ----------------------------------------------------------

# Conflict deaths from year t define armed conflict in the following year.
conflict <- read_csv("data/raw/conflict.csv", show_col_types = FALSE) |>
  group_by(iso, year) |>
  summarise(cumulative_conflict = sum(best, na.rm = TRUE), .groups = "drop") |>
  mutate(
    armed_conflict = as.integer(cumulative_conflict >= 25),
    year = year + 1
  ) |>
  select(iso, year, armed_conflict)

# Merge and save -----------------------------------------------------------------

# Inner joins retain the country-years observed in every source data set.
final_data_copilot <- conflict |>
  inner_join(disasters, by = c("iso", "year")) |>
  inner_join(infant_mortality, by = c("iso", "year")) |>
  inner_join(maternal_mortality, by = c("iso", "year")) |>
  inner_join(neonatal_mortality, by = c("iso", "year")) |>
  inner_join(under5_mortality, by = c("iso", "year")) |>
  inner_join(covariates, by = c("iso", "year")) |>
  arrange(iso, year)

# This distinct check protects the intended one-row-per-country-year panel.
stopifnot(!anyDuplicated(final_data_copilot[c("iso", "year")]))

write_csv(final_data_copilot, "data/processed/final_data_copilot.csv")
