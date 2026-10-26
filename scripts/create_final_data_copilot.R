##############################################
# CHL5233: Create Final Analytical Data Set
##############################################

# Packages ----------------------------------------------------------------------

library(tidyverse)
library(janitor)

# Helper function ----------------------------------------------------------------

# Convert a World Bank file from wide year columns to a country-year panel.
prepare_mortality <- function(data, outcome_name) {
  data |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = outcome_name
    ) |>
    transmute(
      iso,
      year = as.integer(year),
      !!outcome_name := .data[[outcome_name]]
    )
}

# Prepare mortality data ---------------------------------------------------------

maternal_mortality <- read_csv(
  "data/raw/maternal_mortality.csv", show_col_types = FALSE
) |>
  prepare_mortality("maternal_mortality")

infant_mortality <- read_csv(
  "data/raw/infant_mortality.csv", show_col_types = FALSE
) |>
  prepare_mortality("infant_mortality")

neonatal_mortality <- read_csv(
  "data/raw/neonatal_mortality.csv", show_col_types = FALSE
) |>
  prepare_mortality("neonatal_mortality")

under5_mortality <- read_csv(
  "data/raw/under5_mortality.csv", show_col_types = FALSE
) |>
  prepare_mortality("under5_mortality")

# Prepare disaster data ----------------------------------------------------------

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

# A country is in armed conflict when total battle-related deaths in a year are
# at least 25. The paper's exposure is lagged, so values are assigned to the
# following year.
conflict <- read_csv("data/raw/conflict.csv", show_col_types = FALSE) |>
  group_by(iso, year) |>
  summarise(total_battle_deaths = sum(best, na.rm = TRUE), .groups = "drop") |>
  transmute(
    iso,
    year = year + 1L,
    armed_conflict = as.integer(total_battle_deaths >= 25)
  ) |>
  select(iso, year, armed_conflict)

# Prepare covariate data ---------------------------------------------------------

covariates <- read_csv("data/raw/covariates.csv", show_col_types = FALSE)

# Merge and save -----------------------------------------------------------------

# Mortality data form the country-year panel. Missing event records mean that
# the event did not occur in that country-year.
final_data_copilot <- maternal_mortality |>
  left_join(infant_mortality, by = c("iso", "year")) |>
  left_join(neonatal_mortality, by = c("iso", "year")) |>
  left_join(under5_mortality, by = c("iso", "year")) |>
  left_join(disasters, by = c("iso", "year")) |>
  left_join(conflict, by = c("iso", "year")) |>
  left_join(covariates, by = c("iso", "year")) |>
  mutate(
    earthquake = coalesce(earthquake, 0L),
    drought = coalesce(drought, 0L),
    armed_conflict = coalesce(armed_conflict, 0L)
  ) |>
  arrange(iso, year)

stopifnot(!anyDuplicated(final_data_copilot[c("iso", "year")]))

write_csv(final_data_copilot, "data/processed/final_data_copilot.csv")
