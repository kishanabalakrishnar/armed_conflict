##############################################
# CHL5233: Week 3 In-Class Assignment Script
# October 3rd, 2026
##############################################

# Packages ----------------------------------------------------------------------

library(tidyverse)
#install.packages("janitor")
library(janitor)

# Prepare World Bank Data -------------------------------------------------------

## Reading in the Data

maternal_mortalitydf <- read_csv("data/raw/maternal_mortality.csv")

## Converting the Maternal Mortality Data into a Long Format

maternal_mortalitydflong <- maternal_mortalitydf |> 
  pivot_longer(cols = starts_with("X"), 
names_to = "year",
names_prefix = "X", 
values_to = "maternal_mortality"
) |> mutate(year = as.numeric(year)) |> 
  select(iso, year, maternal_mortality)

## Creating a Function

converting_wblong <- function(df, var){
  df |> 
    pivot_longer(cols = starts_with("X"),
  names_to = "year",
  names_prefix = "X",
  values_to = var) |> mutate(year = as.numeric(year)) |> 
    select(iso, year, all_of(var))
}

## Applying it to All WB Data Sets

### Infant Mortality Dataset

infant_mortalitydf <- read_csv("data/raw/infant_mortality.csv")
infant_mortalitydflong <- converting_wblong(infant_mortalitydf, "infant_mortality")

## Neonatal Mortality Dataset

neonatal_mortalitydf <- read_csv("data/raw/neonatal_mortality.csv")
neonatal_mortalitydflong <- converting_wblong(neonatal_mortalitydf, "neonatal_mortality")

## Under 5 Mortality Dataset

under5_mortalitydf <- read_csv("data/raw/under5_mortality.csv")
under5_mortalitydflong <- converting_wblong(under5_mortalitydf, "under5_mortality")

# Prepare Disaster Data ---------------------------------------------------------

disaster_df <- read_csv("data/raw/disaster.csv")
disaster_df <- clean_names(disaster_df)
disaster_df <- disaster_df |> filter(year %in% c(2000:2019)) |> 
  filter(disaster_type %in% c("Earthquake", "Drought"))
disaster_df <- disaster_df |> select(year, iso, disaster_type)
disaster_df <- disaster_df |> mutate(drought = case_when(
  disaster_type == "Drought" ~ 1,
  TRUE ~ 0
),
earthquake = case_when(
  disaster_type == "Earthquake" ~ 1,
  TRUE ~ 0
))
disaster_df <- disaster_df |> select(year, iso, earthquake, drought)

# Prepare Conflict Data ---------------------------------------------------------

conflict_df <- read_csv("data/raw/conflict.csv")
conflict_df <- conflict_df |> group_by(iso, year) |> 
  summarise(cumulative_conflict = sum(best, na.rm = TRUE))
conflict_df <- conflict_df |> mutate(armed_conflict = case_when(cumulative_conflict >= 25 ~ 1,
cumulative_conflict < 25 ~ 0,
TRUE ~ NA_real_))
conflict_df <- conflict_df |> mutate(year = year + 1)

# Merge All Data and Save -------------------------------------------------------

merge1 <- merge(conflict_df, disaster_df, by = c("iso", "year"))
merge2 <- merge(merge1, infant_mortalitydflong, by = c("iso", "year"))
merge3 <- merge(merge2, maternal_mortalitydflong, by = c("iso", "year"))
merge4 <- merge(merge3, neonatal_mortalitydflong, by = c("iso", "year"))
final_data <- merge(merge4, under5_mortalitydflong, by = c("iso", "year"))

write_csv(final_data, file = "data/processed/final_data.csv")
