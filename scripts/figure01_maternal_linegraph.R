##############################################
# Figure 1: Maternal Mortality Increase Trend from 2000-2017
##############################################

# Packages
library(tidyverse)

# Loading Dataset
df <- read_csv("data/processed/final_data.csv")

# Setting up the Total Change from 2000 to 2017
conflict <- df |>
  dplyr::select(iso, year, maternal_mortality) |>
  dplyr::filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(total_change = last(maternal_mortality) - first(maternal_mortality))

# Filtering Countries with Positive Trend
conflict2 <- conflict |> filter(total_change > 0)

# Generating Graph
figure01 <- ggplot(conflict2, aes(x = year, y = maternal_mortality, color = iso)) + geom_line() + scale_y_log10() +
  labs(x = "Year", y = "Maternal Morality Rate Per 1000 Live-Births", color = "Country",
title = "Maternal Mortality Rate by Year\nfor Countries With an Increase From 2000-2017")

# Saving Plot
ggsave("figure01_maternal-linegraph.png", plot = figure01, width = 8, height = 5, path = "outputs")
