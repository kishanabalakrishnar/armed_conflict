##############################################
# Table 1: Patient Characteristics
##############################################

# Packages
library(tidyverse)
library(table1)
library(flextable)

# Loading Dataset
df <- read_csv("data/processed/final_data.csv")

# Factoring
df$drought <- factor(df$drought, levels = c(0,1), labels = c("No", "Yes"))
df$earthquake <- factor(df$earthquake, levels = c(0,1), labels = c("No", "Yes"))
df$oecd <- factor(df$oecd, levels = c(0,1), labels = c("No", "Yes"))
df$armed_conflict <- factor(df$armed_conflict, levels = c(0,1), labels = c("No Armed Conflictᵃ", "Armed Conflict"))

# Footnote
footnote <- "ᵃ &lt;25 battle-related deaths per country–conflict–year<br>
ᵇ Binary variables representing absence or presence<br>
ᶜ USD per capita<br>
ᵈ Percentage of population living in density of &gt;1000 people/km²<br>
ᵉ Percentage of people living in urban areas<br>
ᶠ Proportion of dependents (aged under 15 or over 64 years) per 100 working-age individuals<br>
ᵍ Years per capita and is age-standardised<br>
ʰ Degrees Celsius and is the mean population-weighted annual temperature<br>
ᶦ Mean population-weighted annual rainfall in millimetres per year, scaled down by 1,000<br>
ʲ Per 1000 live-births<br><br>
GDP, gross domestic product; OECD, Organization for Economic Co-operation and Development"

# Creating Labels
labels <- list(variables = list(drought= "Droughtᵇ",
earthquake = "Earthquakeᵇ",
gdp_1000 = "GDPᶜ",
oecd = "OECD Member",
pop_dens = "Population Densityᵈ",
urban = "Urban Residenceᵉ",
age_dep = "Age Dependency Ratioᶠ",
male_edu = "Male Educationᵍ",
temp = "Temperatureʰ",
rainfall_1000 = "Rainfallᶦ",
maternal_mortality = "Maternal Mortality Rateʲ",
infant_mortality = "Infant Mortality Rateʲ",
neonatal_mortality = "Neonatal Mortality Rateʲ",
under5_mortality =  "Under-Five Mortality Rateʲ"),
groups = list("Armed Conflict", ""))

levels(df$armed_conflict) <- c("Noᵃ", "Yes")

strata <- c(
  split(df, df$armed_conflict),
  list(Overall = df)
)

#Only Including Mean and Standard Deviation
render_mean_only <- function(x) {
  stats <- stats.default(x)
  with(stats.apply.rounding(stats, digits = 1), 
       c("", "Mean (SD)" = sprintf("%s (±%s)", MEAN, SD)))
}

#Generating Table 01
table1(strata, labels, groupspan = c(2,1), footnote = footnote, render.continuous = render_mean_only,
caption = "Table 1: Study Sample Characteristics Stratified by Armed Conflict Status (n=3720)")

