##############################################
# Table 1: Patient Characteristics
##############################################

# Packages
library(tidyverse)
library(table1)

# Loading Dataset
df <- read_csv("data/processed/final_data.csv")

# Factoring
df$drought <- factor(df$drought, levels = c(0,1), labels = c("No", "Yes"))
df$earthquake <- factor(df$earthquake, levels = c(0,1), labels = c("No", "Yes"))
df$oecd <- factor(df$oecd, levels = c(0,1), labels = c("No", "Yes"))
df$armed_conflict <- factor(df$armed_conflict, levels = c(0,1), labels = c("No Armed Conflictᵃ", "Armed Conflict"))

# Changing Labels
label(df$drought) <- "Droughtᵇ"
label(df$earthquake) <- "Earthquakeᵇ"
label(df$gdp_1000) <- "GDPᶜ"
label(df$oecd) <- "OECD Member"
label(df$pop_dens) <- "Population Densityᵈ"
label(df$urban) <- "Urban Residenceᵉ"
label(df$age_dep) <- "Age Dependency Ratioᶠ"
label(df$male_edu) <- "Male Educationᵍ"
label(df$temp) <- "Temperatureʰ"
label(df$rainfall_1000)  <- "Rainfallᶦ"
label(df$maternal_mortality) <- "Maternal Mortality Rateʲ"
label(df$infant_mortality) <- "Infant Mortality Rateʲ"
label(df$neonatal_mortality) <- "Neonatal Mortality Rateʲ"
label(df$under5_mortality) <- "Under-Five Mortality Rateʲ"

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


# Creating Table 1

render_mean_only <- function(x) {
  stats <- stats.default(x)
  with(stats.apply.rounding(stats, digits = 1), 
       c("", "Mean (SD)" = sprintf("%s (±%s)", MEAN, SD)))
}


table1(~drought + earthquake + gdp_1000 + oecd +
  pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + 
maternal_mortality + infant_mortality + neonatal_mortality + under5_mortality | armed_conflict, 
data = df, render.continuous = render_mean_only, footnote = footnote,
caption = "Table 1: Study Sample Characteristics Stratified by Armed Conflict Status")




