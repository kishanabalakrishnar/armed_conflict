##############################################
# Table 2: Linear Regression Model
##############################################

# Packages
library(tidyverse)
library(plm)
library(stargazer)

# Loading Dataset
finaldata <- read_csv("data/processed/final_data.csv")

# Log Transformation of GDP
finaldata$log_gdp <- log10(finaldata$gdp_1000)

# Fitting Linear Regression Models

## Maternal Mortality
maternal.1 <- plm(maternal_mortality ~ armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought, 
index = c("iso", "year"), effect = "twoways", model = "within", data = finaldata)
summary(maternal.1)

## Infant Mortality
infant.1 <- plm(infant_mortality ~ armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought, 
index = c("iso", "year"), effect = "twoways", model = "within", data = finaldata)
summary(infant.1)

## Neonatal Mortality
neonatal.1 <- plm(neonatal_mortality ~ armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought, 
index = c("iso", "year"), effect = "twoways", model = "within", data = finaldata)
summary(neonatal.1)

## Under-Five Mortality
underfive.1 <- plm(under5_mortality ~ armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought, 
index = c("iso", "year"), effect = "twoways", model = "within", data = finaldata)
summary(underfive.1)

# Generating Table 2
labels <- c("Armed Conflictᵇ", "GDPᶜ", "OECD Member", "Population Densityᵈ", "Urban Residenceᵉ",
"Age Dependency Ratioᶠ", "Male Educationᵍ", "Temperatureʰ", "Rainfallᶦ", "Earthquake", "Drought")
dep_labels <- c("Maternal Mortalityᵃ", "Under-Five Mortalityᵃ", "Infant Mortalityᵃ", "Neonatal Mortalityᵃ")

models <- list(maternal.1, underfive.1, infant.1, neonatal.1)

country_counts <- vapply(models, function(m) length(unique(plm::index(m)[[1]])), integer(1))

footnote <- paste0(
  "ᵃ Per 1000 live births<br>",
  "ᵇ ≥25 battle-related deaths per country–conflict–year<br>",
  "ᶜ USD per capita on log scale<br>",
  "ᵈ Percentage of population living at a density of &gt;1000 people/km²<br>",
  "ᵉ Percentage of people living in urban areas<br>",
  "ᶠ Dependents (under 15 or over 64) per 100 working-age individuals<br>",
  "ᵍ Age-standardised years per capita<br>",
  "ʰ Mean population-weighted annual temperature in °C<br>",
  "ᶦ Mean population-weighted annual rainfall in mm, scaled down by 1,000<br>",
  "GDP, gross domestic product; OECD, Organization for Economic Co-operation and Development"
)

output_file <- "outputs/table02_regression.html"

stargazer(maternal.1, underfive.1, infant.1, neonatal.1, type = "html", ci = TRUE,
omit.stat = c("rsq", "adj.rsq", "f"),
covariate.labels = labels,
dep.var.labels = dep_labels,
model.numbers = FALSE,
notes.append = TRUE,
add.lines = list(c("Countries", country_counts)),
out = output_file)

## Formatting Table
table_html <- paste(readLines(output_file, warn = FALSE), collapse = "\n")
# Remove Stargazer's in-table significance row; add it above the footnotes below.
table_html <- sub(
  '<tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:(right|left)">.*?</tr>',
  "",
  table_html,
  perl = TRUE
)

armed_pos <- regexpr("Armed Conflict", table_html, fixed = TRUE)[1]
if (armed_pos < 0) stop("Could not find the Armed Conflict row in the Stargazer output.")

after_armed <- substring(table_html, armed_pos)
row_ends <- gregexpr("</tr>", after_armed, fixed = TRUE)[[1]]
if (length(row_ends) < 3 || row_ends[1] < 0) {
  stop("Could not locate the Armed Conflict estimate and confidence interval rows.")
}
insert_pos <- armed_pos + row_ends[3] + nchar("</tr>") - 1
section_heading <- paste0(
  "\n<tr><td colspan='5' style='text-align:left; font-weight:bold'>",
  "Covariates</td></tr>"
)
table_html <- paste0(
  substr(table_html, 1, insert_pos), section_heading,
  substring(table_html, insert_pos + 1)
)
writeLines(table_html, output_file, useBytes = TRUE)

cat(
  "\n<div style='text-align:left; margin-top:0.5em'>",
  "<em>Note:</em> <sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01<br>",
  footnote,
  "</div>\n",
  file = output_file,
  append = TRUE
)

