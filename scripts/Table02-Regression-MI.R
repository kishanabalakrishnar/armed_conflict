##############################################
# Multiple Imputations
##############################################

# Packages
library(mice)
library(tidyverse)
library(plm)
library(texreg)

# Loading Data 
finaldata <- read_csv("data/processed/final_data.csv")
finaldata$log_gdp <- log10(finaldata$gdp_1000)

# Complete Case Analysis
maternal.1 <- plm(maternal_mortality ~ armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought, 
index = c("iso", "year"), effect = "twoways", model = "within", data = finaldata)
neonatal.1 <- plm(neonatal_mortality ~ armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought, 
index = c("iso", "year"), effect = "twoways", model = "within", data = finaldata)
infant.1 <- plm(infant_mortality ~ armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought, 
index = c("iso", "year"), effect = "twoways", model = "within", data = finaldata)
underfive.1 <- plm(under5_mortality ~ armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought, 
index = c("iso", "year"), effect = "twoways", model = "within", data = finaldata)

# Multiple Imputations
midata <- finaldata |>
  mutate(iso_num = as.numeric(as.factor(finaldata$iso))) |>
  select(-iso)

mice0  <- mice(midata, seed = 100, m = 10, maxit = 20, print = FALSE)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall_1000", "log_gdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall_1000", "log_gdp", "pop_dens",
  "maternal_mortality", "infant_mortality", "neonatal_mortality",
  "under5_mortality"), "iso_num"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)
plot(mice.multi.out)

save(mice.multi.out, file = "outputs/mice_out.RData")

# Fitting the Models
fit.mi.matmor1 <- with(mice.multi.out, 
                      lm(maternal_mortality ~ -1 + armed_conflict + log_gdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
fit.mi.matmor2 <- with(mice.multi.out, 
                      lm(neonatal_mortality ~ -1 + armed_conflict + log_gdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
fit.mi.matmor3 <- with(mice.multi.out, 
                      lm(infant_mortality ~ -1 + armed_conflict + log_gdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
fit.mi.matmor4 <- with(mice.multi.out, 
                      lm(under5_mortality ~ -1 + armed_conflict + log_gdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))

out.mi.matmor1 <- pool(fit.mi.matmor1)
out.mi.matmor2 <- pool(fit.mi.matmor2)
out.mi.matmor3 <- pool(fit.mi.matmor3)
out.mi.matmor4 <- pool(fit.mi.matmor4)

output_file <- "outputs/MI-results.html"

# Convert complete-case and pooled MI results to texreg objects containing
# coefficient estimates and 95% confidence intervals.
cc_to_texreg <- function(model) {
  results <- broom::tidy(model, conf.int = TRUE, conf.level = 0.95)
  texreg::createTexreg(
    coef.names = as.character(results$term),
    coef = results$estimate,
    ci.low = results$conf.low,
    ci.up = results$conf.high
  )
}

mi_to_texreg <- function(model) {
  results <- summary(model, conf.int = TRUE, conf.level = 0.95)
  # Keep the fixed effects in the fitted model, but omit their coefficients
  # from the display table.
  results <- results[!grepl("iso_num|year", results$term), ]
  texreg::createTexreg(
    coef.names = as.character(results$term),
    coef = results$estimate,
    ci.low = results$`2.5 %`,
    ci.up = results$`97.5 %`
  )
}

footnote <- paste0(
  "* p<0.05<br>",
  "ᵃ Per 1000 live births<br>",
  "ᵇ ≥25 battle-related deaths per country–conflict–year<br>",
  "ᶜ USD per capita on log scale<br>",
  "ᵈ Percentage of population living at a density of &gt;1000 people/km²<br>",
  "ᵉ Percentage of people living in urban areas<br>",
  "ᶠ Dependents (under 15 or over 64) per 100 working-age individuals<br>",
  "ᵍ Age-standardised years per capita<br>",
  "ʰ Mean population-weighted annual temperature in °C<br>",
  "ᶦ Mean population-weighted annual rainfall in mm, scaled down by 1,000<br>",
  "GDP, gross domestic product; OECD, Organization for Economic Co-operation and Development; MI, Multiple Imputation"
)

htmlreg(list(
  cc_to_texreg(maternal.1), mi_to_texreg(out.mi.matmor1),
  cc_to_texreg(neonatal.1), mi_to_texreg(out.mi.matmor2),
  cc_to_texreg(infant.1), mi_to_texreg(out.mi.matmor3),
  cc_to_texreg(underfive.1), mi_to_texreg(out.mi.matmor4)
),
  custom.header = list(
    "Maternal Mortalityᵃ" = 1:2,
    "Neonatal Mortalityᵃ" = 3:4,
    "Infant Mortalityᵃ" = 5:6,
    "Under-5 Mortalityᵃ" = 7:8
  ),
  custom.model.names = rep(c("Complete Cases", "MI (n=10)"), 4),
  custom.coef.map = list(
    "armed_conflict" = "Armed Conflictᵇ",
    "log_gdp" = "GDPᶜ",
    "oecd" = "OECD Member",
    "pop_dens" = "Population Densityᵈ",
    "urban" = "Urban Residenceᵉ",
    "age_dep" = "Age Dependency Ratioᶠ",
    "male_edu" = "Male Educationᵍ",
    "temp" = "Temperatureʰ",
    "rainfall_1000" = "Rainfallᶦ",
    "earthquake" = "Earthquake",
    "drought" = "Drought"
  ),
  custom.note = footnote,
  caption.above = TRUE,
  caption = "Table 3. Results of Complete Case Analysis Versus Multiple Imputation (N=10)",
  file = output_file)
