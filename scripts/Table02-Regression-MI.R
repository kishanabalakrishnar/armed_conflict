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

mice0  <- mice(midata, seed = 100, m = 20, maxit = 0, print = FALSE)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall_1000", "log_gdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall_1000", "log_gdp", "pop_dens",
  "maternal_mortality", "infant_mortality", "neonatal_mortality",
  "under5_mortality"), "iso_num"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 20, maxit = 3,
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

htmlreg(list(
  cc_to_texreg(maternal.1), mi_to_texreg(out.mi.matmor1),
  cc_to_texreg(neonatal.1), mi_to_texreg(out.mi.matmor2),
  cc_to_texreg(infant.1), mi_to_texreg(out.mi.matmor3),
  cc_to_texreg(underfive.1), mi_to_texreg(out.mi.matmor4)
),
  custom.header = list(
    "Maternal Mortality" = 1:2,
    "Neonatal Mortality" = 3:4,
    "Infant Mortality" = 5:6,
    "Under-5 Mortality" = 7:8
  ),
  custom.model.names = rep(c("Complete Cases", "MI (n=20)"), 4),
  custom.coef.map = list(
    "armed_conflict" = "Armed Conflict",
    "log_gdp" = "Log GDP per capita",
    "oecd" = "OECD Member",
    "pop_dens" = "Population Density",
    "urban" = "Urban Residence",
    "age_dep" = "Age Dependency Ratio",
    "male_edu" = "Male Education",
    "temp" = "Temperature",
    "rainfall_1000" = "Rainfall",
    "earthquake" = "Earthquake",
    "drought" = "Drought"
  ),
  file = output_file)
