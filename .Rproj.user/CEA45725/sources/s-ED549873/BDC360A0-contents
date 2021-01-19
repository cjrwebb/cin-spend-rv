#################################################################################
################ In Defence of Ordinary Help: CIN & Spend: SCRIPT ###############
#################################################################################

# This code replicates the analysis and graphics from the research paper:
# "'In Defence of Ordinary Help: Estimating the effect of Early Help/Family 
# Support Spending on Children in Need Rates in England using ALT-SR".

# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(janitor)
library(GGally)
library(lavaan)
library(fixest)
library(boot)
library(stargazer)
library(skimr)
library(patchwork)


# Read in and tidy data ----------------------------------------------------

# Get variable names
csc_variables <- read_rds("data/csc_data_v3.RDS") %>%
  .$description %>%
  unique()

vars <- csc_variables[c(7, 15, 18)]


# Get data with relevant variables and tidy into wide format
cwi_data <- read_rds("data/csc_data_v3.RDS") %>%
  filter(description %in% vars) %>%
  pivot_wider(names_from = description, values_from = value) %>%
  rename(ehfs_sp = 4, cin = 5, imd = 6) %>%
  mutate_at(vars(ehfs_sp:imd), ~round(., 2)) %>%
  pivot_wider(names_from = year, values_from = ehfs_sp:cin)

# Add region
reg_lu <- read_csv("data/la_region.csv")

# Check for incorrect IDs
anti_join(cwi_data, reg_lu, by = c("new_la_code" = "la_code")) # Only IoS, removed as outlier anyway

cwi_data <- left_join(cwi_data, reg_lu %>% select(-la_name), by = c("new_la_code" = "la_code"))

# Create Inner London variable
cwi_data <- cwi_data %>%
  mutate(inr_ldn = ifelse(la_name %in% c("Camden", "Greenwich", "Hackney", "Hammersmith and Fulham",
                                         "Islington", "Kensington and Chelsea", "Lambeth",
                                         "Lewisham", "Southwark", "Tower Hamlets", "Wandsworth",
                                         "Westminster"), 1, 0))


# Exclude Isles of Scilly and City of London as outliers with missing data, and Haringey as CIN outlier
cwi_data <- cwi_data %>% filter(!la_name %in% c("Isles Of Scilly", "City of London", "Haringey"))

# Visually Inspect Normality
pairs_plot <- ggpairs(cwi_data %>% select(imd:cin_2019)) 

# Shapiro-Wilks tests
sw_tests <- tibble(var = as.character(), W = as.numeric(), p = as.numeric())

for (i in 3:(length(cwi_data) - 2)) {
  
  sw_tests <- sw_tests %>% add_row(
    var = names(cwi_data)[i], W = shapiro.test(cwi_data[[i]])$statistic[[1]], p = shapiro.test(cwi_data[[i]])$p 
  )
  
}

sw_tests


# Descriptive statistics prior to transformations
cwi_data %>%
  select(imd:cin_2019) %>%
  relocate(imd, .after = cin_2019) %>%
  skim()

# Save skimr output as table
cwi_descriptive <- cwi_data %>%
  select(imd:cin_2019) %>%
  relocate(imd, .after = cin_2019) %>%
  skim_without_charts() %>%
  as.tibble(.) %>%
  select(-skim_type, -numeric.p0, -numeric.p100) %>%
  mutate(n = 149 - n_missing, .before = n_missing) %>%
  mutate(skim_variable = str_replace(skim_variable, "ehfs_sp_", "Early Help/Family Support Spend per Child ")) %>%
  mutate(skim_variable = str_replace(skim_variable, "cin_", "Children in Need Rate per 10,000 ")) %>%
  mutate(skim_variable = ifelse(skim_variable == "imd", "Indices of Multiple Deprivation Score", skim_variable)) %>%
  mutate_at(vars(complete_rate:numeric.p75), ~round(., 2)) %>%
  rename(Variable = 1, N = 2, `N Missing` = 3, `Complete Rate` = 4, Mean = 5, SD = 6, 
         `25th Percentile` = 7, `50th Percentile` = 8, `75th Percentile` = 9) 

cwi_descriptive

stargazer(cwi_descriptive, type = "html", summary = FALSE, rownames = FALSE, out = "descriptive_stats.html")  

# Create visualisation of CIN and EHFS spending changes 
cwi_data %>%
  pivot_longer(cols = ehfs_sp_2011:cin_2019, names_to = c(".value", "year"), names_pattern = "(.+)_(.+)") %>%
  mutate(year = as.numeric(year)) 

spend_plot <- cwi_data %>%
  pivot_longer(cols = ehfs_sp_2011:cin_2019, names_to = c(".value", "year"), names_pattern = "(.+)_(.+)") %>%
  mutate(year = as.numeric(year)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = year, y = ehfs_sp, group = la_name),
            se = F, col = "black", size = 0.3, alpha = 0.2) +
  geom_smooth(aes(x = year, y = ehfs_sp), method = "loess", size = 2, se = F, col = "black") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(breaks = seq(0, 1500, 250)) +
  xlab("Year Ending") +
  ylab("Non-Safeguarding, Non-CLA Expenditure per Child")


cin_plot <- cwi_data %>%
  pivot_longer(cols = ehfs_sp_2011:cin_2019, names_to = c(".value", "year"), names_pattern = "(.+)_(.+)") %>%
  mutate(year = as.numeric(year)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = year, y = cin, group = la_name),
            se = F, col = "black", size = 0.3, alpha = 0.2) +
  geom_smooth(aes(x = year, y = cin), method = "loess", size = 2, se = F, col = "black") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(breaks = seq(0, 800, 100)) +
  xlab("Year Ending") +
  ylab("Children in Need Rate per 10,000")

spend_plot + cin_plot

# Create logged version of data
cwi_data <- cwi_data %>% mutate_at(vars(ehfs_sp_2011:cin_2019), ~log(.)) %>%
  mutate(imd = scale(imd)[,1])

cwi_data %>% .$la_name %>% unique(.) %>% sort(.)

# Re-run pair plots and shapiro-wilks tests
pairs_plot_logged <- ggpairs(cwi_data %>% select(imd:cin_2019)) 

# Identify any additional outliers
cwi_data %>%
  arrange(ehfs_sp_2011) # Wiltshire

cwi_data %>%
  arrange(ehfs_sp_2014) # Wokingham

cwi_data %>%
  arrange(ehfs_sp_2015) # Wokingham

cwi_data %>%
  arrange(ehfs_sp_2017) # Wokingham

cwi_data %>%
  arrange(ehfs_sp_2019) # Northamptonshire

cwi_data %>%
  arrange(cin_2012) %>%
  select(la_name, cin_2011:cin_2019) # Wokingham

cwi_data %>%
  arrange(desc(cin_2013)) %>%
  select(la_name, cin_2011:cin_2019) # Middlesborough

cwi_data %>%
  arrange(cin_2014) %>%
  select(la_name, cin_2011:cin_2019) # Wokingham

cwi_data %>%
  arrange(cin_2015) %>%
  select(la_name, cin_2011:cin_2019) # Wokingham & Leicestershire

cwi_data %>%
  arrange(cin_2016) %>%
  select(la_name, cin_2011:cin_2019) # Wokingham & Leicestershire

# Create dataset with these outlier data points removed, if needed
# Above identification indicates these are 'true' outliers, in other words,
# that they are outliers but this is not due to a recording error/one-off
# external event (like Haringey). They should therefore be retained as 
# a representative sample of the population, but bootstrapped or robust confidence
# intervals are preferred.

cwi_data_olr <- cwi_data %>%
  mutate(ehfs_sp_2011 = ifelse(la_name == "Wiltshire", NA, ehfs_sp_2011),
         ehfs_sp_2014 = ifelse(la_name == "Wokingham", NA, ehfs_sp_2014),
         ehfs_sp_2015 = ifelse(la_name == "Wokingham", NA, ehfs_sp_2015),
         ehfs_sp_2017 = ifelse(la_name == "Wokingham", NA, ehfs_sp_2017),
         ehfs_sp_2019 = ifelse(la_name == "Northamptonshire", NA, ehfs_sp_2019),
         cin_2012 = ifelse(la_name == "Wokingham", NA, cin_2012),
         cin_2013 = ifelse(la_name == "Middlesborough", NA, cin_2013),
         cin_2014 = ifelse(la_name == "Wokingham", NA, cin_2014),
         cin_2015 = ifelse(la_name == "Wokingham|Leicestershire", NA, cin_2015),
         cin_2016 = ifelse(la_name == "Wokingham|Leicestershire", NA, cin_2016),
         )


# Shapiro-Wilks tests outliers included
sw_tests <- tibble(var = as.character(), W = as.numeric(), p = as.numeric())

for (i in 3:(length(cwi_data) - 2)) {
  
  sw_tests <- sw_tests %>% add_row(
    var = names(cwi_data)[i], W = shapiro.test(cwi_data[[i]])$statistic[[1]], p = shapiro.test(cwi_data[[i]])$p 
  )
  
}

sw_tests


# Model Building ----------------------------------------------------------
# Model structures/input can be found in models directory for readability.
# Running models may require changing ncpu values according to hardware.


# Latent Curve/Growth Model Specification: Expenditure ---------------------

# Model 1: Random Intercept Model (no constrained residual variance) 
m1_inp <- read_file("models/model_1.txt")
m1_inp
m1_fit <- lavaan::sem(m1_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m1_fit, fit.measures = TRUE) 

# Model 2: RI-Random Slopes Model (no constrained residual variance)
m2_inp <- read_file("models/model_2.txt")
m2_inp
m2_fit <- lavaan::sem(m2_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m2_fit, fit.measures = TRUE) 

# Model 3: RI-RS Fixed Quadratic Model (no autoregressive residual variance)
m3_inp <- read_file("models/model_3.txt")
m3_inp
m3_fit <- lavaan::sem(m3_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m3_fit, fit.measures = TRUE) 
parameterestimates(m3_fit)
fitmeasures(m3_fit, c("cfi", "tli", "srmr"))
inspect(m3_fit, "rsquare")

# Model 4: RI-RS-RQ model
m4_inp <- read_file("models/model_4.txt")
m4_inp
m4_fit <- lavaan::sem(m4_inp, data = cwi_data, optim.dx.tol = 0.01, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler") # Poor fitting model, requires high tol to converge
summary(m4_fit, fit.measures = TRUE) 

# Model 5: RI-RS-FQ AR
m5_inp <- read_file("models/model_5.txt")
m5_inp
m5_fit <- lavaan::sem(m5_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m5_fit, fit.measures = TRUE) 




# Latent Growth Specification - CIN ---------------------------------------

# Model 6: RI Model CIN (no autoregressive residual variance)
m6_inp <- read_file("models/model_6.txt")
m6_inp
m6_fit <- lavaan::sem(m6_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m6_fit, fit.measures = TRUE) 
fitmeasures(m6_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Model 7: RI-RS Model CIN (no autoregressive residual variance)
m7_inp <- read_file("models/model_7.txt")
m7_inp
m7_fit <- lavaan::sem(m7_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m7_fit, fit.measures = TRUE) 
fitmeasures(m7_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Model 8: RI-RS Model CIN (autoregressive residual variance)
m8_inp <- read_file("models/model_8.txt")
m8_inp
m8_fit <- lavaan::sem(m8_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m8_fit, fit.measures = TRUE) 
fitmeasures(m8_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))


# Combined Latent Curve Model - Cross Lag Specification -------------------

# Model 9: Combined LCM (equal time-specific residual covariance, no cross-lag)
m9_inp <- read_file("models/model_9.txt")
m9_inp
m9_fit <- lavaan::sem(m9_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m9_fit, fit.measures = TRUE) 
fitmeasures(m9_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Model 10: Combined LCM (one-way cross-lag, Spend -> CIN)
m10_inp <- read_file("models/model_10.txt")
m10_inp
m10_fit <- lavaan::sem(m10_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m10_fit, fit.measures = TRUE) 
fitmeasures(m10_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Model 11: Combined LCM (one-way cross-lag, Spend -> CIN) fixed
m11_inp <- read_file("models/model_11.txt")
m11_inp
m11_fit <- lavaan::sem(m11_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m11_fit, fit.measures = TRUE) 
fitmeasures(m11_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Model 12: Combined LCM (one-way cross-lag, CIN -> Spend) Free
m12_inp <- read_file("models/model_12.txt")
m12_inp
m12_fit <- lavaan::sem(m12_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m12_fit, fit.measures = TRUE)
fitmeasures(m12_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Model 13: Combined LCM (one-way cross-lag, CIN -> Spend) Fixed
m13_inp <- read_file("models/model_13.txt")
m13_inp
m13_fit <- lavaan::sem(m13_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", estimator = "MLR", test = "Satorra.Bentler")
summary(m13_fit, fit.measures = TRUE)
fitmeasures(m13_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))


# Full Autoregressive Latent Trajectory Model with SR ---------------------
# Model 14: Cross-lagged regressions full ALT-SR
m14_inp <- read_file("models/model_14.txt")
m14_inp
m14_fit <- lavaan::sem(m14_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m14_fit, fit.measures = TRUE)
fitmeasures(m14_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

m14.2_inp <- read_file("models/model_14.2.txt")
m14.2_inp
m14.2_fit <- lavaan::sem(m14.2_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m14.2_fit, fit.measures = TRUE)
fitmeasures(m14.2_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))


# ALT SR with Time Invariant Covariate: ------------------------------------------------------------

# Model 15: Cross-lagged regressions full ALT-SR with time invariant predictor IMD
m15_inp <- read_file("models/model_15.txt")
m15_inp
m15_fit <- lavaan::sem(m15_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m15_fit, fit.measures = TRUE)
fitmeasures(m15_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Model 16: Cross-lagged regressions - fixed - full ALT-SR with time invariant predictor IMD
m16_inp <- read_file("models/model_16.txt")
m16_inp
m16_fit <- lavaan::sem(m16_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m16_fit, fit.measures = TRUE)
fitmeasures(m16_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))


# Comparisons to other models - CLPM --------------------------------------

# Model 17 Conventional Cross-Lagged Panel Model - freely estimated cross-lags
m17_inp <- read_file("models/model_17.txt")
m17_inp
m17_fit <- lavaan::sem(m17_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m17_fit, fit.measures = TRUE) 
fitmeasures(m17_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Model 18 Conventional Cross-Lagged Panel Model - Fixed cross lags
m18_inp <- read_file("models/model_18.txt")
m18_inp
m18_fit <- lavaan::sem(m18_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m18_fit, fit.measures = TRUE)
fitmeasures(m18_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Comparisons to other models - FE PLM --------------------------------------

# Model 19 Fixed Effects Lagged Regression Model - Spend -> CIN
cwi_data

cwi_data_long <- cwi_data %>%
  pivot_longer(cols = ehfs_sp_2011:cin_2019, names_to = c(".value", "year"), names_pattern = "(.+)_(.+)")

cwi_data_long <- cwi_data_long %>%
  mutate(year = as.numeric(year) - 2011) 

# Spend -> CIN
fe_3 <- feols(cin ~ l(cin, 1) + l(ehfs_sp, 1) + year | la_name[year], data = cwi_data_long, panel.id = c("la_name", "year"))
summary(fe_3, cluster = "la_name")
AIC(fe_3)
BIC(fe_3)

fe_3.2 <- feols(cin ~ l(ehfs_sp, 1) + year | la_name[year], data = cwi_data_long, panel.id = c("la_name", "year"))
summary(fe_3.2, cluster = "la_name")
AIC(fe_3.2)
BIC(fe_3.2)

# Custom bootstrap routine for fixest models estimates for AR and CL - bias corrected and accelerated
# Does not work with large number of fixed effects over time because of duplication - clustered SEs used instead

# boot_fixest_cl_ldv <- function(data, i) {
#   
#   data_bs <- panel(data, panel.id = c("la_name", "year"), duplicate.method = "first")[i,]
#   return(coef(feols(cin ~ l(cin, 1) + l(ehfs_sp, 1) | la_name, data = data_bs, warn = FALSE, notes = FALSE)))
#   
# }
# 
# boot_fixest_cl_ldv(cwi_data_long)
# 
# boot_test_fe3 <- boot(cwi_data_long, boot_fixest_cl_ldv, R=200, sim = "balanced")
# boot.ci(boot.out = boot_test_fe3, type = "bca")
# 
# boot_fixest_cl <- function(data, i) {
#   
#   data_bs <- panel(data, panel.id = c("la_name", "year"), duplicate.method = "first")[i,]
#   return(coef(feols(cin ~ l(ehfs_sp, 1) | la_name, data = data_bs, warn = FALSE, notes = FALSE)))
#   
# }
# 
# 
# boot(cwi_data_long, boot_fixest_cl, R=200, sim = "balanced")


# Maximum Likelihood Dynamic Panel Model --------

#### ML Estimation of Dynamic Panel Models - Allison, Williams and Moral-Benito - freed cross-lags
m19_inp <- read_file("models/model_19.txt")
m19_inp
m19_fit <- lavaan::sem(m19_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m19_fit, fit.measures = TRUE)
fitmeasures(m19_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# Model 20 ML-SEM DPM - Fixed Estimates
m20_inp <- read_file("models/model_20.txt")
m20_inp
m20_fit <- lavaan::sem(m20_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m20_fit, fit.measures = TRUE)
fitmeasures(m20_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))


# ALT-SR models with fixed and free cross-lags for comparisons ------------

### Model 21 ALT-SR - Free lags - No exogenous between-cases variables 
m21_inp <- read_file("models/model_21.txt")
m21_inp
m21_fit <- lavaan::sem(m21_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m21_fit, fit.measures = TRUE)
fitmeasures(m21_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

### Model 22 ALT-SR - Fixed lags - No exogenous between-cases variables 
m22_inp <- read_file("models/model_22.txt")
m22_inp
m22_fit <- lavaan::sem(m22_inp, data = cwi_data, missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m22_fit, fit.measures = TRUE)
fitmeasures(m22_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))

# AIC and BIC from fixed effects model
AIC(fe_3)
BIC(fe_3)


# Bootstrapping models -----------------------------------------------
# Models to be reported in results tables - bootstrapped versions with bias corrected confidence intervals (95%) when SEM

# ALT-SR, Free Cross-lags - model 21
m21_inp <- read_file("models/model_21.txt")
m21_fit_b <- lavaan::sem(m21_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", se = "bootstrap", 
                       bootstrap = 10000, parallel = "multicore", ncpus = 8)

# ALT-SR, Fixed Cross-lags - model 22
m22_inp <- read_file("models/model_22.txt")
m22_fit_b <- lavaan::sem(m22_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", se = "bootstrap", 
                       bootstrap = 10000, parallel = "multicore", ncpus = 8)

# ALT-SR, Free Cross-lags, with IMD between-cases covariates - model 15
m15_inp_b <- read_file("models/model_15.txt")
m15_fit_b <- lavaan::sem(m15_inp_b, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", se = "bootstrap", 
                         bootstrap = 10000, parallel = "multicore", ncpus = 8)

# Traditional CLPM - Freed Cross Lags - Model 17
m17_inp <- read_file("models/model_17.txt")
m17_fit_b <- lavaan::sem(m17_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", se = "bootstrap", 
                       bootstrap = 10000, parallel = "multicore", ncpus = 8)

# Traditional CLPM - Fixed Cross Lag - Model 18
m18_inp <- read_file("models/model_18.txt")
m18_fit_b <- lavaan::sem(m18_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", se = "bootstrap", 
                       bootstrap = 10000, parallel = "multicore", ncpus = 8)

# Fixed Effects Lagged Panel Model with lagged dependent variable (AR) and varying year FEs - fe_3
summary(fe_3, cluster = "la_name")

# Fixed effects lagged panel model without lagged dependent variable but including varying year FEs - fe_3.2
summary(fe_3.2, cluster = "la_name")

# Maximum Likelihood Dynamic Panel Model with free CL - Model 19
m19_inp <- read_file("models/model_19.txt")
m19_fit_b <- lavaan::sem(m19_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", se = "bootstrap", 
                       bootstrap = 10000, parallel = "multicore", ncpus = 8)

# Maximum Likelihood Dynamic Panel Model with fixed AR and CL - Model 20
m20_inp <- read_file("models/model_20.txt")
m20_fit_b <- lavaan::sem(m20_inp, data = cwi_data, optim.dx.tol = 0.005, missing = "fiml", se = "bootstrap", 
                       bootstrap = 10000, parallel = "multicore", ncpus = 8)


# Get robust model fit for table two from MLR -------------------------------------------

fitmeasures(m21_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic")) # ALT-SR Free
fitmeasures(m22_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic")) # ALT-SR Fixed
fitmeasures(m17_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic")) # CLPM Free
fitmeasures(m18_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic")) # CLPM Fixed
fitmeasures(m19_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic")) # ML-DPM Free
fitmeasures(m20_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic")) # ML-DPM Fixed

# Get parameter point estimates and BCa Bootstrap confidence intervals ------------------

# ---------------- ALT-SR - Free - Model 21 Parameters
parameterestimates(m21_fit_b, boot.ci.type = "bca.simple", level = 0.95)

# Autoregression
parameterestimates(m21_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(label == "a2")
# Cross lags
parameterestimates(m21_fit_b, boot.ci.type = "bca.simple", level = 0.95)[77:92, ] %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]"))
# Latent variable means
parameterestimates(m21_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(str_detect(label, "mu"))
# Correlations
parameterestimates(m21_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(str_detect(label, "cor")) %>% select(-rhs)

# ---------------- ALT-SR - Fixed Parameters
parameterestimates(m22_fit_b, boot.ci.type = "bca.simple", level = 0.95)

# Autoregression
parameterestimates(m22_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(label == "a2")
# Cross lags
parameterestimates(m22_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(str_detect(label, "cl"))
# Latent variable means
parameterestimates(m22_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(str_detect(label, "mu"))
# Latent variable correlations
parameterestimates(m22_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(str_detect(label, "cor")) %>% select(-rhs)

# ---------------- CLPM - Free Parameters
parameterestimates(m17_fit_b, boot.ci.type = "bca.simple", level = 0.95)

# Autoregression
parameterestimates(m17_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(str_detect(label, "a"))

# Cross lags
parameterestimates(m17_fit_b, boot.ci.type = "bca.simple", level = 0.95)[26:41, ] %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]"))

# ---------------- CLPM - Fixed Parameters
parameterestimates(m18_fit_b, boot.ci.type = "bca.simple", level = 0.95)

# Autoregression
parameterestimates(m18_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(str_detect(label, "a"))

# Cross lags
parameterestimates(m18_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(str_detect(label, "cl"))


# ---------------- Fixed Effects Linear Panel Model

#  FE LPM Lagged Dependent Variable
summary(fe_3)
confint(fe_3)

#  FE LPM No Lagged Dependent Variable
summary(fe_3.2)
confint(fe_3.2)


# ---------------- Maximum Likelihood Dynamic Panel Model

# ML-DPM Free Lags
parameterestimates(m19_fit_b, boot.ci.type = "bca.simple", level = 0.95)

# Autoregression
parameterestimates(m19_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(label == "ar")

# Cross lags
parameterestimates(m19_fit_b, boot.ci.type = "bca.simple", level = 0.95)[9:24, ] %>%
  filter(label != "ar") %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]"))

# ML-DPM Fixed Lags
parameterestimates(m20_fit_b, boot.ci.type = "bca.simple", level = 0.95)

# Autoregression
parameterestimates(m20_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(label == "ar")

# Cross-lag
parameterestimates(m20_fit_b, boot.ci.type = "bca.simple", level = 0.95) %>%
  mutate(value = paste0(round(est, 3), " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")) %>%
  filter(label == "cl1")



# Get final model details for table 3 -------------------------------------
params_m21 <- parameterestimates(m21_fit_b, ci = TRUE, rsquare = TRUE)
params_m15 <- parameterestimates(m15_fit_b, ci = TRUE, rsquare = TRUE)

model_params <- right_join(params_m21, params_m15, by = c("lhs", "op", "rhs", "label"))

stargazer(model_params, type = "html", summary = FALSE, out = "model_results.html") 

fitmeasures(m15_fit, c("cfi.robust", "tli.robust", "srmr", "aic", "bic"))


# Run final model with outliers excluded ----------------------------------

m14_fit_olr <- lavaan::sem(m14_inp, data = cwi_data_olr, 
                           missing = "fiml", optim.dx.tol = 0.005, estimator = "MLR", test = "Satorra.Bentler")
summary(m14_fit_olr, fit.measures = TRUE)



# Additional Tables/Figures -----------------------------------------------

## Recreate illustrative example adjusting for trends:

example_data_wide <- tibble(
  Spend = c(4, 3.9, 3.7, 3.6, 4.0, 3.5, 3.3, 3.4, 3.3),
  CIN = c(3.05, 2.95, 2.9, 2.95, 3.1, 2.9, 3, 3.05, 2.95),
  year = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
) 

example_data_long <- example_data_wide %>% pivot_longer(Spend:CIN) 

# linear trajectory parameters
lm(data = example_data_wide, Spend ~ year)
lm(data = example_data_wide, CIN ~ year)

plot1 <- example_data_long %>%
  ggplot() +
  geom_point(aes(x = year, y = value, pch = name), size = 4) +
  geom_line(aes(x = year, y = value, lty = name), size = 1) +
  geom_line(stat = "smooth", method = lm, aes(x = year, y = value, lty = name), 
            size = 1, se = F, col = "black", alpha = 0.4) +
  theme_minimal() +
  ylab("Spend (£100s)/CIN Rate (per 100)") +
  xlab("Year Ending") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(limits = c(2.5, 4.5)) +
  ggtitle("a) Longitudinal data without controlling for trajectories") +
  theme(legend.position = "bottom", legend.key.width = unit(1, "in"),
        legend.title = element_blank())

plot2 <- example_data_wide %>%
  mutate(Spend = Spend - (178.26667 + -0.08667*year),
         CIN = CIN - (1.304167 + 0.0008333*year)) %>%
  pivot_longer(Spend:CIN) %>%
  ggplot() +
  geom_point(aes(x = year, y = value, pch = name), size = 4) +
  geom_line(aes(x = year, y = value, lty = name), size = 1) +
  geom_line(stat = "smooth", method = lm, aes(x = year, y = value, lty = name), 
            size = 1, se = F, col = "black", alpha = 0.4) +
  theme_minimal() +
  ylab("Spend (£100s)/CIN Rate (per 100) (Trend Centred)") +
  xlab("Year Ending") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(limits = c(-0.2, 0.5)) +
  ggtitle("b) Longitudinal data centered around trajectories") +
  theme(legend.position = "bottom", legend.key.width = unit(1, "in"),
        legend.title = element_blank())

example_data_wide %>%
  mutate(Spend = Spend - (178.26667 + -0.08667*year),
         CIN = CIN - (1.304167 + 0.0008333*year)) %>%
  pivot_longer(Spend:CIN)

plot3 <- example_data_wide %>%
  mutate(Spend = Spend - (178.26667 + -0.08667*year),
         CIN = CIN - (1.304167 + 0.0008333*year)) %>%
  pivot_longer(Spend:CIN) %>%
  ggplot() +
  geom_point(aes(x = year, y = value, pch = name), size = 4) +
  geom_line(aes(x = year, y = value, lty = name), size = 1, alpha = 0.2) +
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.3) +
  geom_curve(aes(x = 2011, xend = 2011.95, y = 0.0267, yend = -0.0308), 
             arrow = arrow(type = "open", length = unit(0.1, "in"))) +
  geom_curve(aes(x = 2012, xend = 2013, y = 0.0134, yend = -0.0750), 
             arrow = arrow(type = "open", length = unit(0.1, "in")), curvature = -0.5) +
  geom_curve(aes(x = 2013, xend = 2013.95, y = -0.100, yend = -0.0374), 
             arrow = arrow(type = "open", length = unit(0.1, "in")), curvature = 0.3) +
  geom_curve(aes(x = 2014, xend = 2015, y = -0.113, yend = 0.105), 
             arrow = arrow(type = "open", length = unit(0.1, "in")), curvature = 0.5) +
  geom_curve(aes(x = 2015, xend = 2015.95, y = 0.373, yend = -0.0795), 
             arrow = arrow(type = "open", length = unit(0.1, "in")), curvature = 0.1) +
  geom_curve(aes(x = 2016, xend = 2016.95, y = -0.0400, yend = 0.0151), 
             arrow = arrow(type = "open", length = unit(0.1, "in")), curvature = -0.5) +
  geom_curve(aes(x = 2017, xend = 2017.95, y = -0.153, yend = 0.0642), 
             arrow = arrow(type = "open", length = unit(0.1, "in")), curvature = -0.1) +
  geom_curve(aes(x = 2018, xend = 2018.95, y = 0.0334, yend = -0.0366), 
             arrow = arrow(type = "open", length = unit(0.1, "in")), curvature = 0.5) +
  theme_minimal() +
  ylab("Spend (£100s)/CIN Rate (per 100) (Trend Centred)") +
  xlab("Year Ending") +
  ggtitle("c) Values above/below 0 now represent deviations within trajectories for analysing within-case lagged associations") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(limits = c(-0.2, 0.5)) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "in"),
        legend.title = element_blank())

figure2 <- (plot1 + plot2) / plot3
figure2

# Tiff version (for journal)
ggsave(plot = figure2, filename = "fig_2.tiff", path = "plots/", 
       width = 12, height = 10, units = "in", device = "tiff", dpi = 400)

# PNG version
ggsave(plot = figure2, filename = "fig_2.png", path = "plots/", 
       width = 12, height = 10, units = "in", device = "png", dpi = 800)


# Graphical version of rates changes (Descriptive) - not used in final draft

rates_1 <- tibble(year = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
       cin_all = c(368, 352, 359, 370, 363, 363, 356, 366, 361),
       spend_all = c(483, 383, 354, 330, 309, 277, 251, 234, 228),
       cin_mostdep = c(448, 429, 441, 449, 446, 427, 434, 445, 438),
       spend_mostdep = c(635, 469, 430, 394, 357, 316, 288, 275, 272),
       cin_leastdep = c(283, 270, 271, 286, 289, 286, 284, 295, 289),
       spend_leastdep = c(329, 292, 267, 257, 245, 222, 200, 185, 180)
       )

rateplot1 <- rates_1 %>%
  select(year, cin_all, spend_all) %>%
  pivot_longer(cin_all:spend_all) %>%
  mutate(name = ifelse(str_detect(name, "cin"), "CIN Rate", "Spend")) %>%
  ggplot() +
  geom_point(aes(x = year, y = value, pch = name), size = 4) +
  geom_line(aes(x = year, y = value, lty = name), size = 1) +
  theme_minimal() +
  ylab("Spend/CIN Rate") +
  xlab("Year Ending") +
  ggtitle("a) All Local Authorities") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(limits = c(100, 700), breaks = seq(100, 800, 50)) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "in"),
        legend.title = element_blank())

rateplot2 <- rates_1 %>%
  select(year, cin_mostdep, spend_mostdep) %>%
  pivot_longer(cin_mostdep:spend_mostdep) %>%
  mutate(name = ifelse(str_detect(name, "cin"), "CIN Rate", "Spend")) %>%
  ggplot() +
  geom_point(aes(x = year, y = value, pch = name), size = 4) +
  geom_line(aes(x = year, y = value, lty = name), size = 1) +
  theme_minimal() +
  ylab("Spend/CIN Rate") +
  xlab("Year Ending") +
  ggtitle("b) Most Deprived Third of Local Authorities") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(limits = c(100, 700), breaks = seq(100, 800, 50)) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "in"),
        legend.title = element_blank())

rateplot3 <- rates_1 %>%
  select(year, cin_leastdep, spend_leastdep) %>%
  pivot_longer(cin_leastdep:spend_leastdep) %>%
  mutate(name = ifelse(str_detect(name, "cin"), "CIN Rate", "Spend")) %>%
  ggplot() +
  geom_point(aes(x = year, y = value, pch = name), size = 4) +
  geom_line(aes(x = year, y = value, lty = name), size = 1) +
  theme_minimal() +
  ylab("Spend/CIN Rate") +
  xlab("Year Ending") +
  ggtitle("c) Least Deprived Third of Local Authorities") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(limits = c(100, 700), breaks = seq(100, 800, 50)) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "in"),
        legend.title = element_blank())


figure1 <- rateplot1 / (rateplot2 + rateplot3)

# Tiff version (for journal)
ggsave(plot = figure1, filename = "fig_1.tiff", path = "plots/", 
       width = 10, height = 10, units = "in", device = "tiff", dpi = 400)

# PNG version
ggsave(plot = figure1, filename = "fig_1.png", path = "plots/", 
       width = 10, height = 10, units = "in", device = "png", dpi = 800)


#### Latent growth plots - back-transformed

# Get growth estimates for each case
growth_preds <- lavPredict(m15_fit_b) %>%
  as.tibble() %>%
  mutate_all(~as.numeric(.))

# Multiply rows by 9 and add year (x) variable for intercept + 8 time points
# then calculate predicted value for each case 
growth_preds <- growth_preds %>%
  mutate(id = row_number()) %>%
  slice(rep(1:n(), each = 9)) %>%
  mutate(year = rep(0:8, 149)) %>%
  mutate(cin_pred = i_cin + (year*s_cin),
         exp_pred = i1_exp + (year*s1_exp) + ((year*year)*q_exp))


lg_plot1 <- growth_preds %>%
  pivot_longer(cin_pred:exp_pred) %>%
  mutate(name = ifelse(name == "exp_pred", 
                       "Expenditure Latent Growth (Adjusted for Within-LA Dynamics)", 
                       "CIN Rate Latent Growth (Adjusted for Within-LA Dynamics)")) %>%
  ggplot() +
  geom_line(aes(x = year+2011, y = exp(value), group = id),
            alpha = 0.2, stat = "smooth", method = "loess") +
  ylab("Predicted Value (Back-transformed)") +
  xlab("Year Ending") +
  scale_x_continuous(breaks = seq(2011, 2019, 2)) +
  facet_wrap(~ name) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12))

# Simple slopes values for low, average, and high deprivation

low = -1
average = 0
high = 1

# Calculate simple slopes at high, mid, and low values of IMD
imd_simpleslopes <- tibble(
  year = 0:8,
  cin_pred_low = (5.848 + (0.191*low)) + (-0.174 + (-0.007*low))*year,
  exp_pred_low = (6.107 + (0.234*low)) + (0.089 + (-0.018*low))*year + -0.054*(year*year),
  cin_pred_average = (5.848 + (0.191*average)) + (-0.174 + (-0.007*average))*year,
  exp_pred_average = (6.107 + (0.234*average)) + (0.089 + (-0.018*average))*year + -0.054*(year*year),
  cin_pred_high = (5.848 + (0.191*high)) + (-0.174 + (-0.007*high))*year,
  exp_pred_high = (6.107 + (0.234*high)) + (0.089 + (-0.018*high))*year + -0.054*(year*year),
)


lg_plot2 <- imd_simpleslopes %>%
  pivot_longer(cin_pred_low:exp_pred_high) %>%
  separate(name, into = c("var", "xval"), sep = 4) %>%
  mutate(var = ifelse(var == "cin_", 
                      "CIN Rate Latent Growth (Adjusted for Within-LA Dynamics)",
                      "Expenditure Latent Growth (Adjusted for Within-LA Dynamics)"),
         xval = case_when(xval == "pred_low" ~ "Low Deprivation (-1SD)",
                          xval == "pred_average" ~ "Average Deprivation (Mean)",
                          TRUE ~ "High Deprivation (+1SD)")) %>%
  ggplot() +
  geom_line(aes(x = year + 2011, y = exp(value), lty = xval), 
            stat = "smooth", method = "loess", size = 1) +
  facet_wrap(~ var) +
  theme_minimal() +
  xlab("Year Ending") +
  ylab("Predicted Value (Back-transformed)") +
  scale_x_continuous(breaks = seq(2011, 2019, 2)) +
  scale_y_continuous(limits = c(0, 1200)) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "in"),
        legend.title = element_blank(), strip.text = element_text(size = 12))

fig3 <- lg_plot1 / lg_plot2

fig3

# Tiff version (for journal)
ggsave(plot = fig3, filename = "fig_3.tiff", path = "plots/", 
       width = 12, height = 10, units = "in", device = "tiff", dpi = 400)

# PNG version
ggsave(plot = fig3, filename = "fig_3.png", path = "plots/", 
       width = 12, height = 10, units = "in", device = "png", dpi = 800)




