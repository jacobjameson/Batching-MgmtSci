### complier share

#=========================================================================
# CORRECT METHOD: Use batch.tendency (the instrument), not raw rates
#=========================================================================

# Define extremes of the INSTRUMENT (batch.tendency), following Dobbie 1st/99th percentiles
z_bar <- quantile(final$batch.tendency, 0.99)      # "most lenient" (highest batch tendency)
z_underbar <- quantile(final$batch.tendency, 0.01) # "most strict" (lowest batch tendency)

cat("=== INSTRUMENT EXTREMES ===\n")
cat("99th percentile of batch.tendency (z_bar):", z_bar, "\n")
cat("1st percentile of batch.tendency (z_underbar):", z_underbar, "\n")

#=========================================================================
# METHOD 1: Local Linear Regression (what Dobbie uses for their main results)
#=========================================================================

# Fit loess of batched on batch.tendency
loess_fit <- loess(batched ~ batch.tendency, data = final, span = 0.75)

# Predict at the extremes
p_batch_zbar_loess <- predict(loess_fit, newdata = data.frame(batch.tendency = z_bar))
p_batch_zunderbar_loess <- predict(loess_fit, newdata = data.frame(batch.tendency = z_underbar))

pi_c_loess <- p_batch_zbar_loess - p_batch_zunderbar_loess
pi_a_loess <- p_batch_zunderbar_loess
pi_n_loess <- 1 - p_batch_zbar_loess

cat("\n=== LOCAL LINEAR MODEL (Dobbie's preferred) ===\n")
cat("P(Batched | Z = z_bar):", round(p_batch_zbar_loess, 3), "\n")
cat("P(Batched | Z = z_underbar):", round(p_batch_zunderbar_loess, 3), "\n")
cat("Complier share:", round(pi_c_loess * 100, 1), "%\n")
cat("Always-taker share:", round(pi_a_loess * 100, 1), "%\n")
cat("Never-taker share:", round(pi_n_loess * 100, 1), "%\n")
cat("Sum:", round((pi_c_loess + pi_a_loess + pi_n_loess) * 100, 1), "%\n")

#=========================================================================
# METHOD 2: Linear Model (Dobbie also reports this)
#=========================================================================

# From the first stage: Batched = α₀ + α₁ × batch.tendency + controls + ε
# π_c = α₁ × (z_bar - z_underbar)
# π_a = α₀ + α₁ × z_underbar  
# π_n = 1 - α₀ - α₁ × z_bar

# Simple first stage without controls (to get α₀ and α₁)
first_stage_simple <- lm(batched ~ batch.tendency, data = final)
alpha_0 <- coef(first_stage_simple)["(Intercept)"]
alpha_1 <- coef(first_stage_simple)["batch.tendency"]

cat("\n=== LINEAR MODEL COEFFICIENTS ===\n")
cat("α₀ (intercept):", round(alpha_0, 4), "\n")
cat("α₁ (slope):", round(alpha_1, 4), "\n")

pi_c_linear <- alpha_1 * (z_bar - z_underbar)
pi_a_linear <- alpha_0 + alpha_1 * z_underbar
pi_n_linear <- 1 - (alpha_0 + alpha_1 * z_bar)

cat("\n=== LINEAR MODEL RESULTS ===\n")
cat("P(Batched | Z = z_bar):", round(alpha_0 + alpha_1 * z_bar, 3), "\n")
cat("P(Batched | Z = z_underbar):", round(alpha_0 + alpha_1 * z_underbar, 3), "\n")
cat("Complier share:", round(pi_c_linear * 100, 1), "%\n")
cat("Always-taker share:", round(pi_a_linear * 100, 1), "%\n")
cat("Never-taker share:", round(pi_n_linear * 100, 1), "%\n")
cat("Sum:", round((pi_c_linear + pi_a_linear + pi_n_linear) * 100, 1), "%\n")



#======================================
#=========================================================================
# STEP 1: First calculate the overall complier share (need this first!)
#=========================================================================

# Function to calculate shares using local linear model
calc_shares_loess <- function(data, cutoff) {
  z_bar <- quantile(data$batch.tendency, 1 - cutoff)
  z_underbar <- quantile(data$batch.tendency, cutoff)
  
  loess_fit <- loess(batched ~ batch.tendency, data = data, span = 0.75)
  
  p_high <- predict(loess_fit, newdata = data.frame(batch.tendency = z_bar))
  p_low <- predict(loess_fit, newdata = data.frame(batch.tendency = z_underbar))
  
  pi_c <- as.numeric(p_high - p_low)
  pi_a <- as.numeric(p_low)
  pi_n <- as.numeric(1 - p_high)
  
  return(data.frame(compliers = pi_c, never_takers = pi_n, always_takers = pi_a))
}

# Function to calculate shares using linear model
calc_shares_linear <- function(data, cutoff) {
  z_bar <- quantile(data$batch.tendency, 1 - cutoff)
  z_underbar <- quantile(data$batch.tendency, cutoff)
  
  lm_fit <- lm(batched ~ batch.tendency, data = data)
  alpha_0 <- coef(lm_fit)["(Intercept)"]
  alpha_1 <- coef(lm_fit)["batch.tendency"]
  
  p_high <- alpha_0 + alpha_1 * z_bar
  p_low <- alpha_0 + alpha_1 * z_underbar
  
  pi_c <- as.numeric(alpha_1 * (z_bar - z_underbar))
  pi_a <- as.numeric(p_low)
  pi_n <- as.numeric(1 - p_high)
  
  return(data.frame(compliers = pi_c, never_takers = pi_n, always_takers = pi_a))
}

# Calculate for different cutoffs
cutoffs <- c(0.01, 0.015, 0.02)

# Local Linear Model results
loess_list <- lapply(cutoffs, function(c) calc_shares_loess(final, c))
loess_results <- do.call(cbind, loess_list)
names(loess_results) <- paste0(rep(c("compliers_", "never_takers_", "always_takers_"), 3),
                               rep(c("1", "1.5", "2"), each = 3))

# Linear Model results  
linear_list <- lapply(cutoffs, function(c) calc_shares_linear(final, c))
linear_results <- do.call(cbind, linear_list)

# Print Table C.1
cat("\n=== TABLE B.1: Sample Share by Compliance Type ===\n\n")
cat("Model Specification:        Local Linear Model              Linear Model\n")
cat("Leniency Cutoff:           1%      1.5%      2%           1%      1.5%      2%\n")
cat("──────────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("Compliers              %5.2f    %5.2f    %5.2f        %5.2f    %5.2f    %5.2f\n",
            loess_list[[1]]$compliers, loess_list[[2]]$compliers, loess_list[[3]]$compliers,
            linear_list[[1]]$compliers, linear_list[[2]]$compliers, linear_list[[3]]$compliers))
cat(sprintf("Never Takers           %5.2f    %5.2f    %5.2f        %5.2f    %5.2f    %5.2f\n",
            loess_list[[1]]$never_takers, loess_list[[2]]$never_takers, loess_list[[3]]$never_takers,
            linear_list[[1]]$never_takers, linear_list[[2]]$never_takers, linear_list[[3]]$never_takers))
cat(sprintf("Always Takers          %5.2f    %5.2f    %5.2f        %5.2f    %5.2f    %5.2f\n",
            loess_list[[1]]$always_takers, loess_list[[2]]$always_takers, loess_list[[3]]$always_takers,
            linear_list[[1]]$always_takers, linear_list[[2]]$always_takers, linear_list[[3]]$always_takers))

# Store the 1% loess results as our main estimates (GLOBAL VARIABLES)
pi_c <- loess_list[[1]]$compliers
pi_n <- loess_list[[1]]$never_takers
pi_a <- loess_list[[1]]$always_takers

cat("\n\nUsing 1% cutoff, Local Linear Model as main estimates:\n")
cat("Complier share (pi_c):", round(pi_c, 3), "\n")
cat("Never-taker share (pi_n):", round(pi_n, 3), "\n")
cat("Always-taker share (pi_a):", round(pi_a, 3), "\n")

#=========================================================================
# STEP 2: Now calculate complier characteristics (pi_c is now defined!)
#=========================================================================

# Function to calculate complier characteristics following Abadie (2003)
calc_complier_char <- function(data, var_name, pi_c_overall, cutoff = 0.01) {
  
  # Overall P(X=x)
  p_x <- mean(data[[var_name]], na.rm = TRUE)
  
  # Calculate complier share for subsample where X=x
  subsample <- data[data[[var_name]] == 1, ]
  
  if (nrow(subsample) < 100) {
    return(c(p_x = p_x, p_x_complier = NA, ratio = NA))
  }
  
  # Get complier share for this subsample
  z_bar <- quantile(data$batch.tendency, 1 - cutoff)
  z_underbar <- quantile(data$batch.tendency, cutoff)
  
  # Use linear model for subsamples (more stable than loess with smaller samples)
  lm_fit_sub <- lm(batched ~ batch.tendency, data = subsample)
  alpha_0_sub <- coef(lm_fit_sub)["(Intercept)"]
  alpha_1_sub <- coef(lm_fit_sub)["batch.tendency"]
  
  p_high_sub <- alpha_0_sub + alpha_1_sub * z_bar
  p_low_sub <- alpha_0_sub + alpha_1_sub * z_underbar
  
  pi_c_given_x <- as.numeric(p_high_sub - p_low_sub)
  
  # Handle edge cases
  if (is.na(pi_c_given_x) | pi_c_given_x < 0) {
    return(c(p_x = p_x, p_x_complier = NA, ratio = NA))
  }
  
  # P(X=x|complier) = [π_{c|x} × P(X=x)] / π_c
  p_x_given_complier <- (pi_c_given_x * p_x) / pi_c_overall
  
  # Ratio
  ratio <- p_x_given_complier / p_x
  
  return(c(p_x = p_x, p_x_complier = p_x_given_complier, ratio = ratio))
}

# Create binary variables for characteristics
final <- final %>%
  mutate(
    # Demographics
    male = ifelse(GENDER == "Male", 1, 0),
    female = ifelse(GENDER == "Female", 1, 0),
    white = ifelse(race == "white", 1, 0),
    non_white = ifelse(race != "white", 1, 0),
    age_under_50 = ifelse(age < 50, 1, 0),
    age_50_plus = ifelse(age >= 50, 1, 0),
    
    # Acuity
    high_esi = ifelse(ESI <= 2, 1, 0),
    low_esi = ifelse(ESI >= 3, 1, 0),
    
    # Vital signs
    abnormal_vitals = ifelse(tachycardic == 1 | tachypneic == 1 | 
                               febrile == 1 | hypotensive == 1, 1, 0),
    normal_vitals = 1 - abnormal_vitals,
    
    # Labs
    labs_ordered = LAB_PERF,
    no_labs = 1 - LAB_PERF
  )

# List of characteristics to analyze
characteristics <- list(
  c("male", "Male"),
  c("female", "Female"),
  c("white", "White"),
  c("non_white", "Non-White"),
  c("age_under_50", "Age < 50"),
  c("age_50_plus", "Age >= 50"),
  c("high_esi", "High Acuity (ESI 1-2)"),
  c("low_esi", "Lower Acuity (ESI 3-5)"),
  c("tachycardic", "Tachycardic"),
  c("abnormal_vitals", "Any Abnormal Vital"),
  c("normal_vitals", "Normal Vitals"),
  c("labs_ordered", "Labs Ordered"),
  c("no_labs", "No Labs Ordered")
)

# Calculate for each characteristic
cat("\n\n=== TABLE B.2: Characteristics of Marginal Patients ===\n")
cat("                              P[X=x]      P[X=x|complier]    Ratio\n")
cat("───────────────────────────────────────────────────────────────────\n")

results_table <- data.frame(
  Characteristic = character(),
  P_X = numeric(),
  P_X_complier = numeric(),
  Ratio = numeric(),
  stringsAsFactors = FALSE
)

for (char in characteristics) {
  var_name <- char[1]
  var_label <- char[2]
  
  # Pass pi_c as argument
  result <- calc_complier_char(final, var_name, pi_c_overall = pi_c)
  
  cat(sprintf("%-28s %6.3f         %6.3f           %5.3f\n",
              var_label, result["p_x"], result["p_x_complier"], result["ratio"]))
  
  results_table <- rbind(results_table, data.frame(
    Characteristic = var_label,
    P_X = result["p_x"],
    P_X_complier = result["p_x_complier"],
    Ratio = result["ratio"]
  ))
}

#=========================================================================
# STEP 3: Add chief complaint analysis (important for your paper!)
#=========================================================================

# Create binary variables for each complaint category
complaint_categories <- unique(final$CHIEF_COMPLAINT)

cat("\n\n=== TABLE B.3: Complier Characteristics by Chief Complaint ===\n")
cat("                                        P[X=x]      P[X=x|complier]    Ratio\n")
cat("─────────────────────────────────────────────────────────────────────────────\n")

for (complaint in complaint_categories) {
  final[[paste0("complaint_", gsub("[^[:alnum:]]", "_", complaint))]] <- 
    ifelse(final$CHIEF_COMPLAINT == complaint, 1, 0)
  
  var_name <- paste0("complaint_", gsub("[^[:alnum:]]", "_", complaint))
  
  result <- calc_complier_char(final, var_name, pi_c_overall = pi_c)
  
  # Truncate complaint name for display
  complaint_short <- substr(complaint, 1, 35)
  
  cat(sprintf("%-38s %6.3f         %6.3f           %5.3f\n",
              complaint_short, result["p_x"], result["p_x_complier"], result["ratio"]))
}




# Approach 1: Restrict to complier-like observable profile

# First, identify which characteristics have high complier ratios
# (Your existing code already computes these — pull from results_table)
# Compliers tend to have: normal vitals, standard ESI, certain complaints

# Define complier-like subsample based on observable profile
complier_like <- final %>%
  mutate(
    complier_like = case_when(
      normal_vitals == 1 & ESI %in% c(2, 3) ~ 1,
      TRUE ~ 0
    )
  )

# Within-physician OLS on complier-like subsample
wp_los_complier <- feols(
  ln_ED_LOS ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  vcov = "HC1", 
  data = complier_like %>% filter(complier_like == 1)
)

wp_admit_complier <- feols(
  admit ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  vcov = "HC1", 
  data = complier_like %>% filter(complier_like == 1)
)

wp_img_complier <- feols(
  imgTests ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  vcov = "HC1", 
  data = complier_like %>% filter(complier_like == 1)
)

wp_downgrade_complier <- feols(
  unstable_admit ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  vcov = "HC1", 
  data = complier_like %>% filter(complier_like == 1)
)

etable(wp_los_complier, wp_admit_complier, wp_img_complier, wp_downgrade_complier,
       keep = "batched")

# Also report sample size
cat("Complier-like sample size:", sum(complier_like$complier_like), "\n")
cat("Full sample size:", nrow(final), "\n")

# Approach 2: Abadie kappa-weighted within-physician OLS
# Following Abadie (2003) and the Chyn-Frandsen-Leslie (2025) JEL implementation

library(fixest)
library(dplyr)

# Step 1: Binarize the instrument at the median
# Z* = 1 if above-median batch tendency, 0 otherwise
final <- final %>%
  mutate(
    Z_star = ifelse(batch.tendency > median(batch.tendency, na.rm = TRUE), 1, 0)
  )

# Step 2: Estimate propensity score P(Z* = 1 | X)
# Use the same covariates as in the main specification
ps_model <- glm(
  Z_star ~ tachycardic + tachypneic + febrile + hypotensive + 
    age + EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
    factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +
    factor(race) + factor(GENDER) + factor(capacity_level) +
    lab.tendency + admit.tendency,
  data = final,
  family = binomial(link = "logit")
)

final$ps <- predict(ps_model, type = "response")

# Trim extreme propensity scores to avoid weight instability
final$ps <- pmin(pmax(final$ps, 0.05), 0.95)

# Step 3: Construct Abadie kappa weights
# kappa_i = 1 - D_i*(1-Z*_i)/(1-PS_i) - (1-D_i)*Z*_i/PS_i
final <- final %>%
  mutate(
    kappa = 1 - 
      (batched * (1 - Z_star)) / (1 - ps) - 
      ((1 - batched) * Z_star) / ps
  )

# Sanity check on weights
cat("\n=== Abadie weights summary ===\n")
cat("Mean kappa:", round(mean(final$kappa, na.rm = TRUE), 3), "\n")
cat("Share negative weights:", round(mean(final$kappa < 0, na.rm = TRUE) * 100, 1), "%\n")
cat("Range of kappa:", round(range(final$kappa, na.rm = TRUE), 3), "\n")

# Note: Some kappa weights will be negative. This is a known feature
# of the Abadie weighting; common practice is to either keep them
# (the IV identification still holds in expectation) or trim them.
# We'll report both.

# Step 4: Complier-weighted within-physician OLS
# Including physician FE + full controls, weighted by kappa

# All weights (including negative)
wp_los_kappa <- feols(
  ln_ED_LOS ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~kappa,
  vcov = "HC1", data = final
)

wp_admit_kappa <- feols(
  admit ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~kappa,
  vcov = "HC1", data = final
)

wp_img_kappa <- feols(
  imgTests ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~kappa,
  vcov = "HC1", data = final
)

wp_downgrade_kappa <- feols(
  unstable_admit ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~kappa,
  vcov = "HC1", data = final
)

etable(wp_los_kappa, wp_admit_kappa, wp_img_kappa, wp_downgrade_kappa,
       keep = "batched")

# Also: trimmed version (drop negative weights)
final_trimmed <- final %>% filter(kappa > 0)

wp_los_kappa_trim <- feols(
  ln_ED_LOS ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~kappa,
  vcov = "HC1", data = final_trimmed
)

wp_admit_kappa_trim <- feols(
  admit ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~kappa,
  vcov = "HC1", data = final_trimmed
)

wp_img_kappa_trim <- feols(
  imgTests ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~kappa,
  vcov = "HC1", data = final_trimmed
)

wp_downgrade_kappa_trim <- feols(
  unstable_admit ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~kappa,
  vcov = "HC1", data = final_trimmed
)

cat("\n=== Kappa-weighted within-physician OLS (trimmed) ===\n")
etable(wp_los_kappa_trim, wp_admit_kappa_trim, wp_img_kappa_trim, wp_downgrade_kappa_trim,
       keep = "batched")

library(dplyr)
library(fixest)

#=========================================================================
# STEP 1: Define cells based on observables driving complier heterogeneity
#=========================================================================
# From your Table B.2/B.3 results, the strongest complier signals are:
# - Normal vitals (ratio 1.13) vs abnormal (ratio 0.79)
# - Labs ordered (ratio 1.13) vs no labs (ratio 0.49)
# - Trauma complaints (ratio 1.66), Neuro (1.24) over-represented
# - Abdominal (0.62), Fevers (0.62) under-represented

# Following Dobbie's footnote 19, we use a small number of mutually exclusive
# subgroups. We'll use vitals × labs × complaint group (high vs. low complier ratio)

final <- final %>%
  mutate(
    complaint_complier_group = case_when(
      CHIEF_COMPLAINT %in% c("Falls, MVA, Assaults, and Trauma",
                             "Neurological Issue") ~ "high",
      TRUE ~ "low"
    ),
    cell = paste(normal_vitals, labs_ordered, complaint_complier_group, sep = "_")
  )

# Check cell sizes
table(final$cell)

#=========================================================================
# STEP 2: For each cell, compute complier share using your existing approach
#=========================================================================

# Function: computes complier share within a subsample using batch.tendency
calc_complier_share_in_cell <- function(data, cutoff = 0.01) {
  if (nrow(data) < 100) return(NA_real_)
  
  z_bar <- quantile(data$batch.tendency, 1 - cutoff, na.rm = TRUE)
  z_underbar <- quantile(data$batch.tendency, cutoff, na.rm = TRUE)
  
  # Use linear model (more stable than loess for subsamples)
  lm_fit <- lm(batched ~ batch.tendency, data = data)
  alpha_0 <- coef(lm_fit)["(Intercept)"]
  alpha_1 <- coef(lm_fit)["batch.tendency"]
  
  p_high <- alpha_0 + alpha_1 * z_bar
  p_low <- alpha_0 + alpha_1 * z_underbar
  
  pi_c <- as.numeric(p_high - p_low)
  
  if (is.na(pi_c) || pi_c < 0) return(NA_real_)
  return(pi_c)
}

# Compute complier share and sample share for each cell
cell_stats <- final %>%
  group_by(cell) %>%
  group_modify(~ tibble(
    n_cell = nrow(.x),
    sample_share = nrow(.x) / nrow(final),
    complier_share = calc_complier_share_in_cell(.x)
  )) %>%
  ungroup()

cat("\n=== Cell-level statistics ===\n")
print(cell_stats)

#=========================================================================
# STEP 3: Compute Bhuller-Dobbie reweighting weights
#=========================================================================
# Weight = (complier share in cell) / (sample share in cell)
# This makes the reweighted sample's cell composition match the complier population

# Overall complier share for normalization
pi_c_overall <- 0.203  # from your existing analysis

cell_stats <- cell_stats %>%
  mutate(
    # Bhuller-Dobbie weight: P(complier | cell) / P(cell)
    # Equivalently: complier share in cell / sample share in cell
    # Following Dobbie footnote 19: "share of compliers relative to share of
    # the estimation sample in each subgroup"
    weight = complier_share / sample_share
  )

cat("\n=== Cell weights ===\n")
print(cell_stats)

# Merge weights back to main data
final <- final %>%
  left_join(cell_stats %>% dplyr::select(cell, complier_weight = weight), by = "cell")

# Check: any missing or weird weights?
summary(final$complier_weight)
cat("Share missing:", round(mean(is.na(final$complier_weight)) * 100, 1), "%\n")
cat("Share <= 0:", round(mean(final$complier_weight <= 0, na.rm = TRUE) * 100, 1), "%\n")

#=========================================================================
# STEP 4: Run complier-weighted OLS with physician fixed effects
#=========================================================================
# This is the move R2 wants:
# - Physician FE in the outcome equation
# - OLS on realized batching
# - Reweighted to make the sample reflect the complier population

# Filter to non-missing, positive weights
final_weighted <- final %>% filter(!is.na(complier_weight), complier_weight > 0)

cat("\nSample for weighted analysis:", nrow(final_weighted), "of", nrow(final), "\n")

# Outcome 1: Log LOS
cw_los_fe <- feols(
  ln_ED_LOS ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~complier_weight,
  vcov = "HC1",
  data = final_weighted
)

# Outcome 2: Admission
cw_admit_fe <- feols(
  admit ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~complier_weight,
  vcov = "HC1",
  data = final_weighted
)

# Outcome 3: Imaging tests
cw_img_fe <- feols(
  imgTests ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~complier_weight,
  vcov = "HC1",
  data = final_weighted
)

# Outcome 4: Unstable admission
cw_unstable_fe <- feols(
  unstable_admit ~ batched + tachycardic + tachypneic + febrile + hypotensive +
    age + hrs_in_shift + capacity_level  |
    ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER,
  weights = ~complier_weight,
  vcov = "HC1",
  data = final_weighted
)

cat("\n=== Complier-Weighted Within-Physician OLS ===\n")
etable(cw_los_fe, cw_admit_fe, cw_img_fe, cw_unstable_fe, keep = "%batched")

