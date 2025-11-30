library(ManyIV)
library(fixest)

run_models_with_ujive <- function(data, y_var) {
  
  # --- 1. Summary stats for sequenced (batched == 0) ---
  mean_batched_0 <- mean(data[[y_var]][data$batched == 0], na.rm = TRUE)
  sd_batched_0   <- sd(data[[y_var]][data$batched == 0], na.rm = TRUE)
  
  # --- 2. OLS without controls (ROBUST SEs) ---
  model_ols_no <- feols(
    as.formula(paste(y_var, "~ batched | dayofweekt + month_of_year")),
    # NO cluster argument = heteroskedasticity-robust SEs
    data = data
  )
  
  # --- 3. OLS with controls (ROBUST SEs) ---
  model_ols_yes <- feols(
    as.formula(paste(
      y_var,
      "~ batched + tachycardic + tachypneic + hrs_in_shift + febrile + hypotensive + age + EXPERIENCE + LAB_PERF |
       dayofweekt + month_of_year + race + complaint_esi + GENDER + PROVIDER_SEX + capacity_level"
    )),
    # NO cluster argument = heteroskedasticity-robust SEs
    data = data
  )
  
  # --- 4. 2SLS without controls (ROBUST SEs) ---
  model_2sls_no <- feols(
    as.formula(paste(y_var, "~ 0 | dayofweekt + month_of_year | batched ~ batch.tendency")),
    # NO cluster argument = heteroskedasticity-robust SEs
    data = data
  )
  
  # --- 5. 2SLS with controls (ROBUST SEs) ---
  model_2sls_yes <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + EXPERIENCE + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX + capacity_level |
       batched ~ batch.tendency"
    )),
    # NO cluster argument = heteroskedasticity-robust SEs
    data = data
  )
  
  # --- 6. UJIVE preparation - create physician dummies ---
  # (Only if not already created)
  if(!"phys_1" %in% colnames(data)) {
    physicians <- sort(unique(data$ED_PROVIDER))
    physicians <- physicians[-1]  # Remove one for reference
    
    for(p in physicians) {
      data[[paste0("phys_", which(physicians == p))]] <- ifelse(data$ED_PROVIDER == p, 1, 0)
    }
  }
  
  instrument_vars <- paste0("phys_", 1:(length(unique(data$ED_PROVIDER))-1))
  
  # --- 7. UJIVE without controls (ROBUST SEs by default) ---
  ujive_formula_no <- as.formula(paste(
    y_var, "~",
    "batched + factor(dayofweekt) + factor(month_of_year) |",
    paste(instrument_vars, collapse = " + "), "+",
    "factor(dayofweekt) + factor(month_of_year)"
  ))
  
  ujive_no <- ujive(formula = ujive_formula_no, data = data)
  
  # --- 8. UJIVE with controls (ROBUST SEs by default) ---
  control_vars <- c("tachycardic", "tachypneic", "febrile", "hypotensive",
                    "age", "EXPERIENCE", "hrs_in_shift", "LAB_PERF")
  
  ujive_formula_yes <- as.formula(paste(
    y_var, "~",
    "batched +", paste(control_vars, collapse = " + "), "+",
    "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
    "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level) |",
    paste(instrument_vars, collapse = " + "), "+",
    paste(control_vars, collapse = " + "), "+",
    "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
    "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level)"
  ))
  
  ujive_yes <- ujive(formula = ujive_formula_yes, data = data)
  
  # --- 9. Extract UJIVE results ---
  ujive_no_coef <- ujive_no$estimate["ujive", "estimate"]
  ujive_no_se <- ujive_no$estimate["ujive", "se_hte"]
  
  ujive_yes_coef <- ujive_yes$estimate["ujive", "estimate"]
  ujive_yes_se <- ujive_yes$estimate["ujive", "se_hte"]
  
  # --- 10. Collect all results ---
  extract_coef <- function(model, term) ifelse(term %in% names(coef(model)), coef(model)[term], NA)
  extract_se   <- function(model, term) ifelse(term %in% names(se(model)),   se(model)[term],   NA)
  
  results <- data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)",
              "OLS No Controls", "OLS With Controls",
              "2SLS No Controls", "2SLS With Controls",
              "UJIVE No Controls", "UJIVE With Controls"),
    Coefficient = c(
      mean_batched_0,
      sd_batched_0,
      extract_coef(model_ols_no, "batched"),
      extract_coef(model_ols_yes, "batched"),
      extract_coef(model_2sls_no, "fit_batched"),
      extract_coef(model_2sls_yes, "fit_batched"),
      ujive_no_coef,
      ujive_yes_coef
    ),
    SE = c(
      NA, NA,
      extract_se(model_ols_no, "batched"),
      extract_se(model_ols_yes, "batched"),
      extract_se(model_2sls_no, "fit_batched"),
      extract_se(model_2sls_yes, "fit_batched"),
      ujive_no_se,
      ujive_yes_se
    )
  )
  
  cat("\n=== RESULTS (ALL USING HETEROSKEDASTICITY-ROBUST SEs) ===\n")
  print(results)
  
  # Return the data with physician dummies for next call
  return(data)
}

# Run for all outcomes
final <- run_models_with_ujive(final, "ln_ED_LOS")
final <- run_models_with_ujive(final, "ln_disp_time")
final <- run_models_with_ujive(final, "ln_treat_time")
final <- run_models_with_ujive(final, "imgTests")
final <- run_models_with_ujive(final, "RTN_72_HR_ADMIT")
final <- run_models_with_ujive(final, "RTN_72_HR")
final <- run_models_with_ujive(final, "PLAIN_XRAY")
final <- run_models_with_ujive(final, "US_PERF")
final <- run_models_with_ujive(final, "NON_CON_CT_PERF")
final <- run_models_with_ujive(final, "CON_CT_PERF")
final <- run_models_with_ujive(final, "admit")


####
sensitivity_analysis_ujive <- function(data, y_var, delta_range = seq(-0.2, 0.2, by = 0.02)) {
  
  # Create physician dummies if not already done
  if(!"phys_1" %in% colnames(data)) {
    physicians <- sort(unique(data$ED_PROVIDER))
    physicians <- physicians[-1]
    for(p in physicians) {
      data[[paste0("phys_", which(physicians == p))]] <- ifelse(data$ED_PROVIDER == p, 1, 0)
    }
  }
  
  results <- data.frame(
    delta = numeric(),
    coef = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric()
  )
  
  instrument_vars <- paste0("phys_", 1:(length(unique(data$ED_PROVIDER))-1))
  control_vars <- c("tachycardic", "tachypneic", "febrile", "hypotensive",
                    "age", "EXPERIENCE", "hrs_in_shift", "LAB_PERF")
  
  for (delta in delta_range) {
    # Adjust outcome by delta * average physician effect
    # This is trickier with multiple instruments - use average exposure
    physician_exposure <- rowSums(data[, instrument_vars]) 
    data$y_adjusted <- data[[y_var]] - delta * physician_exposure
    
    # Run UJIVE on adjusted outcome
    ujive_formula <- as.formula(paste(
      "y_adjusted ~",
      "batched +", paste(control_vars, collapse = " + "), "+",
      "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
      "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level) |",
      paste(instrument_vars, collapse = " + "), "+",
      paste(control_vars, collapse = " + "), "+",
      "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
      "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level)"
    ))
    
    ujive_result <- ujive(formula = ujive_formula, data = data)
    
    coef_val <- ujive_result$estimate["ujive", "estimate"]
    se_val <- ujive_result$estimate["ujive", "se_hte"]
    
    results <- rbind(results, data.frame(
      delta = delta,
      coef = coef_val,
      ci_lower = coef_val - 1.96 * se_val,
      ci_upper = coef_val + 1.96 * se_val
    ))
  }
  
  # Plot results...
  return(results)
}

# plot the results
sensitivity_results <- sensitivity_analysis_ujive(final, "ln_ED_LOS")
ggplot(sensitivity_results, aes(x = delta, y = coef)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
  labs(title = "Sensitivity Analysis of UJIVE Estimates",
       x = "Delta (Unobserved Confounding Adjustment)",
       y = "UJIVE Coefficient Estimate") +
  theme_minimal()

###
# MEDIATION ANALYSIS FOR EXCLUSION RESTRICTION SUPPORT
library(lavaan)
library(fixest)

# Step 1: Get predicted batching from first stage
fs <- feols(batched ~ batch.tendency +  
              tachycardic + tachypneic + febrile + hypotensive + 
              age + EXPERIENCE + hrs_in_shift + LAB_PERF + capacity_level |
              dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX,
            cluster = ~ED_PROVIDER, data = final)

final$batched_pred <- predict(fs, newdata = final)

# Step 2: Residualize key variables to handle fixed effects
final$res_imgTests <- resid(feols(imgTests ~ dayofweekt + month_of_year + complaint_esi, data = final))
final$res_admit <- resid(feols(admit ~ dayofweekt + month_of_year + complaint_esi, data = final))  
final$res_ln_ED_LOS <- resid(feols(ln_ED_LOS ~ dayofweekt + month_of_year + complaint_esi, data = final))
final$res_batched <- resid(feols(batched_pred ~ dayofweekt + month_of_year + complaint_esi, data = final))

# Step 3: SEM for LOS with both mediators
sem_model_los <- '
  # Path 1: Batching → Imaging Tests
  res_imgTests ~ a1 * res_batched
  
  # Path 2: Batching → Admission (both direct and through imaging)
  res_admit ~ a2 * res_batched + a3 * res_imgTests
  
  # Outcome: Effects on LOS
  res_ln_ED_LOS ~ b1 * res_imgTests + b2 * res_admit + c_direct * res_batched
  
  # Indirect effects
  indirect_imaging := a1 * b1
  indirect_admission_direct := a2 * b2  
  indirect_admission_via_imaging := a1 * a3 * b2
  total_indirect := indirect_imaging + indirect_admission_direct + indirect_admission_via_imaging
  
  # Total effect
  total_effect := c_direct + total_indirect
  
  # Proportion mediated
  prop_mediated := total_indirect / total_effect
'

fit_los <- sem(sem_model_los, data = final)
summary(fit_los, standardized = FALSE, fit.measures = TRUE)

# Step 4: Simpler model for time to disposition (no admission mediator)
sem_model_disp <- '
  # Single mediator: Imaging Tests
  res_imgTests ~ a * res_batched
  
  # Outcome: Time to disposition
  res_ln_disp_time ~ b * res_imgTests + c_direct * res_batched
  
  # Effects
  indirect_effect := a * b
  direct_effect := c_direct
  total_effect := indirect_effect + c_direct
  prop_mediated := indirect_effect / total_effect
'

final$res_ln_disp_time <- resid(feols(ln_disp_time ~ dayofweekt + month_of_year + complaint_esi, data = final))
fit_disp <- sem(sem_model_disp, data = final)
summary(fit_disp, standardized = FALSE, fit.measures = TRUE)





library(lavaan)
library(fixest)

# Step 1: Residualize all variables to handle fixed effects
# (This removes the fixed effects so SEM can work with clean variation)

final$res_batched <- resid(feols(batched ~ dayofweekt + month_of_year + complaint_esi, data = final))
final$res_imgTests <- resid(feols(imgTests ~ dayofweekt + month_of_year + complaint_esi, data = final))
final$res_admit <- resid(feols(admit ~ dayofweekt + month_of_year + complaint_esi, data = final))
final$res_ln_ED_LOS <- resid(feols(ln_ED_LOS ~ dayofweekt + month_of_year + complaint_esi, data = final))
final$res_ln_disp_time <- resid(feols(ln_disp_time ~ dayofweekt + month_of_year + complaint_esi, data = final))

# Step 2: Add control variables (don't residualize these)
# We'll include them directly in the SEM model

# Model 1: LOS with both mediators
sem_model_los <- '
  # Mediator 1: Imaging tests increase with batching
  res_imgTests ~ a1 * res_batched + tachycardic + tachypneic + febrile + hypotensive + age
  
  # Mediator 2: Admission (can be affected by both batching and imaging)  
  res_admit ~ a2 * res_batched + a3 * res_imgTests + tachycardic + tachypneic + febrile + hypotensive + age
  
  # Outcome: LOS affected by imaging, admission, and potentially direct batching effect
  res_ln_ED_LOS ~ b1 * res_imgTests + b2 * res_admit + c_prime * res_batched + 
                   tachycardic + tachypneic + febrile + hypotensive + age
  
  # Define indirect effects
  indirect_via_imaging := a1 * b1
  indirect_via_admission_direct := a2 * b2  
  indirect_via_imaging_to_admission := a1 * a3 * b2
  
  # Total effects
  total_indirect := indirect_via_imaging + indirect_via_admission_direct + indirect_via_imaging_to_admission
  direct_effect := c_prime
  total_effect := total_indirect + c_prime
  
  # Proportion mediated
  prop_mediated := total_indirect / total_effect
  prop_direct := c_prime / total_effect
'

fit_los <- sem(sem_model_los, data = final, missing = "ML")
summary(fit_los, standardized = FALSE, fit.measures = TRUE)

# Model 2: Time to disposition (simpler - no admission mediator since disposition comes first)
sem_model_disp <- '
  # Mediator: More imaging with batching
  res_imgTests ~ a * res_batched + tachycardic + tachypneic + febrile + hypotensive + age
  
  # Outcome: Time to disposition
  res_ln_disp_time ~ b * res_imgTests + c_prime * res_batched + 
                      tachycardic + tachypneic + febrile + hypotensive + age
  
  # Effects
  indirect_effect := a * b
  direct_effect := c_prime  
  total_effect := a * b + c_prime
  prop_mediated := indirect_effect / total_effect
'

fit_disp <- sem(sem_model_disp, data = final, missing = "ML")
summary(fit_disp, standardized = FALSE, fit.measures = TRUE)


# UJIVE without controlling for imaging
ujive1 <- ujive(ln_ED_LOS ~ batched + controls | physician_dummies + controls)

# UJIVE controlling for imaging  
ujive2 <- ujive(ln_ED_LOS ~ batched + imgTests + controls | physician_dummies + controls)

# Compare coefficients



library(ggplot2)
library(dplyr)
library(tidyr)
library(ManyIV)

sensitivity_analysis_ujive <- function(data, y_var, delta_range = seq(-1, 1, by = 0.02)) {
  
  # Create physician dummies if not already done
  if(!"phys_1" %in% colnames(data)) {
    physicians <- sort(unique(data$ED_PROVIDER))
    physicians <- physicians[-1]  # Remove one for reference
    for(p in physicians) {
      data[[paste0("phys_", which(physicians == p))]] <- ifelse(data$ED_PROVIDER == p, 1, 0)
    }
  }
  
  results <- data.frame(
    delta = numeric(),
    coef = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric()
  )
  
  instrument_vars <- paste0("phys_", 1:(length(unique(data$ED_PROVIDER))-1))
  control_vars <- c("tachycardic", "tachypneic", "febrile", "hypotensive",
                    "age", "EXPERIENCE", "hrs_in_shift", "LAB_PERF")
  
  for (delta in delta_range) {
    # Adjust outcome by delta * batch.tendency (our interpretable measure)
    # This tests: what if batch tendency has direct effect delta on outcome?
    data$y_adjusted <- data[[y_var]] - delta * data$batch.tendency
    
    # Run UJIVE on adjusted outcome using physician dummies as instruments
    ujive_formula <- as.formula(paste(
      "y_adjusted ~",
      "batched +", paste(control_vars, collapse = " + "), "+",
      "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
      "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level) |",
      paste(instrument_vars, collapse = " + "), "+",  # Physician dummies as instruments
      paste(control_vars, collapse = " + "), "+",
      "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
      "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level)"
    ))
    
    ujive_result <- ujive(formula = ujive_formula, data = data)
    
    coef_val <- ujive_result$estimate["ujive", "estimate"]
    se_val <- ujive_result$estimate["ujive", "se_hte"]  # heteroskedasticity-robust SE
    
    results <- rbind(results, data.frame(
      delta = delta,
      coef = coef_val,
      ci_lower = coef_val - 1.96 * se_val,
      ci_upper = coef_val + 1.96 * se_val
    ))
  }
  
  return(results)
}

# Run for multiple outcomes
outcomes <- list(
  "Log ED LOS" = "ln_ED_LOS",
  "Log Time to Disposition" = "ln_disp_time", 
  "Number of Imaging Tests" = "imgTests",
  "72hr Return with Admission" = "RTN_72_HR_ADMIT"
)

# Collect all results
all_results <- data.frame()

for(outcome_name in names(outcomes)) {
  y_var <- outcomes[[outcome_name]]
  
  cat("Processing", outcome_name, "...\n")
  
  # Get baseline UJIVE estimate (delta = 0) for reference
  if(!"phys_1" %in% colnames(final)) {
    physicians <- sort(unique(final$ED_PROVIDER))
    physicians <- physicians[-1]
    for(p in physicians) {
      final[[paste0("phys_", which(physicians == p))]] <- ifelse(final$ED_PROVIDER == p, 1, 0)
    }
  }
  
  instrument_vars <- paste0("phys_", 1:(length(unique(final$ED_PROVIDER))-1))
  control_vars <- c("tachycardic", "tachypneic", "febrile", "hypotensive",
                    "age", "EXPERIENCE", "hrs_in_shift", "LAB_PERF")
  
  baseline_formula <- as.formula(paste(
    y_var, "~",
    "batched +", paste(control_vars, collapse = " + "), "+",
    "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
    "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level) |",
    paste(instrument_vars, collapse = " + "), "+",
    paste(control_vars, collapse = " + "), "+",
    "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
    "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level)"
  ))
  
  baseline_ujive <- ujive(formula = baseline_formula, data = final)
  baseline_coef <- baseline_ujive$estimate["ujive", "estimate"]
  
  # Run sensitivity analysis
  sens_results <- sensitivity_analysis_ujive(final, y_var, delta_range = seq(-0.2, 0.2, by = 0.02))
  sens_results$outcome <- outcome_name
  sens_results$baseline <- baseline_coef
  
  all_results <- rbind(all_results, sens_results)
}

# Create professional multi-panel plot
p <- all_results %>%
  ggplot(aes(x = delta)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = coef), color = "blue", size = 1.2) +
  geom_point(data = all_results %>% filter(abs(delta) < 0.001), 
             aes(y = baseline), color = "black", size = 3, shape = 19) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50", alpha = 0.7) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  labs(
    #title = "Sensitivity of UJIVE Estimates to Exclusion Restriction Violations",
    x = expression(delta ~ ": Direct effect of batch tendency on outcome"),
    y = "UJIVE treatment effect estimate"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey30"),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 8.5, color = "grey50", lineheight = 1.2)
  )

print(p)

# Create summary table at key delta values
robustness_summary <- all_results %>%
  filter(delta %in% c(-0.1, -0.05, 0, 0.05, 0.1)) %>%
  mutate(
    estimate = paste0(sprintf("%.3f", coef), 
                      ifelse(ci_lower > 0, "***", 
                             ifelse(ci_upper < 0, "†", "")))
  ) %>%
  select(outcome, delta, estimate) %>%
  pivot_wider(names_from = delta, values_from = estimate,
              names_prefix = "δ=")

cat("\n=== Robustness of UJIVE Estimates at Key Delta Values ===\n")
print(as.data.frame(robustness_summary), row.names = FALSE)
cat("*** Significant positive effect; † Significant negative effect\n")

# Find critical thresholds
thresholds <- all_results %>%
  group_by(outcome) %>%
  summarize(
    loses_significance_at = {
      tmp <- delta[which(ci_lower <= 0 & ci_upper >= 0)]
      ifelse(length(tmp) > 0, min(abs(tmp)), NA)
    },
    becomes_negative_at = {
      tmp <- delta[which(coef < 0)]
      ifelse(length(tmp) > 0, min(tmp[tmp > 0], na.rm = TRUE), NA)
    }
  )

cat("\n=== Critical Thresholds for UJIVE Estimates ===\n")
print(as.data.frame(thresholds), row.names = FALSE)

# Save plot
ggsave("conley_sensitivity_ujive.png", p, width = 8, height = 5, dpi = 300, bg='white')





library(ManyIV)
library(fixest)
library(sandwich)
library(lmtest)

# Function to compute clustered SEs for UJIVE
compute_clustered_se_ujive <- function(data, ujive_result, cluster_var = "ED_PROVIDER") {
  # Extract residuals and fitted values
  resids <- ujive_result$resid
  n <- length(resids)
  k <- length(ujive_result$estimate["ujive", "estimate"])
  
  # Get unique clusters
  clusters <- data[[cluster_var]]
  n_clusters <- length(unique(clusters))
  
  # Cluster-robust SE (simplified - may need adjustment for your specific case)
  # This is a rough approximation - for production use, consider bootstrap
  se_clustered <- ujive_result$estimate["ujive", "se_hte"] * sqrt(n_clusters/(n_clusters - 1))
  
  return(se_clustered)
}

# Better approach: Bootstrap clustered SEs
bootstrap_clustered_se <- function(data, formula, cluster_var = "ED_PROVIDER", n_boot = 100) {
  clusters <- unique(data[[cluster_var]])
  n_clusters <- length(clusters)
  
  boot_estimates <- numeric(n_boot)
  
  for(i in 1:n_boot) {
    # Sample clusters with replacement
    sampled_clusters <- sample(clusters, n_clusters, replace = TRUE)
    
    # Create bootstrap sample
    boot_data <- do.call(rbind, lapply(sampled_clusters, function(c) {
      data[data[[cluster_var]] == c, ]
    }))
    
    # Run UJIVE on bootstrap sample
    tryCatch({
      ujive_boot <- ujive(formula = formula, data = boot_data)
      boot_estimates[i] <- ujive_boot$estimate["ujive", "estimate"]
    }, error = function(e) {
      boot_estimates[i] <- NA
    })
  }
  
  # Return bootstrap SE
  return(sd(boot_estimates, na.rm = TRUE))
}

# Modified function to show both SE types
compare_ujive_ses <- function(data, y_var) {
  
  # Create physician dummies if not already done
  if(!"phys_1" %in% colnames(data)) {
    physicians <- sort(unique(data$ED_PROVIDER))
    physicians <- physicians[-1]
    for(p in physicians) {
      data[[paste0("phys_", which(physicians == p))]] <- ifelse(data$ED_PROVIDER == p, 1, 0)
    }
  }
  
  instrument_vars <- paste0("phys_", 1:(length(unique(data$ED_PROVIDER))-1))
  control_vars <- c("tachycardic", "tachypneic", "febrile", "hypotensive",
                    "age", "EXPERIENCE", "hrs_in_shift", "LAB_PERF")
  
  # Full specification formula
  ujive_formula <- as.formula(paste(
    y_var, "~",
    "batched +", paste(control_vars, collapse = " + "), "+",
    "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
    "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level) |",
    paste(instrument_vars, collapse = " + "), "+",
    paste(control_vars, collapse = " + "), "+",
    "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
    "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level)"
  ))
  
  # Run UJIVE
  ujive_result <- ujive(formula = ujive_formula, data = data)
  
  # Extract different SE types
  coef_ujive <- ujive_result$estimate["ujive", "estimate"]
  se_robust <- ujive_result$estimate["ujive", "se_hte"]  # Heteroskedasticity-robust
  se_homo <- ujive_result$estimate["ujive", "se_homo"]   # Homoskedastic
  
  # Bootstrap clustered SE (this will take time)
  cat("Computing bootstrap clustered SE (this may take a minute)...\n")
  se_clustered_boot <- bootstrap_clustered_se(data, ujive_formula, "ED_PROVIDER", n_boot = 100)
  
  # Create comparison table
  results <- data.frame(
    Outcome = y_var,
    Coefficient = coef_ujive,
    SE_Homoskedastic = se_homo,
    SE_Robust = se_robust,
    SE_Clustered_Bootstrap = se_clustered_boot,
    CI95_Lower_Homo = coef_ujive - 1.96 * se_homo,
    CI95_Upper_Homo = coef_ujive + 1.96 * se_homo,
    CI95_Lower_Robust = coef_ujive - 1.96 * se_robust,
    CI95_Upper_Robust = coef_ujive + 1.96 * se_robust,
    CI95_Lower_Clustered = coef_ujive - 1.96 * se_clustered_boot,
    CI95_Upper_Clustered = coef_ujive + 1.96 * se_clustered_boot
  )
  
  return(results)
}

# Run comparison for your main outcomes
outcomes_to_test <- c("ln_ED_LOS", "ln_disp_time", "imgTests")

all_comparisons <- data.frame()
for(outcome in outcomes_to_test) {
  cat("\nProcessing", outcome, "...\n")
  result <- compare_ujive_ses(final, outcome)
  all_comparisons <- rbind(all_comparisons, result)
}

# Display results
print(all_comparisons)

# Create a formatted comparison
cat("\n=== Standard Error Comparison for UJIVE Estimates ===\n")
for(i in 1:nrow(all_comparisons)) {
  cat("\n", all_comparisons$Outcome[i], ":\n")
  cat("  Coefficient: ", sprintf("%.3f", all_comparisons$Coefficient[i]), "\n")
  cat("  SE (Homoskedastic):  ", sprintf("%.3f", all_comparisons$SE_Homoskedastic[i]), "\n")
  cat("  SE (Robust):         ", sprintf("%.3f", all_comparisons$SE_Robust[i]), "\n")
  cat("  SE (Clustered):      ", sprintf("%.3f", all_comparisons$SE_Clustered_Bootstrap[i]), "\n")
  cat("  Ratio Clustered/Robust: ", sprintf("%.2f", all_comparisons$SE_Clustered_Bootstrap[i]/all_comparisons$SE_Robust[i]), "\n")
}










library(fixest)
library(dplyr)
library(lme4)

# Test for correlation within physician across shifts
test_within_physician_correlation <- function(data) {
  
  # First, run your main model and get residuals
  main_model <- feols(
    ln_ED_LOS ~ batched + tachycardic + tachypneic + febrile + hypotensive + 
      age + EXPERIENCE + hrs_in_shift + LAB_PERF | 
      dayofweekt + month_of_year + complaint_esi + race + GENDER + 
      PROVIDER_SEX + capacity_level,
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # Add residuals to data
  data$resid <- residuals(main_model)
  
  # Create physician-shift identifier
  data$physician_shift <- paste(data$ED_PROVIDER, 
                                data$dayofweekt,
                                data$hour_of_day,
                                sep = "_")
  
  # --- Test 1: ICC (Intraclass Correlation) ---
  cat("\n=== Test 1: Intraclass Correlation Coefficient ===\n")
  
  # ICC at physician level
  icc_physician <- lmer(resid ~ 1 + (1|ED_PROVIDER), data = data)
  icc_phys_value <- VarCorr(icc_physician)$ED_PROVIDER[1] / 
    (VarCorr(icc_physician)$ED_PROVIDER[1] + attr(VarCorr(icc_physician), "sc")^2)
  
  # ICC at physician-shift level  
  icc_shift <- lmer(resid ~ 1 + (1|physician_shift), data = data)
  icc_shift_value <- VarCorr(icc_shift)$physician_shift[1] / 
    (VarCorr(icc_shift)$physician_shift[1] + attr(VarCorr(icc_shift), "sc")^2)
  
  cat("ICC at physician level: ", round(icc_phys_value, 4), "\n")
  cat("ICC at physician-shift level: ", round(icc_shift_value, 4), "\n")
  cat("Interpretation: Values close to 0 suggest little clustering\n\n")
  
  # --- Test 2: Correlation Matrix Approach ---
  cat("=== Test 2: Average Pairwise Correlations ===\n")
  
  # For each physician, calculate average correlation between different shifts
  physicians <- unique(data$ED_PROVIDER)
  within_physician_cors <- c()
  
  for(p in physicians[1:min(10, length(physicians))]) {  # Sample for speed
    physician_data <- data[data$ED_PROVIDER == p, ]
    if(nrow(physician_data) > 20) {  # Need enough data
      shifts <- unique(physician_data$physician_shift)
      if(length(shifts) > 1) {
        # Get correlations between different shifts for same physician
        shift_pairs <- combn(shifts, 2, simplify = FALSE)
        for(pair in shift_pairs[1:min(5, length(shift_pairs))]) {
          shift1_resids <- physician_data$resid[physician_data$physician_shift == pair[1]]
          shift2_resids <- physician_data$resid[physician_data$physician_shift == pair[2]]
          if(length(shift1_resids) > 2 && length(shift2_resids) > 2) {
            # Sample same number from each shift
            n_sample <- min(length(shift1_resids), length(shift2_resids))
            cor_val <- cor(sample(shift1_resids, n_sample), 
                           sample(shift2_resids, n_sample), 
                           use = "complete.obs")
            if(!is.na(cor_val)) within_physician_cors <- c(within_physician_cors, cor_val)
          }
        }
      }
    }
  }
  
  # Between physician correlations (random pairs)
  between_physician_cors <- c()
  for(i in 1:100) {  # 100 random comparisons
    p1 <- sample(physicians, 1)
    p2 <- sample(physicians[physicians != p1], 1)
    p1_resids <- sample(data$resid[data$ED_PROVIDER == p1], 
                        min(20, sum(data$ED_PROVIDER == p1)))
    p2_resids <- sample(data$resid[data$ED_PROVIDER == p2], 
                        min(20, sum(data$ED_PROVIDER == p2)))
    cor_val <- cor(p1_resids, p2_resids, use = "complete.obs")
    if(!is.na(cor_val)) between_physician_cors <- c(between_physician_cors, cor_val)
  }
  
  cat("Mean correlation within physician (across shifts): ", 
      round(mean(within_physician_cors, na.rm = TRUE), 4), "\n")
  cat("Mean correlation between physicians: ", 
      round(mean(between_physician_cors, na.rm = TRUE), 4), "\n")
  cat("Difference: ", 
      round(mean(within_physician_cors, na.rm = TRUE) - 
              mean(between_physician_cors, na.rm = TRUE), 4), "\n\n")
  
  # --- Test 3: Variance Decomposition ---
  cat("=== Test 3: Variance Decomposition ===\n")
  
  var_decomp <- data %>%
    group_by(ED_PROVIDER) %>%
    summarise(
      within_physician_var = var(resid, na.rm = TRUE),
      n_patients = n()
    ) %>%
    filter(n_patients > 50)  # Only physicians with enough data
  
  total_var <- var(data$resid, na.rm = TRUE)
  between_physician_var <- var(var_decomp$within_physician_var, na.rm = TRUE)
  
  cat("Total residual variance: ", round(total_var, 4), "\n")
  cat("Average within-physician variance: ", round(mean(var_decomp$within_physician_var), 4), "\n")
  cat("Variance explained by physician: ", 
      round(between_physician_var / total_var * 100, 2), "%\n\n")
  
  # --- Test 4: Statistical Test ---
  cat("=== Test 4: Likelihood Ratio Test for Physician Random Effects ===\n")
  
  # Null model: no physician effects
  null_model <- lm(resid ~ 1, data = data)
  
  # Alternative: physician random effects
  alt_model <- lmer(resid ~ 1 + (1|ED_PROVIDER), data = data, REML = FALSE)
  
  # LR test
  lr_test <- anova(null_model, alt_model)
  cat("LR test p-value: ", lr_test$`Pr(>Chisq)`[2], "\n")
  cat("Significant p-value suggests physician-level clustering needed\n\n")
  
  # --- Summary Recommendation ---
  cat("=== SUMMARY ===\n")
  if(icc_phys_value < 0.01) {
    cat("✓ Very low ICC (<0.01): Physician clustering likely unnecessary\n")
  } else if(icc_phys_value < 0.05) {
    cat("⚠ Low ICC (0.01-0.05): Consider showing both clustered and unclustered\n")
  } else {
    cat("✗ Moderate/High ICC (>0.05): Physician clustering recommended\n")
  }
  
  return(list(
    icc_physician = icc_phys_value,
    icc_shift = icc_shift_value,
    within_cors = within_physician_cors,
    between_cors = between_physician_cors
  ))
}

# Run the test
results <- test_within_physician_correlation(final)

# Visualize if you want
library(ggplot2)
cor_data <- data.frame(
  Correlation = c(results$within_cors, results$between_cors),
  Type = c(rep("Within Physician\n(Across Shifts)", length(results$within_cors)),
           rep("Between Physicians", length(results$between_cors)))
)

p <- ggplot(cor_data, aes(x = Type, y = Correlation)) +
  geom_boxplot(aes(fill = Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Test for Within-Physician Correlation Across Shifts",
       subtitle = "If within-physician correlation > between-physician, need physician clustering",
       y = "Residual Correlation",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none")

print(p)



library(kableExtra)
library(dplyr)

# Function to create comparison table
create_comparison_table <- function(results_list) {
  
  # Extract results
  outcomes <- names(results_list)
  
  comparison_data <- data.frame(
    Outcome = character(),
    Mean_Seq = numeric(),
    OLS = character(),
    `2SLS_No_Ctrl` = character(),
    `2SLS_With_Ctrl` = character(),
    UJIVE_No_Ctrl = character(),
    UJIVE_With_Ctrl = character(),
    stringsAsFactors = FALSE
  )
  
  for(outcome in outcomes) {
    res <- results_list[[outcome]]
    
    # Helper function to format coef (SE)
    fmt <- function(coef, se) {
      if(is.na(se)) return(sprintf("%.3f", coef))
      sprintf("%.3f\n(%.3f)", coef, se)
    }
    
    # Extract values
    mean_seq <- res$Coefficient[res$Model == "Mean (Sequenced)"]
    ols_coef <- res$Coefficient[res$Model == "OLS With Controls"]
    ols_se <- res$SE[res$Model == "OLS With Controls"]
    
    tsls_no_coef <- res$Coefficient[res$Model == "2SLS No Controls"]
    tsls_no_se <- res$SE[res$Model == "2SLS No Controls"]
    
    tsls_yes_coef <- res$Coefficient[res$Model == "2SLS With Controls"]
    tsls_yes_se <- res$SE[res$Model == "2SLS With Controls"]
    
    ujive_no_coef <- res$Coefficient[res$Model == "UJIVE No Controls"]
    ujive_no_se <- res$SE[res$Model == "UJIVE No Controls"]
    
    ujive_yes_coef <- res$Coefficient[res$Model == "UJIVE With Controls"]
    ujive_yes_se <- res$SE[res$Model == "UJIVE With Controls"]
    
    comparison_data <- rbind(comparison_data, data.frame(
      Outcome = outcome,
      Mean_Seq = sprintf("%.3f", mean_seq),
      OLS = fmt(ols_coef, ols_se),
      `2SLS_No_Ctrl` = fmt(tsls_no_coef, tsls_no_se),
      `2SLS_With_Ctrl` = fmt(tsls_yes_coef, tsls_yes_se),
      UJIVE_No_Ctrl = fmt(ujive_no_coef, ujive_no_se),
      UJIVE_With_Ctrl = fmt(ujive_yes_coef, ujive_yes_se),
      stringsAsFactors = FALSE
    ))
  }
  
  return(comparison_data)
}

# Store all results
results_list <- list(
  "Log ED Length of Stay" = data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)", "OLS No Controls", "OLS With Controls",
              "2SLS No Controls", "2SLS With Controls", "UJIVE No Controls", "UJIVE With Controls"),
    Coefficient = c(5.48994495, 0.45588094, 0.10527236, 0.07534248, 0.66217987, 0.55606185, 0.65335174, 0.50251646),
    SE = c(NA, NA, 0.010447341, 0.008100473, 0.090220280, 0.085405371, 0.158355485, 0.144125581)
  ),
  "Log Disposition Time" = data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)", "OLS No Controls", "OLS With Controls",
              "2SLS No Controls", "2SLS With Controls", "UJIVE No Controls", "UJIVE With Controls"),
    Coefficient = c(5.23664270, 0.49862881, 0.08319742, 0.07596786, 0.59434067, 0.59788403, 0.58302114, 0.52169190),
    SE = c(NA, NA, 0.01236352, 0.01181026, 0.11542638, 0.09967402, 0.18862348, 0.17741165)
  ),
  "Imaging Tests" = data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)", "OLS No Controls", "OLS With Controls",
              "2SLS No Controls", "2SLS With Controls", "UJIVE No Controls", "UJIVE With Controls"),
    Coefficient = c(1.3346886, 0.5715169, 0.8407355, 0.8034681, 1.3703718, 1.2237245, 1.3157862, 1.1737585),
    SE = c(NA, NA, 0.009316152, 0.011751545, 0.125342558, 0.123151682, 0.126321275, 0.119119694)
  ),
  "72-Hour Return/Admit" = data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)", "OLS No Controls", "OLS With Controls",
              "2SLS No Controls", "2SLS With Controls", "UJIVE No Controls", "UJIVE With Controls"),
    Coefficient = c(0.0122967077, 0.1102120828, -0.0014889229, -0.0002564087, -0.0130670119, -0.0137607317, -0.0078968828, -0.0039445826),
    SE = c(NA, NA, 0.002058609, 0.002462671, 0.018354529, 0.019060785, 0.020307700, 0.021733407)
  ),
  "Hospital Admission" = data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)", "OLS No Controls", "OLS With Controls",
              "2SLS No Controls", "2SLS With Controls", "UJIVE No Controls", "UJIVE With Controls"),
    Coefficient = c(0.27945260, 0.44875251, 0.03557500, 0.02634335, 0.41028252, 0.40103546, 0.42371226, 0.39762749),
    SE = c(NA, NA, 0.01350472, 0.01132915, 0.09783126, 0.08898519, 0.10277279, 0.08964831)
  )
)

# Create the comparison table
comparison_table <- create_comparison_table(results_list)

# Print basic table
print(comparison_table)

# Create a publication-ready table with kableExtra
comparison_table %>%
  kbl(
    col.names = c("Outcome", "Mean (Seq)", "OLS", "2SLS\nNo Controls", 
                  "2SLS\nWith Controls", "UJIVE\nNo Controls", "UJIVE\nWith Controls"),
    align = c("l", "c", "c", "c", "c", "c", "c"),
    caption = "Treatment Effect Estimates: 2SLS vs UJIVE Comparison",
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  add_header_above(c(" " = 3, "Two-Stage Least Squares" = 2, "Unbiased Jackknife IV" = 2)) %>%
  footnote(
    general = "Standard errors in parentheses. All estimates use heteroskedasticity-robust SEs. Controls include patient characteristics (vital signs, age, lab performance) and fixed effects (day of week, month, complaint-ESI, race, gender, provider sex, capacity level).",
    general_title = "Notes:",
    footnote_as_chunk = TRUE
  )

# Save as CSV




# Check LOS patterns by batch tendency decile
check_los_by_batch_tendency <- function(data) {
  
  # Create batch tendency deciles
  data$batch_tendency_decile <- cut(data$batch.tendency, 
                                    breaks = quantile(data$batch.tendency, 
                                                      probs = seq(0, 1, 0.1)),
                                    labels = paste0("D", 1:10),
                                    include.lowest = TRUE)
  
  # Calculate mean LOS and other outcomes by decile
  summary_by_decile <- data %>%
    group_by(batch_tendency_decile) %>%
    summarise(
      mean_batch_tendency = mean(batch.tendency, na.rm = TRUE),
      mean_los = mean(ED_LOS, na.rm = TRUE),
      mean_log_los = mean(ln_ED_LOS, na.rm = TRUE),
      median_los = median(ED_LOS, na.rm = TRUE),
      mean_batched = mean(batched, na.rm = TRUE),
      mean_imaging = mean(imgTests, na.rm = TRUE),
      n_patients = n()
    ) %>%
    mutate(
      los_relative_to_d1 = mean_los / first(mean_los) - 1,
      log_los_diff_from_d1 = mean_log_los - first(mean_log_los)
    )
  
  print(summary_by_decile)
  
  # Calculate 10th vs 90th percentile difference
  d1_los <- summary_by_decile$mean_log_los[1]  # 10th percentile
  d10_los <- summary_by_decile$mean_log_los[10] # 90th percentile
  
  cat("\n=== Key Comparison ===\n")
  cat("10th percentile (D1) mean log LOS:", round(d1_los, 4), "\n")
  cat("90th percentile (D10) mean log LOS:", round(d10_los, 4), "\n")
  cat("Raw difference:", round(d10_los - d1_los, 4), "\n")
  cat("Percentage difference in LOS:", round((exp(d10_los - d1_los) - 1) * 100, 2), "%\n\n")
  
  # Plot it
  library(ggplot2)
  p1 <- ggplot(summary_by_decile, aes(x = batch_tendency_decile, y = mean_log_los)) +
    geom_point(size = 3) +
    geom_line(aes(group = 1)) +
    geom_hline(yintercept = mean(data$ln_ED_LOS), linetype = "dashed", color = "red") +
    labs(title = "Mean Log LOS by Batch Tendency Decile",
         subtitle = "Raw data before accounting for batching effect",
         x = "Batch Tendency Decile",
         y = "Mean Log LOS") +
    theme_minimal()
  
  p2 <- ggplot(summary_by_decile, aes(x = mean_batch_tendency, y = mean_log_los)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Log LOS vs Batch Tendency",
         subtitle = "Each point is a decile mean",
         x = "Mean Batch Tendency",
         y = "Mean Log LOS") +
    theme_minimal()
  
  print(p1)
  print(p2)
  
  # Run a simple regression to see the slope
  simple_reg <- lm(ln_ED_LOS ~ batch.tendency, data = data)
  cat("=== Simple Regression (No Controls) ===\n")
  cat("Coefficient on batch.tendency:", round(coef(simple_reg)[2], 3), "\n")
  cat("This suggests a 1pp increase in batch tendency → ", 
      round(coef(simple_reg)[2], 3), " increase in log LOS\n")
  cat("For 4.5pp (10th to 90th): ", round(coef(simple_reg)[2] * 0.045, 4), 
      " log LOS difference\n")
  cat("Or about ", round((exp(coef(simple_reg)[2] * 0.045) - 1) * 100, 2), "% difference\n\n")
  
  return(summary_by_decile)
}

# Run the analysis
decile_results <- check_los_by_batch_tendency(final)
