library(fixest)

run_models <- function(data, y_var) {
  
  # --- 1. Summary stats for sequenced (batched == 0) ---
  mean_batched_0 <- mean(data[[y_var]][data$batched == 0], na.rm = TRUE)
  sd_batched_0   <- sd(data[[y_var]][data$batched == 0], na.rm = TRUE)
  
  # --- 2. 2SLS without controls ---
  model_2 <- feols(
    as.formula(paste(y_var, "~ 0 | dayofweekt + month_of_year | batched ~ batch.tendency")),
    vcov = 'HC1',
    data = data
  )
  
  # --- 3. 2SLS with controls ---
  model_3 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + EXPERIENCE + age  |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX + capacity_level + LAB_PERF |
       batched ~ batch.tendency"
    )),
    data = data,
    vcov = 'HC1'
  )
  
  # --- 4. OLS without controls ---
  model_4 <- feols(
    as.formula(paste(y_var, "~ batched | dayofweekt + month_of_year")),
    vcov = 'HC1',
    data = data
  )
  
  # --- 5. OLS with controls ---
  model_5 <- feols(
    as.formula(paste(
      y_var,
      "~ batched + tachycardic + tachypneic + hrs_in_shift + febrile + hypotensive + age + EXPERIENCE   |
       dayofweekt + month_of_year + race +  complaint_esi + GENDER + PROVIDER_SEX + capacity_level"
    )),
    vcov = 'HC1',
    data = data
  )
  
  # --- 6. Collect coefficients and SEs safely ---
  extract_coef <- function(model, term) ifelse(term %in% names(coef(model)), coef(model)[term], NA)
  extract_se   <- function(model, term) ifelse(term %in% names(se(model)),   se(model)[term],   NA)
  
  results <- data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)", 
              "2SLS No Controls", "2SLS With Controls"),
    Coefficient = c(
      mean_batched_0,
      sd_batched_0,
      extract_coef(model_2, "fit_batched"),
      extract_coef(model_3, "fit_batched")
    ),
    SE = c(
      NA, NA,
      extract_se(model_2, "fit_batched"),
      extract_se(model_3, "fit_batched")
    )
  )
  
  print(results)
  
  # --- 7. Display regression table ---
  return(
    etable(model_2, model_3,
           keep = "batched")
  )
}

run_models(final, "ln_total_testing_time")
run_models(final, "ln_treat_time")
run_models(final, "ln_disp_time")
run_models(final, "ln_ED_LOS")
run_models(final, "imgTests")
run_models(final, "RTN_72_HR_ADMIT")
run_models(final, "RTN_72_HR")
run_models(final, "PLAIN_XRAY")
run_models(final, "US_PERF")
run_models(final, "NON_CON_CT_PERF")
run_models(final, "CON_CT_PERF")
run_models(final, "admit")





## START HERE JACOB


run_models_iv <- function(data, y_var) {
  
  cat("\n=====================================================\n")
  cat("                 RESULTS FOR", y_var, "\n")
  cat("=====================================================\n\n")
  
  #==============================
  # Define control sets
  #==============================
  
  # necessary controls (only assignment controls)
  NECESSARY <- "factor(dayofweekt) + factor(month_of_year)"
  
  # precision controls (everything else)
  PRECISION <- paste(
    "factor(complaint_esi)",
    "factor(race)",
    "factor(GENDER)",
    "factor(PROVIDER_SEX)",
    #"factor(capacity_level)",
    "tachycardic", "tachypneic", "febrile", "hypotensive",
    "LAB_PERF", "age", "EXPERIENCE", "hrs_in_shift",
    sep = " + "
  )
  
  #==============================
  # 1. UJIVE (necessary controls)
  #==============================
  ujive_form_necessary <- as.formula(paste0(
    y_var, " ~ batched + ", NECESSARY, " | ",
    "factor(ED_PROVIDER) + ", NECESSARY
  ))
  
  ujive_nec <- ujive(ujive_form_necessary, data = data)
  
  #==============================
  # 2. UJIVE (necessary + precision)
  #==============================
  ujive_form_full <- as.formula(paste0(
    y_var, " ~ batched + ", NECESSARY, " + ", PRECISION, " | ",
    "factor(ED_PROVIDER) + ", NECESSARY, " + ", PRECISION
  ))
  
  ujive_full <- ujive(ujive_form_full, data = data)
  
  #==============================
  # 3. 2SLS (batch tendency IV) — necessary only
  #==============================
  tsls_nec <- feols(
    as.formula(paste0(
      y_var, " ~ 0 | ", NECESSARY, " | batched ~ batch.tendency"
    )),
    data = data, vcov = "HC1"
  )
  
  #==============================
  # 4. 2SLS (batch tendency IV) — necessary + precision
  #==============================
  tsls_full <- feols(
    as.formula(paste0(
      y_var, " ~ ", PRECISION, " | ",
      NECESSARY, " + factor(complaint_esi) + factor(race) + factor(GENDER) + ",
      "factor(PROVIDER_SEX) + LAB_PERF | ",
      "batched ~ batch.tendency"
    )),
    data = data, vcov = "HC1"
  )
  
  #==============================
  # Extract estimates
  #==============================
  est <- data.frame(
    Model = c(
      "UJIVE (Necessary)",
      "UJIVE (Full Controls)",
      "2SLS Batch Tendency (Necessary)",
      "2SLS Batch Tendency (Full Controls)"
    ),
    Coefficient = c(
      ujive_nec$estimate["ujive","estimate"],
      ujive_full$estimate["ujive","estimate"],
      coef(tsls_nec)["fit_batched"],
      coef(tsls_full)["fit_batched"]
    ),
    SE = c(
      ujive_nec$estimate["ujive","se_hte"],
      ujive_full$estimate["ujive","se_hte"],
      se(tsls_nec)["fit_batched"],
      se(tsls_full)["fit_batched"]
    )
  )
  
  print(est)
  
  #==============================
  # Return regression table
  #==============================
  
  return(
    etable(
      tsls_nec, tsls_full,
      keep = "%fit_batched",
      dict = c("fit_batched" = "Batched")
    )
  )
}


###############################################
# Run on your outcomes
###############################################

run_models_iv(final, "ln_total_testing_time")
run_models_iv(final, "ln_treat_time")
run_models_iv(final, "ln_disp_time")
run_models_iv(final, "ln_ED_LOS")
run_models_iv(final, "imgTests")
run_models_iv(final, "RTN_72_HR_ADMIT")
run_models_iv(final, "RTN_72_HR")
run_models_iv(final, "PLAIN_XRAY")
run_models_iv(final, "US_PERF")
run_models_iv(final, "NON_CON_CT_PERF")
run_models_iv(final, "CON_CT_PERF")
run_models_iv(final, "admit")


run_models_iv(subset(final, discharge==1), "imgTests")



run_models_iv(subset(final, capacity_level=='Normal Operations') , "ln_disp_time")
run_models_iv(subset(final, capacity_level=='Normal Operations') , "ln_ED_LOS")
run_models_iv(subset(final, capacity_level=='Normal Operations') , "imgTests")
run_models_iv(subset(final, capacity_level=='Normal Operations') , "RTN_72_HR_ADMIT")

run_models_iv(subset(final, capacity_level=='Minor Overcapacity') , "ln_disp_time")
run_models_iv(subset(final, capacity_level=='Minor Overcapacity') , "ln_ED_LOS")
run_models_iv(subset(final, capacity_level=='Minor Overcapacity') , "imgTests")
run_models_iv(subset(final, capacity_level=='Minor Overcapacity') , "RTN_72_HR_ADMIT")

run_models_iv(subset(final, capacity_level=='Major Overcapacity') , "ln_disp_time")
run_models_iv(subset(final, capacity_level=='Major Overcapacity') , "ln_ED_LOS")
run_models_iv(subset(final, capacity_level=='Major Overcapacity') , "imgTests")
run_models_iv(subset(final, capacity_level=='Major Overcapacity') , "RTN_72_HR_ADMIT")


feols(batched ~ capacity_level + EXPERIENCE + PROVIDER_SEX + age + hrs_in_shift | tachycardic + tachypneic + febrile +
        hypotensive + LAB_PERF + complaint_esi + race + GENDER, final, vcov = 'HC1')

feols(imgTests ~ capacity_level + EXPERIENCE + PROVIDER_SEX + age + hrs_in_shift | tachycardic + tachypneic + febrile +
        hypotensive + LAB_PERF + complaint_esi + race + GENDER, final, vcov = 'HC1')
library(fixest)

final$capacity <- factor(final$capacity_level,
                         levels = c("Normal Operations",
                                    "Minor Overcapacity",
                                    "Major Overcapacity"))

# outcome variable
y <- "ln_ED_LOS"   # or "ln_disp_time", "imgTests", etc.

# Define interacted treatment and IV
fs_formula <- as.formula(paste0(
  "batched ~ batch.tendency + ",
  "batch.tendency:capacity + ",
  "factor(dayofweekt) + factor(month_of_year) + ",
  "factor(complaint_esi) + factor(race) + factor(GENDER) + ",
  "factor(PROVIDER_SEX) + factor(capacity_level) + ",
  "LAB_PERF + age + EXPERIENCE + hrs_in_shift"
))

ss_formula <- as.formula(paste0(
  y, " ~ batched + batched:capacity | ",
  "factor(dayofweekt) + factor(month_of_year) + ",
  "factor(complaint_esi) + factor(race) + factor(GENDER) + ",
  "factor(PROVIDER_SEX) + factor(capacity_level) + ",
  "LAB_PERF + age + EXPERIENCE + hrs_in_shift | ",
  # endogenous + instruments
  "batched + batched:capacity ~ batch.tendency + batch.tendency:capacity"
))

iv_int <- feols(ss_formula, data = final, vcov = "HC1")
summary(iv_int)


run_models_iv_and_glm <- function(data, y_var, outcome_type) {
  
  cat("\n=====================================================\n")
  cat("           RESULTS FOR", y_var, "(Model Robustness)\n")
  cat("=====================================================\n\n")
  
  #==============================
  # Define control sets
  #==============================
  NECESSARY <- "factor(dayofweekt) + factor(month_of_year)"
  
  PRECISION <- paste(
    "factor(complaint_esi)",
    "factor(race)",
    "factor(GENDER)",
    "factor(PROVIDER_SEX)",
    "factor(capacity_level)",
    "tachycardic", "tachypneic", "febrile", "hypotensive",
    "LAB_PERF", "age", "EXPERIENCE", "hrs_in_shift",
    sep = " + "
  )
  
  #==============================
  # 1. First-stage for batch tendency IV
  #==============================
  fs <- feols(
    as.formula(paste0("batched ~ batch.tendency | ", NECESSARY, " + ", PRECISION)),
    data = data
  )
  
  data$batched_hat <- fs$fitted.values
  
  #==============================
  # 2. Second-stage: linear 2SLS (main estimator)
  #==============================
  tsls <- feols(
    as.formula(paste0(
      y_var, " ~ ", PRECISION, 
      " | ", NECESSARY, 
      " | batched ~ batch.tendency"
    )),
    data = data, vcov = "HC1"
  )
  
  #==============================
  # 3. Nonlinear outcome models (NOT causal IV)
  #==============================
  
  if (outcome_type == "binary") {
    glm_model <- glm(
      as.formula(paste0(y_var, " ~ batched_hat + ", NECESSARY, " + ", PRECISION)),
      data = data, family = binomial(link = "logit")
    )
    marginal <- margins::margins(glm_model, variables = "batched_hat")$AME
    
  } else if (outcome_type == "count") {
    glm_model <- glm(
      as.formula(paste0(y_var, " ~ batched_hat + ", NECESSARY, " + ", PRECISION)),
      data = data, family = poisson(link = "log")
    )
    marginal <- margins::margins(glm_model, variables = "batched_hat")$AME
    
  } else {
    glm_model <- lm(
      as.formula(paste0(y_var, " ~ batched_hat + ", NECESSARY, " + ", PRECISION)),
      data = data
    )
    marginal <- coef(glm_model)["batched_hat"]
  }
  
  #==============================
  # 4. Combine results
  #==============================
  results <- data.frame(
    Model = c("2SLS (Causal LATE)", "Nonlinear Outcome Model"),
    Estimate = c(
      coef(tsls)["fit_batched"],
      marginal
    ),
    row.names = NULL
  )
  
  print(results)
  
  return(results)
}


#==============================
# Example usage:
# ln_total_testing_time is continuous
run_models_iv_and_glm(final, "ln_disp_time", "continuous")
run_models_iv_and_glm(final, "ln_ED_LOS", "continuous")

# admission_72hr is binary
run_models_iv_and_glm(final, "RTN_72_HR_ADMIT", "binary")
run_models_iv_and_glm(final, "admit", "binary")

# num_tests is count
run_models_iv_and_glm(final, "imgTests", "count")




###########################################################
# 0. SET OUTCOME
###########################################################
fs_model <- feols(batched ~ batch.tendency +
                    tachycardic + tachypneic + febrile + hypotensive + 
                    EXPERIENCE + PROVIDER_SEX + LAB_PERF + EXPERIENCE + hrs_in_shift +
                    age + capacity_level | 
                    dayofweekt + month_of_year + complaint_esi + race + GENDER, 
                  vcov = 'HC1', data = final)

# Store the fitted values
final$batched_hat <- fs_model$fitted.values

cat("First stage completed. Var(batched_hat) =", var(final$batched_hat), "\n")

ss_formula <- as.formula(
  paste0(
    'admit', " ~ ", PRECISION, 
    " | ", NECESSARY, 
    " | batched ~ batch.tendency"
  )
)

tsls <- feols(ss_formula, data = final, vcov = "HC1")

causal_estimate <- coef(tsls)["fit_batched"]
cat("2SLS estimate:", causal_estimate, "\n")


glm_mod <- glm(
  as.formula(
    paste0('admit', " ~ batched_hat + ", NECESSARY, " + ", PRECISION)
  ),
  data = final,
  family = binomial(link = "logit")
)

summary(margins::margins(glm_mod, variables = "batched_hat"))



glm_mod <- glm(
  as.formula(
    paste0('imgTests', " ~ batched_hat + ", NECESSARY, " + ", PRECISION)
  ),
  data = final,
  family = poisson(link = "log")
)
summary(margins::margins(glm_mod, variables = "batched_hat"))




# write a function that runs the first stage regression and returns the summary

m1 <- feols(batched ~ batch.tendency + tachycardic + tachypneic + febrile + hypotensive + 
         age  | 
        dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level, 
      cluster = ~ED_PROVIDER, data = final) 


m2 <- feols(batched ~ batch.tendency + tachycardic + tachypneic + febrile + hypotensive + 
        EXPERIENCE + PROVIDER_SEX + age  | 
        dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level, 
      cluster = ~ED_PROVIDER, data = final) 

m3 <- feols(batched ~ batch.tendency + tachycardic + tachypneic + febrile + hypotensive + 
                      EXPERIENCE + PROVIDER_SEX + hrs_in_shift + age | 
                      dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level, 
                    cluster = ~ED_PROVIDER, data = final)


m4 <- feols(batched ~ batch.tendency + tachycardic + tachypneic + febrile + hypotensive + 
        EXPERIENCE + PROVIDER_SEX + LAB_PERF + hrs_in_shift +  age  | 
        dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level, 
      cluster = ~ED_PROVIDER, data = final) 


m5 <- feols(batched ~ batch.tendency + tachycardic + tachypneic + febrile + hypotensive + 
        EXPERIENCE + PROVIDER_SEX + LAB_PERF + hrs_in_shift + admit.tendency + age | 
        dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level, 
      cluster = ~ED_PROVIDER, data = final) 

etable(m1, m2, m3, cluster = "ED_PROVIDER",
       se = "cluster", keep = c("batch.tendency"))









library(fixest)

run_models_with_cf <- function(data, y_var) {
  
  # --- Control Function Approach ---
  
  # First stage: Predict batching from instrument
  first_stage <- feols(
    batched ~ batch.tendency + tachycardic + tachypneic + febrile + hypotensive + 
      hrs_in_shift + EXPERIENCE + age  | 
      dayofweekt + month_of_year + complaint_esi + race + GENDER + 
      PROVIDER_SEX + capacity_level,
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # Extract residuals from first stage
  data$first_stage_resid <- residuals(first_stage)
  
  # Control Function: Include both predicted batching AND residuals
  model_cf <- feols(
    as.formula(paste(
      y_var,
      "~ batched + first_stage_resid + tachycardic + tachypneic + febrile + 
         hypotensive + hrs_in_shift + EXPERIENCE + age  |
         dayofweekt + month_of_year + complaint_esi + race + GENDER + 
         PROVIDER_SEX + capacity_level"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # Your original 2SLS for comparison
  model_2sls <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + EXPERIENCE + age |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX + capacity_level |
       batched ~ batch.tendency"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # Compare results
  cat("\n=== Results for", y_var, "===\n")
  cat("\nControl Function Approach:\n")
  cat("Batched coefficient:", coef(model_cf)["batched"], "\n")
  cat("Batched SE:", se(model_cf)["batched"], "\n")
  cat("First-stage residual coefficient:", coef(model_cf)["first_stage_resid"], "\n")
  cat("First-stage residual p-value:", 
      2 * pnorm(-abs(coef(model_cf)["first_stage_resid"] / se(model_cf)["first_stage_resid"])), "\n")
  
  cat("\nStandard 2SLS:\n")
  cat("Batched coefficient:", coef(model_2sls)["fit_batched"], "\n")
  cat("Batched SE:", se(model_2sls)["fit_batched"], "\n")
  
  return(list(cf = model_cf, iv = model_2sls, first_stage = first_stage))
}

# Conley et al. (2012) style bounds
sensitivity_analysis <- function(data, y_var, delta_range = seq(-0.2, 0.2, by = 0.02)) {
  
  results <- data.frame(
    delta = numeric(),
    coef = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric()
  )
  
  for (delta in delta_range) {
    # Adjust outcome by delta * instrument
    data$y_adjusted <- data[[y_var]] - delta * data$batch.tendency
    
    # Run 2SLS on adjusted outcome
    model <- feols(
      as.formula(paste(
        "y_adjusted ~ tachycardic + tachypneic + febrile + hypotensive + 
         hrs_in_shift + EXPERIENCE + age |
         dayofweekt + month_of_year + complaint_esi + race + GENDER + 
         PROVIDER_SEX + capacity_level |
         batched ~ batch.tendency"
      )),
      cluster = ~ED_PROVIDER,
      data = data
    )
    
    coef_val <- coef(model)["fit_batched"]
    se_val <- se(model)["fit_batched"]
    
    results <- rbind(results, data.frame(
      delta = delta,
      coef = coef_val,
      ci_lower = coef_val - 1.96 * se_val,
      ci_upper = coef_val + 1.96 * se_val
    ))
  }
  
  # Plot sensitivity
  library(ggplot2)
  p <- ggplot(results, aes(x = delta)) +
    geom_line(aes(y = coef), color = "blue") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      x = "Direct effect of instrument on outcome (δ)",
      y = "Treatment effect estimate",
      title = paste("Sensitivity of", y_var, "to exclusion restriction violations")
    ) +
    theme_minimal()
  
  
  
  print(p)
  return(results)
}

# Run the enhanced analysis
outcomes <- c("ln_disp_time", "ln_ED_LOS")

for (outcome in outcomes) {
  cat("\n\n========================================\n")
  cat("OUTCOME:", outcome)
  cat("\n========================================\n")
  
  # 1. Control function approach
  cf_results <- run_models_with_cf(final, outcome)
  
  # 3. Sensitivity analysis
  sensitivity_results <- sensitivity_analysis(final, outcome)
}

# Create comparison table
etable(
  cf_results$iv, 
  cf_results$cf,
  headers = c("Standard 2SLS", "Control Function", "2SLS + Proxies"),
  cluster = ~ED_PROVIDER,
  keep = c("batched", "fit_batched", "first_stage_resid")
)




library(tidyverse)
library(fixest)

#------------------------------------------------------------------
# Compute UJIVE leniency instrument using ED_PROVIDER as "judge"
#------------------------------------------------------------------
compute_ujive_instrument <- function(data, 
                                     treat,           # character, e.g. "batched"
                                     judge,           # character, e.g. "ED_PROVIDER"
                                     assign_controls  # formula, e.g. ~ dayofweekt + month_of_year + ...
) {
  
  # 1. Pull variables
  x <- data[[treat]]
  judge_f <- factor(data[[judge]])
  
  # Judge dummies (Z): one column per provider, no intercept
  Z <- model.matrix(~ judge_f - 1)
  
  # Assignment controls (W); recommend specifying with "- 1" or without intercept, 
  # but this will work as long as W is full rank.
  if (!is.null(assign_controls)) {
    W <- model.matrix(assign_controls, data = data)
    
    # 2. Residualize treatment and instruments on W (Frisch–Waugh–Lovell)
    # x_tilde = M_W x, Z_tilde = M_W Z
    
    # Projection of Z on W in one shot
    XtX_W <- crossprod(W)                    # W'W   (L x L)
    WX    <- crossprod(W, x)                 # W'x   (L x 1)
    WZ    <- crossprod(W, Z)                 # W'Z   (L x K)
    
    beta_W_x <- solve(XtX_W, WX)             # (W'W)^{-1} W'x
    beta_W_Z <- solve(XtX_W, WZ)             # (W'W)^{-1} W'Z  (L x K)
    
    x_hat_W <- as.vector(W %*% beta_W_x)
    Z_hat_W <- W %*% beta_W_Z
    
    x_tilde <- x - x_hat_W                   # residuals of x on W
    Z_tilde <- Z - Z_hat_W                   # residuals of each Z column on W
    
  } else {
    # No controls: center x and Z
    x_tilde <- x - mean(x, na.rm = TRUE)
    Z_tilde <- scale(Z, center = TRUE, scale = FALSE)
  }
  
  # 3. OLS of residualized treatment on residualized instruments:
  #    x_tilde = Z_tilde * pi + v
  XtX_Z <- crossprod(Z_tilde)                # Z_tilde' Z_tilde  (K x K)
  ZX    <- crossprod(Z_tilde, x_tilde)       # Z_tilde' x_tilde  (K x 1)
  
  pi_hat <- solve(XtX_Z, ZX)                 # K x 1
  xhat_tilde <- as.vector(Z_tilde %*% pi_hat)
  e <- x_tilde - xhat_tilde                  # residuals
  
  # 4. Hat matrix diagonals h_ii for regression x_tilde ~ Z_tilde
  inv_XtX_Z <- solve(XtX_Z)
  H_diag <- rowSums((Z_tilde %*% inv_XtX_Z) * Z_tilde)
  
  # 5. Leave-one-out predicted residualized treatment:
  #    \hat l_{-i} = x_tilde_i - e_i / (1 - h_ii)
  #    (standard LOOCV identity)
  
  denom <- 1 - H_diag
  # Guard against any exact-leverage issues
  if (any(abs(denom) < 1e-10)) {
    warning("Some 1 - h_ii are ~0; setting those UJIVE instruments to NA.")
    denom[abs(denom) < 1e-10] <- NA_real_
  }
  
  lhat_loo <- x_tilde - e / denom
  
  # This lhat_loo is the relative leniency instrument (residualized w.r.t. W), 
  # suitable to use as a single instrument in a just-identified IV regression.
  return(as.numeric(lhat_loo))
}

# Assignment controls for provider assignment (W):
assign_controls_formula <- ~ dayofweekt + month_of_year 

final$z_ujive <- compute_ujive_instrument(
  data            = final,
  treat           = "batched",
  judge           = "ED_PROVIDER",
  assign_controls = assign_controls_formula
)

run_models_ujive <- function(data, y_var) {
  
  # 1. Summary stats for sequenced (batched == 0)
  mean_batched_0 <- mean(data[[y_var]][data$batched == 0], na.rm = TRUE)
  sd_batched_0   <- sd(data[[y_var]][data$batched == 0], na.rm = TRUE)
  
  # 2. UJIVE 2SLS without additional covariates (FE only)
  #    y ~ 0 | day/time FE | batched ~ z_ujive
  model_2 <- feols(
    as.formula(paste(
      y_var,
      "~ 0 | dayofweekt + month_of_year | batched ~ z_ujive"
    )),
    data = data,
    vcov = "HC1"
  )
  
  # 3. UJIVE 2SLS with covariates and FE
  #    Note: keep assignment controls in FE/controls consistent with what
  #    you used inside compute_ujive_instrument.
  model_3 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + EXPERIENCE + age |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX + capacity_level + LAB_PERF |
       batched ~ z_ujive"
    )),
    data = data,
    vcov = "HC1"
  )
  
  # 4. OLS comparators (unchanged from your code)
  model_4 <- feols(
    as.formula(paste(y_var, "~ batched | dayofweekt + month_of_year")),
    vcov = "HC1",
    data = data
  )
  
  model_5 <- feols(
    as.formula(paste(
      y_var,
      "~ batched + tachycardic + tachypneic + hrs_in_shift + febrile + hypotensive + age + EXPERIENCE |
       dayofweekt + month_of_year + race + complaint_esi + GENDER + PROVIDER_SEX + capacity_level"
    )),
    vcov = "HC1",
    data = data
  )
  
  # 5. Collect coefficients and SEs for UJIVE specs
  extract_coef <- function(model, term) ifelse(term %in% names(coef(model)), coef(model)[term], NA)
  extract_se   <- function(model, term) ifelse(term %in% names(se(model)),   se(model)[term],   NA)
  
  results <- data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)",
              "UJIVE 2SLS No Controls", "UJIVE 2SLS With Controls"),
    Coefficient = c(
      mean_batched_0,
      sd_batched_0,
      extract_coef(model_2, "batched"),
      extract_coef(model_3, "batched")
    ),
    SE = c(
      NA, NA,
      extract_se(model_2, "batched"),
      extract_se(model_3, "batched")
    )
  )
  
  print(results)
  
  # 6. Return nice regression table
  etable(
    model_2, model_3, model_4, model_5,
    keep = "batched",
    se.below = TRUE
  )
}

# Example calls (mirroring your original):
run_models_ujive(final, "ln_total_testing_time")
run_models_ujive(final, "ln_treat_time")
run_models_ujive(final, "ln_disp_time")
run_models_ujive(final, "ln_ED_LOS")
run_models_ujive(final, "imgTests")
run_models_ujive(final, "RTN_72_HR_ADMIT")
run_models_ujive(final, "RTN_72_HR")
run_models_ujive(final, "PLAIN_XRAY")
run_models_ujive(final, "US_PERF")
run_models_ujive(final, "NON_CON_CT_PERF")
run_models_ujive(final, "CON_CT_PERF")
run_models_ujive(final, "admit")

















