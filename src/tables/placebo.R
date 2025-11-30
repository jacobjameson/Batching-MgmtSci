################################################################################
#-------------------------------------------------------------------------------
# Reduced Form
#-------------------------------------------------------------------------------
################################################################################

sink('outputs/tables/placebo.txt')

placebo <- data %>%
  group_by(complaint_esi) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  group_by(complaint_esi) %>%
  mutate(batchmean = mean(batched)) %>%
  ungroup() %>% 
  filter(imgTests == 0)

placebo$ln_ED_LOS <- log(placebo$ED_LOS)
placebo$ln_disp_time <- log(placebo$time_to_dispo)


placebo$capacity_level <- factor(placebo$capacity_level,
                               levels = c('Normal Operations', 
                                          'Minor Overcapacity', 
                                          'Major Overcapacity'))

# ------------------------------------------------------------------------------

rf_model_disp <- feols(
  ln_disp_time ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level + # ED variables
    LAB_PERF + EXPERIENCE  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = placebo, vcov = "HC1")

rf_model_los <- feols(
  ln_ED_LOS ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level + # ED variables
    LAB_PERF + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = placebo, vcov = "HC1")

rf_model_ra <- feols(
  RTN_72_HR_ADMIT ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level + # ED variables
    LAB_PERF + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = placebo, vcov = "HC1")

# ------------------------------------------------------------------------------

etable(rf_model_disp, rf_model_los, rf_model_ra, 
       keep = c("batch.tendency"))


quantile(data$batch.tendency, probs = seq(0, 1, 0.1))[c(2,10)]

# Calculate F-statistics for reduced-form models
wald_rf_1 <- wald(rf_model_disp, cluster = "ED_PROVIDER")
wald_rf_2 <- wald(rf_model_los, cluster = "ED_PROVIDER")
wald_rf_3 <- wald(rf_model_img, cluster = "ED_PROVIDER")
wald_rf_4 <- wald(rf_model_ra, cluster = "ED_PROVIDER")


print(paste('ln_disp_time mean:', mean(data$ln_disp_time)))
print(paste('ln_disp_time sd:', sd(data$ln_disp_time)))

print(paste('ln_ED_LOS mean:', mean(data$ln_ED_LOS)))
print(paste('ln_ED_LOS sd:', sd(data$ln_ED_LOS)))

print(paste('imgTests mean:', mean(data$imgTests)))
print(paste('imgTests sd:', sd(data$imgTests)))

print(paste('RTN_72_HR_ADMIT mean:', mean(data$RTN_72_HR_ADMIT)))
print(paste('RTN_72_HR_ADMIT sd:', sd(data$RTN_72_HR_ADMIT)))

sink()


##
# Alternative Instrument: Physician Fixed Effects with Leave-One-Out Correction

# Step 1: Estimate model WITH physician fixed effects
model_with_fe <- felm(batched ~ tachycardic + tachypneic + febrile + hypotensive + age + LAB_PERF 
                      | dayofweekt + month_of_year + complaint_esi + race + GENDER + ED_PROVIDER 
                      | 0 | ED_PROVIDER, 
                      data = data)

# Step 2: Extract physician fixed effects using getfe()
physician_fes <- getfe(model_with_fe, se = FALSE)

# Filter to only physician FEs (not the other FEs like dayofweekt, etc.)
physician_fes <- physician_fes %>%
  filter(fe == "ED_PROVIDER") %>%
  select(idx, effect) %>%
  rename(ED_PROVIDER = idx, physician_fe = effect)

# Step 3: Merge physician FEs back to main data
data <- data %>%
  left_join(physician_fes, by = "ED_PROVIDER")

# Step 4: Get residuals from the model
# These residuals are after removing ALL fixed effects
data$residual_batch_fe_model <- resid(model_with_fe)

# Step 5: Calculate the "full" residual including physician FE
# This is: (batched - X'beta - other_FEs) = residual + physician_fe
data$full_residual <- data$residual_batch_fe_model + data$physician_fe

# Step 6: Apply leave-one-out correction
# batch.tendency.fe = (N_j * alpha_j - full_residual_i) / (N_j - 1)
data <- data %>%
  group_by(ED_PROVIDER) %>%
  mutate(
    n_provider = n(),
    sum_full_residual = sum(full_residual, na.rm = TRUE),
    batch.tendency.fe = (sum_full_residual - full_residual) / (n_provider - 1)
  ) %>%
  ungroup()

# Clean up intermediate variables if desired
data <- data %>%
  select(-residual_batch_fe_model, -full_residual, -sum_full_residual, -n_provider)

# Now you have two instruments:
# - batch.tendency (your original residual-based instrument)
# - batch.tendency.fe (the new physician FE-based instrument)

final <- data %>%
  group_by(complaint_esi) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  group_by(complaint_esi) %>%
  mutate(batchmean = mean(batched)) %>%
  ungroup() %>% 
  filter(batchmean > 0.05, imaging == 1)

final$ln_ED_LOS <- log(final$ED_LOS)
final$ln_disp_time <- log(final$time_to_dispo)


final$capacity_level <- factor(final$capacity_level,
                               levels = c('Normal Operations', 
                                          'Minor Overcapacity', 
                                          'Major Overcapacity'))


fs_model_1 <- feols(batched ~ batch.tendency +
                      tachycardic + tachypneic + febrile + hypotensive + 
                      EXPERIENCE + PROVIDER_SEX + LAB_PERF  +
                      age + capacity_level | 
                      dayofweekt + month_of_year + complaint_esi + race + GENDER, 
                    cluster = ~ED_PROVIDER, data = final)

fs_model_2 <- feols(batched ~ batch.tendency.fe +
                      tachycardic + tachypneic + febrile + hypotensive + 
                      EXPERIENCE + PROVIDER_SEX + LAB_PERF  +
                      age + capacity_level | 
                      dayofweekt + month_of_year + complaint_esi + race + GENDER, 
                    cluster = ~ED_PROVIDER, data = final)

wald(fs_model_1, keep = "batch.tendency", cluster = "ED_PROVIDER")
wald(fs_model_2, keep = "batch.tendency.fe", cluster = "ED_PROVIDER")
etable(fs_model_1, fs_model_2, cluster = "ED_PROVIDER",
       se = "cluster", keep = c("batch.tendency", "batch.tendency.fe"))



run_models <- function(data, y_var) {
  
  # --- 2. 2SLS without controls ---
  model_1 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + EXPERIENCE + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX + capacity_level |
       batched ~ batch.tendency"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  
  # --- 3. 2SLS with controls ---
  model_2 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + EXPERIENCE + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX + capacity_level |
       batched ~ batch.tendency.fe"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )

  
  # --- 6. Collect coefficients and SEs safely ---
  extract_coef <- function(model, term) ifelse(term %in% names(coef(model)), coef(model)[term], NA)
  extract_se   <- function(model, term) ifelse(term %in% names(se(model)),   se(model)[term],   NA)
  
  results <- data.frame(
    Model = c("Old Instrument", "New Instrument"),
    Coefficient = c(
      extract_coef(model_1, "fit_batched"),
      extract_coef(model_2, "fit_batched")
    ),
    SE = c(
      NA, NA,
      extract_se(model_1, "fit_batched"),
      extract_se(model_2, "fit_batched")
    )
  )
  
  print(results)
  
  # --- 7. Display regression table ---
  return(
    etable(model_1, model_2,
           cluster = "ED_PROVIDER", se = "cluster",
           keep = "batched")
  )
}

run_models(final, "ln_disp_time")
run_models(final, "ln_ED_LOS")
run_models(final, "imgTests")
run_models(final, "RTN_72_HR_ADMIT")


###

library(fixest)
library(margins)  # for marginal effects

# Function to compare linear OLS with nonlinear models
compare_functional_forms <- function(data, y_var, outcome_type = "binary") {
  
  # Linear OLS (your model_5 specification)
  linear_ols <- feols(
    as.formula(paste(
      y_var,
      "~ batched + tachycardic + tachypneic + hrs_in_shift + febrile + hypotensive + age + EXPERIENCE + LAB_PERF |
       dayofweekt + month_of_year + race + complaint_esi + GENDER + PROVIDER_SEX + capacity_level"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # Nonlinear model depends on outcome type
  if (outcome_type == "binary") {
    # Logit for binary outcomes
    nonlinear_model <- feglm(
      as.formula(paste(
        y_var,
        "~ batched + tachycardic + tachypneic + hrs_in_shift + febrile + hypotensive + age + EXPERIENCE + LAB_PERF |
         dayofweekt + month_of_year + race + complaint_esi + GENDER + PROVIDER_SEX + capacity_level"
      )),
      family = binomial(link = "logit"),
      cluster = ~ED_PROVIDER,
      data = data
    )
    model_name <- "Logit"
    
  } else if (outcome_type == "count") {
    # Negative binomial for count outcomes
    nonlinear_model <- fenegbin(
      as.formula(paste(
        y_var,
        "~ batched + tachycardic + tachypneic + hrs_in_shift + febrile + hypotensive + age + EXPERIENCE + LAB_PERF |
         dayofweekt + month_of_year + race + complaint_esi + GENDER + PROVIDER_SEX + capacity_level"
      )),
      cluster = ~ED_PROVIDER,
      data = data
    )
    model_name <- "Negative Binomial"
    
  } else {
    stop("outcome_type must be 'binary' or 'count'")
  }
  
  # Extract coefficients
  linear_coef <- coef(linear_ols)["batched"]
  linear_se <- se(linear_ols)["batched"]
  
  nonlinear_coef <- coef(nonlinear_model)["batched"]
  nonlinear_se <- se(nonlinear_model)["batched"]
  
  # Calculate marginal effect for nonlinear model
  # Average marginal effect = average of marginal effects across all observations
  if (outcome_type == "binary") {
    # For logit: ME = beta * Lambda(X'beta) where Lambda is logistic PDF
    # Average: mean(beta * exp(X'beta) / (1 + exp(X'beta))^2)
    
    # Get predicted linear index
    xb <- predict(nonlinear_model, type = "link")
    # Logistic PDF: exp(xb) / (1 + exp(xb))^2
    lambda <- exp(xb) / (1 + exp(xb))^2
    # Marginal effect for each obs
    me_i <- nonlinear_coef * lambda
    # Average marginal effect
    ame <- mean(me_i, na.rm = TRUE)
    
    # Standard error via delta method (approximate)
    # SE(AME) ≈ SE(beta) * mean(lambda)
    ame_se <- nonlinear_se * mean(lambda, na.rm = TRUE)
    
  } else if (outcome_type == "count") {
    # For negative binomial: ME = beta * lambda(X'beta) where lambda is exp(X'beta)
    # Average: mean(beta * exp(X'beta))
    
    # Get predicted linear index
    xb <- predict(nonlinear_model, type = "link")
    # Count data: lambda = exp(xb)
    lambda <- exp(xb)
    # Marginal effect for each obs
    me_i <- nonlinear_coef * lambda
    # Average marginal effect
    ame <- mean(me_i, na.rm = TRUE)
    
    # Standard error via delta method (approximate)
    ame_se <- nonlinear_se * mean(lambda, na.rm = TRUE)
  }
  
  # Create results table
  results <- data.frame(
    Model = c("Linear OLS", model_name, paste(model_name, "- AME")),
    Coefficient = c(linear_coef, nonlinear_coef, ame),
    SE = c(linear_se, nonlinear_se, ame_se),
    Type = c("Direct comparison", "Log-odds/Log-count", "Marginal effect")
  )
  
  cat("\n=== Functional Form Comparison:", y_var, "===\n")
  print(results, row.names = FALSE)
  
  # Return models for further inspection
  return(list(
    linear = linear_ols,
    nonlinear = nonlinear_model,
    results = results
  ))
}

# Run comparisons
# Binary outcomes
admission_compare <- compare_functional_forms(final, "admit", outcome_type = "binary")
return_compare <- compare_functional_forms(final, "RTN_72_HR_ADMIT", outcome_type = "binary")

# Count outcome
tests_compare <- compare_functional_forms(final, "imgTests", outcome_type = "count")

# Create a summary table for the paper
create_comparison_table <- function(admission_res, return_res, tests_res) {
  
  df <- data.frame(
    Outcome = rep(c("Admission", "72hr return w/ admit", "Number of tests"), each = 2),
    Model = rep(c("Linear OLS", "Nonlinear (AME)"), 3),
    Coefficient = c(
      admission_res$results$Coefficient[1],
      admission_res$results$Coefficient[3],
      return_res$results$Coefficient[1],
      return_res$results$Coefficient[3],
      tests_res$results$Coefficient[1],
      tests_res$results$Coefficient[3]
    ),
    SE = c(
      admission_res$results$SE[1],
      admission_res$results$SE[3],
      return_res$results$SE[1],
      return_res$results$SE[3],
      tests_res$results$SE[1],
      tests_res$results$SE[3]
    )
  )
  
  df$Stars <- ifelse(abs(df$Coefficient/df$SE) > 2.576, "***",
                     ifelse(abs(df$Coefficient/df$SE) > 1.96, "**",
                            ifelse(abs(df$Coefficient/df$SE) > 1.645, "*", "")))
  
  return(df)
}

comparison_table <- create_comparison_table(admission_compare, return_compare, tests_compare)
print(comparison_table)


#####
#comparison of groups
# FILTER TO MAIN SAMPLE
main <- data %>%
  group_by(complaint_esi) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  group_by(complaint_esi) %>%
  mutate(batchmean = mean(batched)) %>%
  ungroup() %>% 
  filter(batchmean > 0.05, imaging == 1)


other <- data %>%
  group_by(complaint_esi) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  group_by(complaint_esi) %>%
  mutate(batchmean = mean(batched)) %>%
  ungroup() %>% 
  filter(batchmean <= 0.05 | imaging == 1)


library(dplyr)
library(broom)

# Define included and excluded samples
# Included: imaging==1 AND batchmean > 0.05
included <- data %>%
  group_by(complaint_esi) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  group_by(complaint_esi) %>%
  mutate(batchmean = mean(batched)) %>%
  ungroup() %>% 
  filter(batchmean > 0.05, imaging == 1) %>%
  mutate(sample = "Included")

# Excluded: everything else (imaging==0 OR batchmean <= 0.05)
excluded <- data %>%
  group_by(complaint_esi) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  group_by(complaint_esi) %>%
  mutate(batchmean = mean(batched)) %>%
  ungroup() %>% 
  filter(batchmean <= 0.05 | imaging == 0) %>%
  mutate(sample = "Excluded")

# Function to compare a variable between two groups
compare_variable <- function(data_inc, data_exc, var_name, var_type = "continuous") {
  
  if (var_type == "continuous") {
    # T-test for continuous variables
    mean_inc <- mean(data_inc[[var_name]], na.rm = TRUE)
    sd_inc <- sd(data_inc[[var_name]], na.rm = TRUE)
    
    mean_exc <- mean(data_exc[[var_name]], na.rm = TRUE)
    sd_exc <- sd(data_exc[[var_name]], na.rm = TRUE)
    
    # T-test
    test_result <- t.test(data_inc[[var_name]], data_exc[[var_name]])
    
    return(data.frame(
      Variable = var_name,
      Excluded_Mean = round(mean_exc, 2),
      Excluded_SD = paste0("(", round(sd_exc, 2), ")"),
      Included_Mean = round(mean_inc, 2),
      Included_SD = paste0("(", round(sd_inc, 2), ")"),
      P_Value = round(test_result$p.value, 3),
      Significant = ifelse(test_result$p.value < 0.05, "*", "")
    ))
    
  } else if (var_type == "binary") {
    # Proportion test for binary variables
    prop_inc <- mean(data_inc[[var_name]], na.rm = TRUE) * 100
    n_inc <- sum(!is.na(data_inc[[var_name]]))
    
    prop_exc <- mean(data_exc[[var_name]], na.rm = TRUE) * 100
    n_exc <- sum(!is.na(data_exc[[var_name]]))
    
    # Proportion test
    test_result <- prop.test(
      x = c(sum(data_inc[[var_name]], na.rm = TRUE), 
            sum(data_exc[[var_name]], na.rm = TRUE)),
      n = c(n_inc, n_exc)
    )
    
    return(data.frame(
      Variable = var_name,
      Excluded_Mean = paste0(round(prop_exc, 1), "%"),
      Excluded_SD = "",
      Included_Mean = paste0(round(prop_inc, 1), "%"),
      Included_SD = "",
      P_Value = round(test_result$p.value, 3),
      Significant = ifelse(test_result$p.value < 0.05, "*", "")
    ))
  }
}

included$ESI <- as.numeric(included$ESI)
excluded$ESI <- as.numeric(excluded$ESI)

  compare_variable(included, excluded, "age", "continuous")

  # Acuity (assuming ESI is numeric where lower = higher acuity)
  # You may need to create indicator variables if ESI is categorical
  data.frame(
    Variable = "ESI Level (mean)",
    Excluded_Mean = round(mean(excluded$ESI, na.rm = TRUE), 2),
    Excluded_SD = paste0("(", round(sd(excluded$ESI, na.rm = TRUE), 2), ")"),
    Included_Mean = round(mean(included$ESI, na.rm = TRUE), 2),
    Included_SD = paste0("(", round(sd(included$ESI, na.rm = TRUE), 2), ")"),
    P_Value = t.test(included$ESI, excluded$ESI)$p.value,
    Significant = ifelse(t.test(included$ESI, excluded$ESI)$p.value < 0.05, "*", "")
  )
  
  # Vital signs
  compare_variable(included, excluded, "tachycardic", "binary")
  compare_variable(included, excluded, "tachypneic", "binary")
  compare_variable(included, excluded, "febrile", "binary")
  compare_variable(included, excluded, "hypotensive", "binary")
  
  # Outcomes
  compare_variable(included, excluded, "ED_LOS", "continuous")
  compare_variable(included, excluded, "admit", "binary")


# Print formatted table
print(comparison_results)

# Sample sizes
cat("\nSample Sizes:\n")
cat("Excluded:", nrow(excluded), "\n")
cat("Included:", nrow(included), "\n")
cat("Total:", nrow(excluded) + nrow(included), "\n")
cat("Percentage Included:", round(100 * nrow(included) / (nrow(excluded) + nrow(included)), 1), "%\n")

# Create LaTeX-friendly version
create_latex_table <- function(comp_results) {
  cat("\\begin{table}[h]\n")
  cat("\\centering\n")
  cat("\\caption*{Comparison of Excluded vs. Included Encounters}\n")
  cat("\\begin{tabular}{lccc}\n")
  cat("\\toprule\n")
  cat("& Excluded & Included & p-value \\\\\n")
  cat("& Encounters & (Analytical Sample) & \\\\\n")
  cat("\\midrule\n")
  cat("\\textit{Demographics} \\\\\n")
  
  for (i in 1:nrow(comp_results)) {
    row <- comp_results[i, ]
    
    # Format excluded column
    if (row$Excluded_SD == "") {
      exc_col <- row$Excluded_Mean
    } else {
      exc_col <- paste0(row$Excluded_Mean, " ", row$Excluded_SD)
    }
    
    # Format included column
    if (row$Included_SD == "") {
      inc_col <- row$Included_Mean
    } else {
      inc_col <- paste0(row$Included_Mean, " ", row$Included_SD)
    }
    
    # Format p-value with significance stars
    p_col <- paste0(row$P_Value, row$Significant)
    
    cat(row$Variable, "&", exc_col, "&", inc_col, "&", p_col, "\\\\\n")
  }
  
  cat("\\midrule\n")
  cat("N &", nrow(excluded), "&", nrow(included), "& \\\\\n")
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

# Generate LaTeX table
create_latex_table(comparison_results)

