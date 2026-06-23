library(tidyverse)
library(fixest)
library(ManyIV)
library(marginaleffects)
library(sandwich)

final <- final %>%
  
  mutate(
    
    stable_admit = if_else(downgrade == 0 & admit == 1, 1, 0, missing = 0),
    
    unstable_admit = if_else(downgrade == 1 & admit == 1, 1, 0, missing = 0)
    
  )

#============================================================
# 0. Controls
#============================================================

necessary_fe <- c(
  "dayofweekt",
  "month_of_year"
)

precision_fe <- c(
  "complaint_esi",
  "race",
  "GENDER",
  "PROVIDER_SEX",
  "capacity_level"
)

precision_controls <- c(
  "tachycardic",
  "tachypneic",
  "febrile",
  "hypotensive",
  "lab.tendency",
  "admit.tendency",
  "age",
  "EXPERIENCE",
  "hrs_in_shift"
)

all_fe <- c(necessary_fe, precision_fe)

fe_rhs <- function(x) paste(x, collapse = " + ")
factor_rhs <- function(x) paste0("factor(", x, ")", collapse = " + ")
linear_rhs <- function(x) paste(x, collapse = " + ")

#============================================================
# 1. Clean model data for one outcome
#============================================================

prep_model_data <- function(data, y_var) {
  
  needed_vars <- unique(c(
    y_var,
    "batched",
    "batch.tendency",
    "ED_PROVIDER",
    necessary_fe,
    precision_fe,
    precision_controls
  ))
  
  needed_vars <- needed_vars[needed_vars %in% names(data)]
  
  data_model <- data %>%
    dplyr::select(dplyr::all_of(needed_vars)) %>%
    tidyr::drop_na()
  
  factor_vars <- c(necessary_fe, precision_fe, "ED_PROVIDER")
  factor_vars <- factor_vars[factor_vars %in% names(data_model)]
  
  data_model <- data_model %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(factor_vars), as.factor)) %>%
    droplevels()
  
  one_level_vars <- factor_vars[
    sapply(data_model[factor_vars], function(x) nlevels(x) < 2)
  ]
  
  if (length(one_level_vars) > 0) {
    message("Dropping one-level factor variables: ", paste(one_level_vars, collapse = ", "))
    
    data_model <- data_model %>%
      dplyr::select(-dplyr::all_of(one_level_vars))
  }
  
  data_model
}

#============================================================
# 2. 2SLS
#============================================================

run_2sls <- function(data, y_var) {
  
  data_model <- prep_model_data(data, y_var)
  
  
  fe_available <- all_fe[all_fe %in% names(data_model)]
  controls_available <- precision_controls[precision_controls %in% names(data_model)]
  
  model <- feols(
    as.formula(paste0(
      y_var, " ~ ",
      linear_rhs(controls_available),
      " | ",
      fe_rhs(fe_available),
      " | batched ~ batch.tendency"
    )),
    data = data_model,
    vcov = "HC1"
  )
  
  estimate <- unname(coef(model)["fit_batched"])
  se_val <- unname(se(model)["fit_batched"])
  p_val <- 2 * pnorm(-abs(estimate / se_val))
  
  tibble(
    outcome = y_var,
    estimator = "2SLS",
    estimate = estimate,
    se = se_val,
    p = p_val,
    n = nobs(model),
    first_stage_f = tryCatch(
      as.numeric(fitstat(model, "ivf")[[1]][["stat"]]),
      error = function(e) NA_real_
    )
  )
}

#============================================================
# 3. 2SRI with AME
#============================================================

run_2sri <- function(data, y_var, outcome_type = c("continuous", "binary", "count"), binary_link = "probit") {
  
  outcome_type <- match.arg(outcome_type)
  
  data_model <- prep_model_data(data, y_var)
  
  fe_available <- all_fe[all_fe %in% names(data_model)]
  controls_available <- precision_controls[precision_controls %in% names(data_model)]
  
  # First stage
  first_stage <- feols(
    as.formula(paste0(
      "batched ~ batch.tendency + ",
      linear_rhs(controls_available),
      " | ",
      fe_rhs(fe_available)
    )),
    data = data_model,
    vcov = "HC1"
  )
  
  data_model$cf_resid <- resid(first_stage)
  
  # Second stage
  rhs <- paste(
    c(
      "batched",
      "cf_resid",
      controls_available,
      paste0("factor(", fe_available, ")")
    ),
    collapse = " + "
  )
  
  form <- as.formula(paste0(y_var, " ~ ", rhs))
  
  if (outcome_type == "continuous") {
    model <- lm(form, data = data_model)
  }
  
  if (outcome_type == "binary") {
    model <- glm(
      form,
      data = data_model,
      family = binomial(link = binary_link),
      control = glm.control(maxit = 100)
    )
  }
  
  if (outcome_type == "count") {
    model <- glm(
      form,
      data = data_model,
      family = poisson(link = "log"),
      control = glm.control(maxit = 100)
    )
  }
  
  robust_vcov <- sandwich::vcovHC(model, type = "HC1")
  
  ame <- tryCatch(
    marginaleffects::avg_comparisons(
      model,
      variables = "batched",
      vcov = robust_vcov
    ),
    error = function(e) NULL
  )
  
  cf_resid_coef <- unname(coef(model)["cf_resid"])
  cf_resid_se <- unname(sqrt(diag(robust_vcov))["cf_resid"])
  
  cf_resid_p <- 2 * pnorm(
    -abs(cf_resid_coef / cf_resid_se)
  )
  
  if (is.null(ame)) {
    ame_est <- NA_real_
    ame_se <- NA_real_
    ame_p <- NA_real_
  } else {
    ame_df <- as.data.frame(ame)
    ame_est <- ame_df$estimate[1]
    ame_se <- ame_df$std.error[1]
    ame_p <- ame_df$p.value[1]
  }
  
  tibble(
    outcome = y_var,
    estimator = "2SRI",
    index_estimate = unname(coef(model)["batched"]),
    index_se = unname(sqrt(diag(robust_vcov))["batched"]),
    ame = ame_est,
    ame_se = ame_se,
    ame_p = ame_p,
    cf_resid_coef = cf_resid_coef,
    cf_resid_se = cf_resid_se,
    cf_resid_p = cf_resid_p,
    n = nobs(model)
  )
}

#============================================================
# 4. UJIVE
#============================================================

run_ujive <- function(data, y_var, include_physician_chars = TRUE) {
  
  data_model <- prep_model_data(data, y_var)
  
  fe_available <- all_fe[all_fe %in% names(data_model)]
  
  ujive_patient_controls <- c(
    "tachycardic",
    "tachypneic",
    "febrile",
    "hypotensive",
    "age",
    "hrs_in_shift"
  )
  
  ujive_physician_controls <- c(
    "EXPERIENCE"
  )
  
  if (include_physician_chars) {
    ujive_controls <- c(
      ujive_patient_controls,
      ujive_physician_controls
    )
  } else {
    ujive_controls <- ujive_patient_controls
  }
  
  ujive_controls <- ujive_controls[ujive_controls %in% names(data_model)]
  
  rhs_controls <- paste(
    c(
      paste0("factor(", fe_available, ")"),
      ujive_controls
    ),
    collapse = " + "
  )
  
  model <- ujive(
    as.formula(paste0(
      y_var,
      " ~ batched + ",
      rhs_controls,
      " | factor(ED_PROVIDER) + ",
      rhs_controls
    )),
    data = data_model
  )
  
  estimate <- unname(model$estimate["ujive", "estimate"])
  se_val <- unname(model$estimate["ujive", "se_hte"])
  p_val <- 2 * pnorm(-abs(estimate / se_val))
  
  tibble(
    outcome = y_var,
    estimator = ifelse(
      include_physician_chars,
      "UJIVE + physician chars",
      "UJIVE patient controls only"
    ),
    estimate = estimate,
    se = se_val,
    p = p_val,
    n = nrow(data_model)
  )
}

# Continuous outcomes
run_2sls(final, "ln_ED_LOS")
run_2sri(final, "ln_ED_LOS", outcome_type = "continuous")
run_ujive(final, "ln_ED_LOS", include_physician_chars = FALSE)

run_2sls(final, "ln_disp_time")
run_2sri(final, "ln_disp_time", outcome_type = "continuous")
run_ujive(final, "ln_disp_time",  include_physician_chars = FALSE)

# Count outcome
run_2sls(final, "imgTests")
run_2sri(final, "imgTests", outcome_type = "count")
run_ujive(final, "imgTests", include_physician_chars = FALSE)

# Binary outcomes
run_2sls(final, "admit")
run_2sri(final, "admit", outcome_type = "binary")
run_ujive(final, "admit", include_physician_chars = FALSE)

run_2sls(final, "RTN_72_HR_ADMIT")
run_2sri(final, "RTN_72_HR_ADMIT", outcome_type = "binary")
run_ujive(final, "RTN_72_HR_ADMIT", include_physician_chars = FALSE)




run_fe_ols <- function(data, y_var) {
  
  data_model <- prep_model_data(data, y_var)
  
  model <- feols(
    as.formula(paste0(
      y_var, " ~ batched + ",
      linear_rhs(precision_controls),
      " | ED_PROVIDER + ",
      fe_rhs(all_fe)
    )),
    data = data_model,
    vcov = "HC1"
  )
  
  tibble(
    outcome = y_var,
    estimator = "Physician FE OLS",
    estimate = unname(coef(model)["batched"]),
    se = unname(se(model)["batched"]),
    p = 2 * pnorm(-abs(unname(coef(model)["batched"]) / unname(se(model)["batched"]))),
    n = nobs(model)
  )
}

run_fe_ols(final, "ln_ED_LOS")
run_fe_ols(final, "ln_disp_time")
run_fe_ols(final, "imgTests")
run_fe_ols(final, "admit")
run_fe_ols(final, "RTN_72_HR_ADMIT")


#============================================================
# Dobbie-style complier reweighting for physician-FE OLS
#============================================================

final <- final %>%
  mutate(
    normal_vitals = if_else(
      tachycardic == 0 & tachypneic == 0 & febrile == 0 & hypotensive == 0,
      1, 0
    ),
    labs_ordered = LAB_PERF,
    complaint_complier_group = case_when(
      CHIEF_COMPLAINT %in% c(
        "Falls, MVA, Assaults, and Trauma",
        "Neurological Issue"
      ) ~ "high",
      TRUE ~ "low"
    ),
    cell = paste(normal_vitals, labs_ordered, complaint_complier_group, sep = "_")
  )

calc_complier_share_in_cell <- function(data, cutoff = 0.01) {
  
  if (nrow(data) < 100) return(NA_real_)
  
  z_bar <- quantile(final$batch.tendency, 1 - cutoff, na.rm = TRUE)
  z_underbar <- quantile(final$batch.tendency, cutoff, na.rm = TRUE)
  
  lm_fit <- lm(batched ~ batch.tendency, data = data)
  
  alpha_1 <- coef(lm_fit)["batch.tendency"]
  
  pi_c <- as.numeric(alpha_1 * (z_bar - z_underbar))
  
  if (is.na(pi_c) || pi_c < 0) return(NA_real_)
  
  pi_c
}

pi_c_overall <- calc_complier_share_in_cell(final)

cell_stats <- final %>%
  group_by(cell) %>%
  group_modify(~ tibble(
    n_cell = nrow(.x),
    sample_share = nrow(.x) / nrow(final),
    complier_share = calc_complier_share_in_cell(.x)
  )) %>%
  ungroup() %>%
  mutate(
    complier_weight = complier_share / pi_c_overall
  )

print(cell_stats)

final_weighted <- final %>%
  left_join(
    cell_stats %>% dplyr::select(cell, complier_weight),
    by = "cell"
  ) %>%
  filter(!is.na(complier_weight), complier_weight > 0)

run_complier_weighted_fe_ols <- function(data, y_var) {
  
  model <- feols(
    as.formula(paste0(
      y_var,
      " ~ batched + tachycardic + tachypneic + febrile + hypotensive + ",
      "age + hrs_in_shift + capacity_level + lab.tendency + admit.tendency | ",
      "ED_PROVIDER + dayofweekt + month_of_year + complaint_esi + race + GENDER"
    )),
    weights = ~ complier_weight,
    vcov = "HC1",
    data = data
  )
  
  estimate <- unname(coef(model)["batched"])
  se_val <- unname(se(model)["batched"])
  p_val <- 2 * pnorm(-abs(estimate / se_val))
  
  tibble(
    outcome = y_var,
    estimator = "Complier-weighted physician FE OLS",
    estimate = estimate,
    se = se_val,
    p = p_val,
    n = nobs(model)
  )
}

run_complier_weighted_fe_ols(final_weighted, "ln_ED_LOS")
run_complier_weighted_fe_ols(final_weighted, "ln_disp_time")
run_complier_weighted_fe_ols(final_weighted, "imgTests")
run_complier_weighted_fe_ols(final_weighted, "admit")
run_complier_weighted_fe_ols(final_weighted, "RTN_72_HR_ADMIT")
run_complier_weighted_fe_ols(final_weighted, "unstable_admit")
