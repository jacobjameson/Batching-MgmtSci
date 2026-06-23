################################################################################
#-------------------------------------------------------------------------------
# Reduced Form
#-------------------------------------------------------------------------------
################################################################################

sink('outputs/tables/Table3.txt')

# ------------------------------------------------------------------------------

rf_model_disp <- feols(
  ln_disp_time ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level + # ED variables
    lab.tendency + admit.tendency + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = final, vcov = "HC1")

rf_model_los <- feols(
  ln_ED_LOS ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level +  # ED variables
    lab.tendency + admit.tendency + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
   data = final, vcov = "HC1")

rf_model_img <- feols(
  imgTests ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level  + # ED variables
    lab.tendency + admit.tendency  + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
   data = final, vcov = "HC1")

rf_model_ra <- feols(
  RTN_72_HR_ADMIT ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level  + # ED variables
    lab.tendency + admit.tendency  + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = final, vcov = "HC1")

# ------------------------------------------------------------------------------

etable(rf_model_disp, rf_model_los,rf_model_img, rf_model_ra, 
       keep = c("batch.tendency"))


quantile(data$batch.tendency, probs = seq(0, 1, 0.1))[c(2,10)]

# Calculate F-statistics for reduced-form models
wald_rf_1 <- wald(rf_model_disp)
wald_rf_2 <- wald(rf_model_los)
wald_rf_3 <- wald(rf_model_img)
wald_rf_4 <- wald(rf_model_ra)


print(paste('ln_disp_time mean:', mean(final$ln_disp_time)))
print(paste('ln_disp_time sd:', sd(final$ln_disp_time)))

print(paste('ln_ED_LOS mean:', mean(final$ln_ED_LOS)))
print(paste('ln_ED_LOS sd:', sd(final$ln_ED_LOS)))

print(paste('imgTests mean:', mean(final$imgTests)))
print(paste('imgTests sd:', sd(final$imgTests)))

print(paste('RTN_72_HR_ADMIT mean:', mean(final$RTN_72_HR_ADMIT)))
print(paste('RTN_72_HR_ADMIT sd:', sd(final$RTN_72_HR_ADMIT)))

sink


################################################################################
#-------------------------------------------------------------------------------
# Reduced Form + Nonlinear Reduced Form AMEs
#-------------------------------------------------------------------------------
################################################################################

library(fixest)
library(dplyr)
library(tibble)
library(purrr)
library(marginaleffects)
library(sandwich)
library(knitr)

sink("outputs/tables/Table3.txt")

# ------------------------------------------------------------------------------
# Scaling: 10th to 90th percentile of physician batch tendency
# ------------------------------------------------------------------------------

bt_q <- quantile(final$batch.tendency, probs = c(0.10, 0.90), na.rm = TRUE)
bt_delta <- unname(bt_q[2] - bt_q[1])

print(bt_q)
print(paste("10th to 90th percentile delta:", round(bt_delta, 4)))

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ ".",
    TRUE ~ ""
  )
}

fmt_est <- function(est, se, p, digits = 3) {
  ifelse(
    is.na(est) | is.na(se),
    "",
    paste0(
      sprintf(paste0("%.", digits, "f"), est),
      stars(p),
      " (",
      sprintf(paste0("%.", digits, "f"), se),
      ")"
    )
  )
}

fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))
}

get_fixest <- function(model, term) {
  ct <- coeftable(model)
  if (!term %in% rownames(ct)) {
    return(list(coef = NA_real_, se = NA_real_, p = NA_real_))
  }
  p_col <- grep("Pr", colnames(ct), value = TRUE)[1]
  list(
    coef = unname(ct[term, "Estimate"]),
    se = unname(ct[term, "Std. Error"]),
    p = unname(ct[term, p_col])
  )
}

get_glm_robust <- function(model, term) {
  V <- sandwich::vcovHC(model, type = "HC1")
  b <- coef(model)
  
  if (!term %in% names(b)) {
    return(list(coef = NA_real_, se = NA_real_, p = NA_real_))
  }
  
  est <- unname(b[term])
  se <- unname(sqrt(diag(V))[term])
  z <- est / se
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  
  list(coef = est, se = se, p = p)
}

# ------------------------------------------------------------------------------
# Shared controls
# ------------------------------------------------------------------------------

PRECISION <- paste(
  "tachycardic",
  "tachypneic",
  "febrile",
  "hypotensive",
  "age",
  "capacity_level",
  "lab.tendency",
  "admit.tendency",
  "EXPERIENCE",
  "hrs_in_shift",
  "PROVIDER_SEX",
  sep = " + "
)

FE_FIXEST <- paste(
  "dayofweekt",
  "month_of_year",
  "complaint_esi",
  "race",
  "GENDER",
  sep = " + "
)

FE_GLM <- paste(
  "factor(dayofweekt)",
  "factor(month_of_year)",
  "factor(complaint_esi)",
  "factor(race)",
  "factor(GENDER)",
  sep = " + "
)

GLM_CONTROLS <- paste(
  "tachycardic",
  "tachypneic",
  "febrile",
  "hypotensive",
  "age",
  "factor(capacity_level)",
  "lab.tendency",
  "admit.tendency",
  "EXPERIENCE",
  "hrs_in_shift",
  "factor(PROVIDER_SEX)",
  FE_GLM,
  sep = " + "
)

# ------------------------------------------------------------------------------
# Function to run reduced form models
# ------------------------------------------------------------------------------

run_reduced_form <- function(data,
                             y_var,
                             outcome_label,
                             outcome_type = c("continuous", "binary", "count"),
                             link = "probit") {
  
  outcome_type <- match.arg(outcome_type)
  
  cat("\n=====================================================\n")
  cat("Reduced form for:", y_var, "\n")
  cat("=====================================================\n")
  
  data_model <- as.data.frame(data)
  
  # Linear reduced form with absorbed FEs
  rf_linear <- feols(
    as.formula(paste0(
      y_var, " ~ batch.tendency + ", PRECISION, " | ", FE_FIXEST
    )),
    data = data_model,
    vcov = "HC1"
  )
  
  lin <- get_fixest(rf_linear, "batch.tendency")
  
  # Nonlinear / corresponding functional form
  rf_formula_glm <- as.formula(paste0(
    y_var, " ~ batch.tendency + ", GLM_CONTROLS
  ))
  
  if (outcome_type == "continuous") {
    rf_nl <- lm(rf_formula_glm, data = data_model)
  } else if (outcome_type == "binary") {
    rf_nl <- glm(
      rf_formula_glm,
      data = data_model,
      family = binomial(link = link),
      control = glm.control(maxit = 100)
    )
  } else if (outcome_type == "count") {
    rf_nl <- glm(
      rf_formula_glm,
      data = data_model,
      family = poisson(link = "log"),
      control = glm.control(maxit = 100)
    )
  }
  
  nl <- get_glm_robust(rf_nl, "batch.tendency")
  
  # AME for nonlinear reduced form
  if (outcome_type == "continuous") {
    # For linear continuous model, AME is just the coefficient
    ame_est <- nl$coef
    ame_se <- nl$se
    ame_p <- nl$p
  } else {
    V_nl <- sandwich::vcovHC(rf_nl, type = "HC1")
    
    ame_obj <- tryCatch(
      marginaleffects::avg_slopes(
        rf_nl,
        variables = "batch.tendency",
        vcov = V_nl
      ),
      error = function(e) {
        message("AME failed for ", y_var, ": ", e$message)
        NULL
      }
    )
    
    if (is.null(ame_obj)) {
      ame_est <- NA_real_
      ame_se <- NA_real_
      ame_p <- NA_real_
    } else {
      ame_est <- ame_obj$estimate[1]
      ame_se <- ame_obj$std.error[1]
      ame_p <- ame_obj$p.value[1]
    }
  }
  
  # Scale by 10th to 90th percentile difference in batch tendency
  lin_scaled <- lin$coef * bt_delta
  lin_scaled_se <- lin$se * bt_delta
  
  nl_scaled <- nl$coef * bt_delta
  nl_scaled_se <- nl$se * bt_delta
  
  ame_scaled <- ame_est * bt_delta
  ame_scaled_se <- ame_se * bt_delta
  
  # Means
  y_mean <- mean(data_model[[y_var]], na.rm = TRUE)
  y_sd <- sd(data_model[[y_var]], na.rm = TRUE)
  
  tibble(
    outcome = y_var,
    outcome_label = outcome_label,
    outcome_type = outcome_type,
    n_linear = nobs(rf_linear),
    n_nonlinear = nobs(rf_nl),
    mean_y = y_mean,
    sd_y = y_sd,
    
    rf_linear_coef = lin$coef,
    rf_linear_se = lin$se,
    rf_linear_p = lin$p,
    rf_linear_scaled = lin_scaled,
    rf_linear_scaled_se = lin_scaled_se,
    
    rf_nl_index_coef = nl$coef,
    rf_nl_index_se = nl$se,
    rf_nl_index_p = nl$p,
    rf_nl_index_scaled = nl_scaled,
    rf_nl_index_scaled_se = nl_scaled_se,
    
    rf_nl_ame = ame_est,
    rf_nl_ame_se = ame_se,
    rf_nl_ame_p = ame_p,
    rf_nl_ame_scaled = ame_scaled,
    rf_nl_ame_scaled_se = ame_scaled_se,
    
    adj_r2_linear = fitstat(rf_linear, "ar2")[[1]]
  )
}

# ------------------------------------------------------------------------------
# Outcomes for Table 3
# ------------------------------------------------------------------------------

rf_specs <- tribble(
  ~outcome,            ~label,                           ~type,
  "ln_disp_time",      "Log time to disposition",         "continuous",
  "ln_ED_LOS",         "Log LOS",                         "continuous",
  "imgTests",          "Number of distinct imaging tests","count",
  "RTN_72_HR_ADMIT",   "72hr return with admission",      "binary"
) %>%
  filter(outcome %in% names(final))

# ------------------------------------------------------------------------------
# Run models
# ------------------------------------------------------------------------------

rf_results <- pmap_dfr(
  list(rf_specs$outcome, rf_specs$label, rf_specs$type),
  ~ run_reduced_form(
    data = final,
    y_var = ..1,
    outcome_label = ..2,
    outcome_type = ..3,
    link = "probit"
  )
)

# ------------------------------------------------------------------------------
# Clean table output
# ------------------------------------------------------------------------------

rf_clean <- rf_results %>%
  mutate(
    `Mean dependent variable` = paste0(
      sprintf("%.3f", mean_y),
      " (",
      sprintf("%.3f", sd_y),
      ")"
    ),
    `Linear RF: batch tendency` = fmt_est(
      rf_linear_coef,
      rf_linear_se,
      rf_linear_p
    ),
    `Linear RF: 10th to 90th pct.` = fmt_num(rf_linear_scaled, 4),
    `Nonlinear RF: index coef.` = fmt_est(
      rf_nl_index_coef,
      rf_nl_index_se,
      rf_nl_index_p
    ),
    `Nonlinear RF: AME` = fmt_est(
      rf_nl_ame,
      rf_nl_ame_se,
      rf_nl_ame_p
    ),
    `Nonlinear RF: AME, 10th to 90th pct.` = fmt_num(rf_nl_ame_scaled, 4),
    `Adj. R2` = sprintf("%.4f", adj_r2_linear)
  ) %>%
  dplyr::select(
    Outcome = outcome_label,
    `Mean dependent variable`,
    `Linear RF: batch tendency`,
    `Linear RF: 10th to 90th pct.`,
    `Nonlinear RF: index coef.`,
    `Nonlinear RF: AME`,
    `Nonlinear RF: AME, 10th to 90th pct.`,
    `Adj. R2`,
    Observations = n_linear
  )

print(rf_clean, n = Inf, width = Inf)

# ------------------------------------------------------------------------------
# Original etable-style output for linear reduced form
# ------------------------------------------------------------------------------

rf_linear_models <- map(
  rf_specs$outcome,
  ~ feols(
    as.formula(paste0(
      .x, " ~ batch.tendency + ", PRECISION, " | ", FE_FIXEST
    )),
    data = final,
    vcov = "HC1"
  )
)

etable(
  rf_linear_models,
  keep = c("batch.tendency")
)


# ------------------------------------------------------------------------------
# Diagnostics and descriptive stats
# ------------------------------------------------------------------------------

cat("\n\nBatch tendency 10th percentile:", bt_q[1], "\n")
cat("Batch tendency 90th percentile:", bt_q[2], "\n")
cat("Batch tendency delta:", bt_delta, "\n\n")

for (v in rf_specs$outcome) {
  cat(v, "mean:", mean(final[[v]], na.rm = TRUE), "\n")
  cat(v, "sd:", sd(final[[v]], na.rm = TRUE), "\n\n")
}


sink()