library(fixest)

run_models <- function(data, y_var) {
  
  # --- 1. Summary stats for sequenced (batched == 0) ---
  mean_batched_0 <- mean(data[[y_var]][data$batched == 0], na.rm = TRUE)
  sd_batched_0   <- sd(data[[y_var]][data$batched == 0], na.rm = TRUE)
  
  # --- 2. 2SLS without controls ---
  model_2 <- feols(
    as.formula(paste(y_var, "~ 0 | dayofweekt + month_of_year | batched ~ batch.tendency")),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # --- 3. 2SLS with controls ---
  model_3 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + EXPERIENCE + PROVIDER_SEX + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level |
       batched ~ batch.tendency"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # --- 4. OLS without controls ---
  model_4 <- feols(
    as.formula(paste(y_var, "~ batched | dayofweekt + month_of_year")),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # --- 5. OLS with controls ---
  model_5 <- feols(
    as.formula(paste(
      y_var,
      "~ batched + tachycardic + tachypneic + febrile + hypotensive + EXPERIENCE + PROVIDER_SEX + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # --- 6. Collect coefficients and SEs safely ---
  extract_coef <- function(model, term) ifelse(term %in% names(coef(model)), coef(model)[term], NA)
  extract_se   <- function(model, term) ifelse(term %in% names(se(model)),   se(model)[term],   NA)
  
  results <- data.frame(
    Model = c("Mean (Sequenced)", "SD (Sequenced)", 
              "2SLS No Controls", "2SLS With Controls", 
              "OLS No Controls", "OLS With Controls"),
    Coefficient = c(
      mean_batched_0,
      sd_batched_0,
      extract_coef(model_2, "fit_batched"),
      extract_coef(model_3, "fit_batched"),
      extract_coef(model_4, "batched"),
      extract_coef(model_5, "batched")
    ),
    SE = c(
      NA, NA,
      extract_se(model_2, "fit_batched"),
      extract_se(model_3, "fit_batched"),
      extract_se(model_4, "batched"),
      extract_se(model_5, "batched")
    )
  )
  
  print(results)
  
  # --- 7. Display regression table ---
  return(
    etable(model_2, model_3, model_4, model_5,
           cluster = "ED_PROVIDER", se = "cluster",
           keep = "batched")
  )
}

run_models(data, "ln_disp_time")
run_models(data, "ln_ED_LOS")
run_models(data, "imgTests")
run_models(data, "RTN_72_HR_ADMIT")
run_models(data, "RTN_72_HR")