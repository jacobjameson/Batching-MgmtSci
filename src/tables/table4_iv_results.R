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
      "~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + EXPERIENCE + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX + capacity_level |
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
      "~ batched + tachycardic + tachypneic + hrs_in_shift + febrile + hypotensive + age + EXPERIENCE + LAB_PERF |
       dayofweekt + month_of_year + race +  complaint_esi + GENDER + PROVIDER_SEX + capacity_level"
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

run_models(final, "ln_disp_time")
run_models(final, "ln_ED_LOS")
run_models(final, "imgTests")
run_models(final, "RTN_72_HR_ADMIT")
#run_models(final, "RTN_72_HR")
run_models(final, "PLAIN_XRAY")
run_models(final, "US_PERF")
run_models(final, "NON_CON_CT_PERF")
run_models(final, "CON_CT_PERF")

run_models(final, "admit")





run_models <- function(data, y_var) {
  
  model_0 <- feols(
    as.formula(paste(y_var, "~ 0 | dayofweekt + month_of_year | batched ~ batch.tendency")),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  model_1 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + age |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level |
       batched ~ batch.tendency"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  model_2 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + PROVIDER_SEX + EXPERIENCE + age  |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level |
       batched ~ batch.tendency"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  model_3 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + PROVIDER_SEX + EXPERIENCE + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level |
       batched ~ batch.tendency"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  model_4 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + PROVIDER_SEX + EXPERIENCE + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level |
       batched ~ batch.tendency"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  model_5 <- feols(
    as.formula(paste(
      y_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + PROVIDER_SEX + EXPERIENCE + admit.tendency + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + capacity_level |
       batched ~ batch.tendency"
    )),
    cluster = ~ED_PROVIDER,
    data = data
  )
  
  # --- 6. Collect coefficients and SEs safely ---
  extract_coef <- function(model, term) ifelse(term %in% names(coef(model)), coef(model)[term], NA)
  extract_se   <- function(model, term) ifelse(term %in% names(se(model)),   se(model)[term],   NA)
  
  results <- data.frame(
    Model = c("2SLS no controls", "2SLS Patient", "2SLS Patient + Provider", 
              "2SLS Patient + Provider + Lab", "2SLS Patient + Provider + Lab + hrs", "2SLS Patient + Provider + Lab + hrs+ admit.tend"),
    Coefficient = c(
      extract_coef(model_0, "fit_batched"),
      extract_coef(model_1, "fit_batched"),
      extract_coef(model_2, "fit_batched"),
      extract_coef(model_3, "fit_batched"),
      extract_coef(model_4, "fit_batched"),
      extract_coef(model_5, "fit_batched")
    ),
    SE = c(
      extract_se(model_0, "fit_batched"),
      extract_se(model_1, "fit_batched"),
      extract_se(model_2, "fit_batched"),
      extract_se(model_3, "fit_batched"),
      extract_se(model_4, "fit_batched"),
      extract_se(model_5, "fit_batched")
    )
  )
  
  print(results)
  
  # --- 7. Display regression table ---
  return(
    etable(model_0, model_1, model_2, model_3, model_4,model_5,
           cluster = "ED_PROVIDER", se = "cluster",
           keep = "batched")
  )
}

run_models(final, "ln_disp_time")
run_models(final, "ln_ED_LOS")
run_models(final, "imgTests")
run_models(final, "RTN_72_HR_ADMIT")
run_models(final, "RTN_72_HR")
run_models(final, "admit")


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

etable(m1, m2, m3, m4, m5, cluster = "ED_PROVIDER",
       se = "cluster", keep = c("batch.tendency"))
