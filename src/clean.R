#=========================================================================
# Purpose: Main R file for Preparing + Cleaning Mayo Data
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls())

# libraries
library(tidyverse)
library(stringr)
library(lfe)
library(lubridate)
library(ggthemes)

source("src/utils.R")


path <- '~/Sue Goldie Dropbox/Jacob Jameson/Batch vs sequential testing/Data/'
data <- read.csv(paste0(path, 'deidentified_FINAL.csv'))

data[c('PLAIN_XRAY', 'US_PERF', 'NON_CON_CT_PERF', 
       'CON_CT_PERF', 'LAB_PERF')] <- lapply(
         data[c('PLAIN_XRAY', 'US_PERF', 'NON_CON_CT_PERF', 
                'CON_CT_PERF', 'LAB_PERF')], 
         function(x) ifelse(x == 'Y', 1, 0))

data$nEDTests = rowSums(data[c('PLAIN_XRAY', 'US_PERF',
                               'NON_CON_CT_PERF', 'CON_CT_PERF', 'LAB_PERF')])

data$imgTests = data$nEDTests - data$LAB_PERF

rel_cols <- grep("_REL$", names(data), value = TRUE)

for (col in rel_cols) {
  data <- data %>%
    separate(col, into = c(paste0(col, "_hrs"), paste0(col, "_mins")),
             sep = ":", fill = "right", remove = TRUE) %>%
    mutate(
      across(all_of(c(paste0(col, "_hrs"), paste0(col, "_mins"))), as.numeric),
      !!col := get(paste0(col, "_hrs")) * 60 + get(paste0(col, "_mins"))
    ) %>%
    dplyr::select(-matches(paste0(col, "_hrs|", col, "_mins")))
}


data$admit <- ifelse(data$ED_DISPOSITION == 'Admit', 1, 0)

# create a dispo time that is the rowwise min of the three possible dispo times
data$dispo_time <- pmin(data$ED_DISCHARGE_DT_REL,
                        data$ADMIT_OBS_ORD_DTTM_REL,
                        data$ADMIT_INP_ORD_DTTM_REL,
                        na.rm = TRUE)


# create time to disposition variable
data$time_to_dispo <- data$dispo_time - data$ARRIVAL_DTTM_REL
data$time_to_dispo <- ifelse(data$time_to_dispo < 0, NA, data$time_to_dispo)
data$time_to_dispo <- ifelse(data$time_to_dispo > data$ED_LOS, 
                             data$ED_LOS, data$time_to_dispo)

# create waiting time variable
data$wait_time <- data$PATIENT_ROOMED_IN_ED_REL - data$ARRIVAL_DTTM_REL
data$wait_time <- ifelse(data$wait_time <= 0, 1, data$wait_time)

calculate_waiting_patients <- function(time, arrivals, first_contacts) {
  sum(arrivals <= time & first_contacts > time, na.rm = TRUE)
}

data <- data %>%
  mutate(
    # number of patients waiting at this patient's arrival time
    patients_waiting = map_dbl(
      ARRIVAL_DTTM_REL,
      ~calculate_waiting_patients(.x, ARRIVAL_DTTM_REL, FIRST_CONTACT_DTTM_REL)
    ),
    # classify capacity state
    capacity_level = case_when(
      wait_time > 90 | patients_waiting > 20 ~ "Major Overcapacity",
      (wait_time >= 21 & wait_time <= 90) | patients_waiting >= 10 ~ "Minor Overcapacity",
      wait_time < 20 & patients_waiting < 10 ~ "Normal Operations",
      TRUE ~ "Normal Operations"
    )
  )

data <- data %>%
  filter(!is.na(time_to_dispo),
         time_to_dispo > 0,
         ED_LOS <= 1440,
         ED_LOS > 0,
         !is.na(ED_LOS))

#=========================================================================
# Determine batching
#   - criteria: ordered within 5 minutes of each other
#               first tests ordered were in a batch
#=========================================================================

# Vectorized calculation for min_time and max_time
data$min_time <- pmin(data$US_ORDER_DTTM_REL,
                      data$CT_WITHOUT_CONTR_ORDER_DTTM_REL, 
                      data$CT_WITH_CONTR_ORDER_DTTM_REL,
                      data$PLAIN_XRAY_ORDER_DTTM_REL, na.rm = TRUE)

data$max_time <- pmax(data$CT_WITHOUT_CONTR_RESULT_DTTM_REL, 
                      data$CT_WITH_CONTR_RESULT_DTTM_REL, 
                      data$PLAIN_XRAY_RESULT_DTTM_REL, 
                      data$US_RESULT_DTTM_REL, na.rm = TRUE)

data$total_testing_time <- data$max_time - data$min_time
data$treatment_time <- data$time_to_dispo - data$wait_time 
data$treatment_time <- ifelse(data$treatment_time <= 0, 1, data$treatment_time)

# determine turnaround times for each test
data$CT_WITHOUT_CONTR_TAT <- ifelse(!is.na(data$CT_WITHOUT_CONTR_ORDER_DTTM_REL) & 
                                      !is.na(data$CT_WITHOUT_CONTR_RESULT_DTTM_REL),
                                    data$CT_WITHOUT_CONTR_RESULT_DTTM_REL - data$CT_WITHOUT_CONTR_ORDER_DTTM_REL,
                                    NA)

data$CT_WITH_CONTR_TAT <- ifelse(!is.na(data$CT_WITH_CONTR_ORDER_DTTM_REL) &
                                   !is.na(data$CT_WITH_CONTR_RESULT_DTTM_REL),
                                 data$CT_WITH_CONTR_RESULT_DTTM_REL - data$CT_WITH_CONTR_ORDER_DTTM_REL,
                                 NA)

data$PLAIN_XRAY_TAT <- ifelse(!is.na(data$PLAIN_XRAY_ORDER_DTTM_REL) &
                               !is.na(data$PLAIN_XRAY_RESULT_DTTM_REL),
                             data$PLAIN_XRAY_RESULT_DTTM_REL - data$PLAIN_XRAY_ORDER_DTTM_REL,
                             NA)

data$US_TAT <- ifelse(!is.na(data$US_ORDER_DTTM_REL) &
                        !is.na(data$US_RESULT_DTTM_REL),
                      data$US_RESULT_DTTM_REL - data$US_ORDER_DTTM_REL,
                      NA)

data$LAB_TAT <- ifelse(!is.na(data$LAB_ORDER_DTTM_REL) &
                         !is.na(data$LAB_RESULT_DTTM_REL),
                       data$LAB_RESULT_DTTM_REL - data$LAB_ORDER_DTTM_REL,
                       NA)


cols_of_interest <- c("US_ORDER_DTTM_REL", "CT_WITHOUT_CONTR_ORDER_DTTM_REL",
                      "CT_WITH_CONTR_ORDER_DTTM_REL", "PLAIN_XRAY_ORDER_DTTM_REL")

data[cols_of_interest] <- data[cols_of_interest] - data$min_time


# check for each row how many tests were ordered within 5 minutes of each other
data$batch_count <- rowSums(data[cols_of_interest] >= 0 & data[cols_of_interest] <= 5, na.rm = TRUE)
data$batched <- ifelse(data$batch_count > 1, 1, 0)

#===============================================================================
# Clean other variables

data$PATIENT_RACE <- str_to_lower(data$PATIENT_RACE)

data <- data %>%
  mutate(
    race = case_when(
      str_detect(PATIENT_RACE, "black|african") ~ "black",
      str_detect(PATIENT_RACE, "asian|pacific islander") ~ "asian",
      str_detect(PATIENT_RACE, "native") ~ "native",
      str_detect(PATIENT_RACE, "white") ~ "white",
      str_detect(PATIENT_RACE, "samoan|guamanian|chamorro") ~ "other",
      str_detect(PATIENT_RACE, "unknown|choose not|unable") ~ "unknown",
      TRUE ~ "other"
    )
  )

data$age <- ifelse(
  data$ARRIVAL_AGE_DI == '85+', '85', data$ARRIVAL_AGE_DI
)
data$age <- as.numeric(data$age)

data <- data %>% 
  filter(ARRIVAL_AGE_DI >= 18)

# get IQR median for treatment time
summary(data$treatment_time)
#=========================================================================
# Clean Chief Complaint --------------------------------------------------
#=========================================================================

data$complaint <- data$CHIEF_COMPLAINT

for (i in seq(1,length(complaints))){
  name <- names(complaints[i])
  complaint <- complaints[[i]]
  
  data$CHIEF_COMPLAINT <- ifelse(
    data$CHIEF_COMPLAINT %in% complaint, name, data$CHIEF_COMPLAINT
  )
}

data <- data %>%
  mutate(complaint_esi  = paste(ESI, CHIEF_COMPLAINT),
         complaint_esi = factor(complaint_esi))


#=========================================================================
# Categorize Vital Signs -------------------------------------------------
#=========================================================================

data$tachycardic <- ifelse(
  is.na(data$TRIAGE_PULSE) == F & data$TRIAGE_PULSE > 100, 1, 0
)
data$tachypneic <- ifelse(
  is.na(data$TRIAGE_RR)  == F  & data$TRIAGE_RR > 20, 1, 0
)
data$febrile <- ifelse(
  is.na(data$TRIAGE_TEMP)  == F  & data$TRIAGE_TEMP > 38, 1, 0
)
data$hypotensive <- ifelse(
  is.na(data$TRIAGE_SBP)  == F  & data$TRIAGE_SBP < 90, 1, 0
)


#=========================================================================
# Time -------------------------------------------------
#=========================================================================

data <- data %>%
  mutate(
    rel_minutes_arrival = ARRIVAL_DTTM_REL,
    rel_minutes_depart = rel_minutes_arrival + ED_LOS
  )

# Count number of patients present in ED at each patient's arrival (excluding self)
data$patients_in_ed <- sapply(
  data$rel_minutes_arrival,
  function(arrival_time) {
    sum(data$rel_minutes_arrival <= arrival_time & data$rel_minutes_depart > arrival_time) - 1
  }
)

# Add date and temporal features
data <- data %>%
  mutate(
    actual_date = as.POSIXct("2018-10-06 00:00:00", 
                             tz = "UTC") + minutes(ARRIVAL_DTTM_REL),
    hour_of_day = hour(actual_date),
    is_weekend = if_else(wday(actual_date) %in% c(1, 7), "Weekend", "Weekday"),
    month = month(actual_date, label = TRUE)
  )

source('src/figures/hourly_arrivals.R')
#===============================================================================

data <- data %>%
  mutate(
    time = hour_of_day,
    hour_of_day = cut(
      hour_of_day,
      breaks = c(-Inf, 6, 12, 18, Inf),
      labels = c("0:00–6:00", "6:00–12:00", "12:00–18:00", "18:00–24:00"),
      right = FALSE
    ),
    day_of_week = wday(actual_date, label = TRUE, abbr = FALSE),
    month_of_year = month(actual_date, label = TRUE),
    dayofweekt = paste(day_of_week, hour_of_day)
  )


data <- data %>% 
  mutate(RTN_72_HR = ifelse(RTN_72_HR == 'Y', 1, 0),
         RTN_72_HR_ADMIT = ifelse(RTN_72_HR_ADMIT == 'Y', 1, 0)) 

data$admit = ifelse(data$ED_DISPOSITION == 'Admit', 1, 0)
data$discharge = ifelse(data$ED_DISPOSITION == 'Discharge', 1, 0)
data$observation = ifelse(data$ED_DISPOSITION == 'Observation', 1, 0)

# Create Table 1
source('src/tables/summary_statistics_mayo.R')

#=========================================================================
# Create Final Dataset ---------------------------------------------------
#=========================================================================

data <- data %>%
  mutate(dispo = case_when(
    admit == 1 ~ 'admit',
    discharge == 1 ~ 'discharge',
    observation == 1 ~ 'observation',
    TRUE ~ 'other')) 

# Limit dataset to only physicians that had more than 500 encounters
provider_counts <- table(data$ED_PROVIDER)
providers_less_than_500 <- names(provider_counts[provider_counts < 500])
data <- data[!(data$ED_PROVIDER %in% providers_less_than_500), ]
data$complaint_esi <- paste(data$CHIEF_COMPLAINT, data$ESI)
data <- filter(data, !is.na(data$ESI))
data <- filter(data, !is.na(data$ED_PROVIDER))

# --- Provider experience and sex ---
data <- data %>%
  mutate(
    EXPERIENCE = case_when(
      ED_PROVIDER == 'JUDSON, KURTIS A' ~ 2006,
      ED_PROVIDER == 'KOMARA, JAMES S' ~ 1985,
      ED_PROVIDER == 'RAPPAPORT, DOUGLAS E' ~ 2016,
      ED_PROVIDER == 'MONAS, JESSICA' ~ 2011,
      ED_PROVIDER == 'KELLEY, JAMES' ~ 1994,
      ED_PROVIDER == 'DRECHSEL, KEVIN M' ~ 1998,
      ED_PROVIDER == 'DIETRICH, BOB D' ~ 1999,
      ED_PROVIDER == 'URUMOV, ANDREJ' ~ 2005,
      ED_PROVIDER == 'MACY, CHERYL' ~ 2011,
      ED_PROVIDER == 'BRAND, SHARI I.' ~ 1999,
      ED_PROVIDER == 'TRAUB, STEPHEN J' ~ 1998,
      ED_PROVIDER == 'CHANTLER, EDMUNDO L' ~ 2005,
      ED_PROVIDER == 'HAY-ROE, NEIL' ~ 1987,
      ED_PROVIDER == 'GAUHAROU, ERIK SHAWN' ~ 1999,
      ED_PROVIDER == 'HODGSON, NICOLE R' ~ 2017,
      ED_PROVIDER == 'STEWART, CHRISTOPHER F' ~ 1999,
      ED_PROVIDER == 'TORRES, MARCELLA' ~ 1995,
      ED_PROVIDER == 'MAHER, STEVEN A' ~ 2004,
      ED_PROVIDER == 'ARNOLD, RICKY R' ~ 1997,
      ED_PROVIDER == 'VINCIJANOVIC, LISA M' ~ 2007,
      ED_PROVIDER == 'MORRO, DAVID C' ~ 2003,
      ED_PROVIDER == 'PETRI, ROLAND W' ~ 1988,
      ED_PROVIDER == 'KOZAK, PAUL A' ~ 1993,
      ED_PROVIDER == 'LINDOR, RACHEL A' ~ 2017,
      TRUE ~ NA_real_
    ),
    EXPERIENCE = 2019 - EXPERIENCE,  # years since start
    PROVIDER_SEX = case_when(
      ED_PROVIDER %in% c(
        'JUDSON, KURTIS A','KOMARA, JAMES S','RAPPAPORT, DOUGLAS E','KELLEY, JAMES',
        'DRECHSEL, KEVIN M','DIETRICH, BOB D','URUMOV, ANDREJ','TRAUB, STEPHEN J',
        'CHANTLER, EDMUNDO L','HAY-ROE, NEIL','GAUHAROU, ERIK SHAWN','STEWART, CHRISTOPHER F',
        'MAHER, STEVEN A','ARNOLD, RICKY R','MORRO, DAVID C','PETRI, ROLAND W',
        'KOZAK, PAUL A'
      ) ~ 'M',
      ED_PROVIDER %in% c(
        'MONAS, JESSICA','MACY, CHERYL','BRAND, SHARI I.','HODGSON, NICOLE R',
        'TORRES, MARCELLA','VINCIJANOVIC, LISA M','LINDOR, RACHEL A'
      ) ~ 'F',
      TRUE ~ NA_character_
    )
  )

# --- Convert to date type ---
data <- data %>%
  mutate(date = as.Date(actual_date))

# --- Define shifts based on time gaps > 8 hours ---
data <- data %>%
  arrange(ED_PROVIDER, actual_date) %>%
  group_by(ED_PROVIDER) %>%
  mutate(
    time_diff = as.numeric(difftime(actual_date, lag(actual_date), units = "hours")),
    new_shift = if_else(is.na(time_diff) | time_diff > 8, 1, 0),
    shift_id = cumsum(new_shift)
  ) %>%
  group_by(ED_PROVIDER, shift_id) %>%
  mutate(
    shift_start = min(actual_date),
    hrs_in_shift = as.numeric(difftime(actual_date, shift_start, units = "hours"))
  ) %>%
  ungroup()

# --- Per-day metrics ---
data <- data %>%
  group_by(ED_PROVIDER, date) %>%
  mutate(
    patients_seen = n(),
    patient_order_of_day = row_number(),
    patients_tbs = patients_seen - patient_order_of_day  # patients "to be seen"
  ) %>%
  ungroup()

#############
data$imaging <- ifelse(data$imgTests > 0, 1, 0)

# keep complaints that appear more than 1000 times
complaint_counts <- table(data$CHIEF_COMPLAINT)
complaints_less_than_500 <- names(complaint_counts[complaint_counts < 500])
data <- data[!(data$CHIEF_COMPLAINT %in% complaints_less_than_500), ]

data <- data %>%
  group_by(complaint_esi) %>%
  filter(n() > 1) %>%
  ungroup()


#####

source('src/figures/fig1_batch_rates.R')

### Create the instrument

data$ln_ED_LOS <- log(data$ED_LOS)

data$residual_admit <- resid(
  felm(admit ~ tachycardic + tachypneic + febrile + hypotensive + age 
       | dayofweekt + month_of_year + complaint_esi + race + GENDER |0| ED_PROVIDER, data=data)
)

data$residual_batch <- resid(
  felm(batched ~ tachycardic + tachypneic + febrile + hypotensive + age 
       | dayofweekt + month_of_year + complaint_esi + race + GENDER |0| ED_PROVIDER, data=data)
)

data$residual_los <- resid(
  felm(ln_ED_LOS ~ tachycardic + tachypneic + febrile + hypotensive + age 
       | dayofweekt + month_of_year + complaint_esi + race + GENDER |0| ED_PROVIDER, data=data)
)

# Step 2: get batch tendency for each provider
data <- data %>%
  group_by(ED_PROVIDER) %>%
  mutate(Sum_Resid=sum(residual_batch, na.rm=T),
         batch.tendency = (Sum_Resid - residual_batch) / (n() - 1),
         
         Sum_Resid=sum(residual_admit, na.rm=T),
         admit.tendency = (Sum_Resid - residual_admit) / (n() - 1)) %>%
  ungroup()


rm(list = setdiff(ls(), c("data")))


source('src/figures/fig2_randomization.R')

# FILTER TO MAIN SAMPLE
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
final$ln_treat_time <- log(final$treatment_time)

# if total_testing_time > 1440 (24 hours), set to 1440
final$total_testing_time <- ifelse(final$total_testing_time > 1440, 
                                   1440, final$total_testing_time)

final$ln_total_testing_time <- log(final$total_testing_time)

final$capacity_level <- factor(final$capacity_level,
                              levels = c('Normal Operations', 
                                         'Minor Overcapacity', 
                                         'Major Overcapacity'))


source('src/tables/table2_first_stage.R')
source('src/figures/fig3_firststage.R')
source('src/tables/table3_reduced_form.R')
source('src/tables/placebo.R')
source('src/tables/heterogeneous_analysis.R')




#=========================================================================
# UJIVE Analysis (following Goldsmith-Pinkham et al. 2024)
#=========================================================================

# Install if needed (only run once)
library(ManyIV)

# Create physician ID mapping using the full 'data' dataset
physician_map <- data.frame(
  ED_PROVIDER = unique(data$ED_PROVIDER),
  physician_id = as.numeric(factor(unique(data$ED_PROVIDER)))
)

# Add physician IDs to final dataset
final <- final %>%
  left_join(physician_map, by = "ED_PROVIDER")

# Function to prepare data for UJIVE
prepare_ujive_data <- function(data, outcome_var) {
  # Create physician dummy matrix (instruments)
  # Using physician indicators as instruments (not batch.tendency)
  Z_matrix <- model.matrix(~ factor(physician_id) - 1, data = data)
  # Remove one column to avoid collinearity
  Z_matrix <- Z_matrix[, -1]
  
  # Create control matrix
  W_matrix <- model.matrix(
    ~ factor(dayofweekt) + factor(month_of_year) + 
      factor(complaint_esi) + factor(race) + 
      factor(GENDER) + factor(capacity_level) +
      tachycardic + tachypneic + febrile + hypotensive + LAB_PERF +
      age + EXPERIENCE + factor(PROVIDER_SEX) + hrs_in_shift - 1, 
    data = data
  )
  
  # Prepare outcome and treatment
  y <- data[[outcome_var]]
  x <- data$batched
  
  return(list(y = y, x = x, Z = Z_matrix, W = W_matrix))
}

compare_ujive_2sls <- function(data, outcome_var) {
  cat("\n=== Results for", outcome_var, "===\n")
  
  # Create physician dummies (same as before)
  physicians <- sort(unique(data$ED_PROVIDER))
  physicians <- physicians[-1]
  
  for(p in physicians) {
    data[[paste0("phys_", which(physicians == p))]] <- ifelse(data$ED_PROVIDER == p, 1, 0)
  }
  
  instrument_vars <- paste0("phys_", 1:length(physicians))
  control_vars <- c("tachycardic", "tachypneic", "febrile", "hypotensive",
                    "age", "EXPERIENCE", "hrs_in_shift")
  
  # Build formula
  ujive_formula <- as.formula(paste(
    outcome_var, "~",
    "batched +", paste(control_vars, collapse = " + "), "+",
    "factor(dayofweekt) + factor(month_of_year) + LAB_PERF + factor(complaint_esi) +",
    "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level) |",
    paste(instrument_vars, collapse = " + "), "+",
    paste(control_vars, collapse = " + "), "+",
    "factor(dayofweekt) + factor(month_of_year) + factor(complaint_esi) +",
    "factor(race) + factor(GENDER) + factor(PROVIDER_SEX) + factor(capacity_level)"
  ))
  
  # Run UJIVE
  ujive_result <- ujive(formula = ujive_formula, data = data)
  
  # Extract results from the estimate dataframe
  estimates <- ujive_result$estimate
  
  # Your current 2SLS
  tsls_result <- feols(
    as.formula(paste(
      outcome_var,
      "~ tachycardic + tachypneic + febrile + hypotensive + 
       hrs_in_shift + EXPERIENCE + age + LAB_PERF |
       dayofweekt + month_of_year + complaint_esi + race + GENDER + 
       PROVIDER_SEX + capacity_level |
       batched ~ batch.tendency"
    )),
    vcov = 'HC1',
    data = data
  )
  
  # Compare results
  cat("\n=== Comparison of Estimates ===\n")
  cat("OLS:", estimates["ols", "estimate"], 
      "(SE:", estimates["ols", "se_hte"], ")\n")
  cat("2SLS (physician dummies):", estimates["tsls", "estimate"], 
      "(SE:", estimates["tsls", "se_hte"], ")\n")
  cat("UJIVE:", estimates["ujive", "estimate"], 
      "(SE:", estimates["ujive", "se_hte"], ")\n")
  cat("Your 2SLS (batch.tendency):", coef(tsls_result)["fit_batched"], 
      "(SE:", se(tsls_result)["fit_batched"], ")\n")
  
  cat("\n=== Key Insights ===\n")
  cat("1. UJIVE vs your 2SLS difference:", 
      round(estimates["ujive", "estimate"] - coef(tsls_result)["fit_batched"], 3), "\n")
  cat("2. Finite-sample bias (2SLS vs UJIVE):", 
      round(estimates["tsls", "estimate"] - estimates["ujive", "estimate"], 3), "\n")
  cat("3. Selection bias (OLS vs UJIVE):", 
      round(estimates["ols", "estimate"] - estimates["ujive", "estimate"], 3), "\n")
  
  return(list(
    all_estimates = estimates,
    your_2sls = tsls_result
  ))
}

# Run it
results_ln_ED_LOS <- compare_ujive_2sls(final, "ln_ED_LOS")
results_ln_ED_LOS <- compare_ujive_2sls(final, "admit")
results_ln_ED_LOS <- compare_ujive_2sls(final, "ln_disp_time")
results_ln_ED_LOS <- compare_ujive_2sls(final, "ln_treat_time")



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



###############################################
# UJIVE USING ED_PROVIDER AS MANY INSTRUMENTS
###############################################

library(ManyIV)
library(dplyr)

run_ujive <- function(data, outcome_var) {
  
  # Assignment controls 
  W_assign <- "factor(dayofweekt) + factor(month_of_year)"
  
  # Precision controls
  W_precise <- paste(
    "factor(complaint_esi)",
    "factor(race)",
    "factor(GENDER)",
    "factor(PROVIDER_SEX)",
    "factor(capacity_level)",
    "tachycardic", "tachypneic", "febrile", "hypotensive",
    "LAB_PERF", "age", "EXPERIENCE", "hrs_in_shift",
    sep = " + "
  )
  
  # Build full formula for UJIVE
  ujive_formula <- as.formula(paste0(
    outcome_var, " ~ batched + ",
    W_assign, " + ", W_precise, " | ",
    "factor(ED_PROVIDER) + ",
    W_assign, " + ", W_precise
  ))
  
  # Run UJIVE
  fit <- ujive(
    formula = ujive_formula,
    data = data,
    dropleverage = TRUE  # recommended for many instruments
  )
  
  print(paste("UJIVE for", outcome_var))
  print(fit$estimate)
  
  return(fit)
}

###############################################
# RUN MODELS
###############################################

fit_LOS   <- run_ujive(final, "ln_ED_LOS")
fit_disp  <- run_ujive(final, "ln_disp_time")
fit_treat <- run_ujive(final, "ln_treat_time")
fit_admit <- run_ujive(final, "admit")




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






### CBA

#=========================================================================
# COST-BENEFIT ANALYSIS OF DISCRETIONARY BATCH ORDERING
#=========================================================================

#=========================================================================
# STEP 1: Establish the key parameters
#=========================================================================

# Complier shares (from our analysis above)
pi_c <- 0.203   # 20.3% compliers
pi_a <- 0.067   # 6.7% always-takers
pi_n <- 0.730   # 73% never-takers

# Sample sizes
n_total <- nrow(final)
n_batched <- sum(final$batched)

# Counts by type
n_compliers <- round(n_total * pi_c)
n_always_takers <- round(n_total * pi_a)
n_never_takers <- round(n_total * pi_n)

# Batched compliers = total batched - always takers
# These are the patients whose batching could have been avoided
n_batched_compliers <- n_batched - n_always_takers

cat("=== STEP 1: SAMPLE COMPOSITION ===\n")
cat("Total encounters in analytical sample:", n_total, "\n")
cat("Total batched encounters:", n_batched, "(", round(n_batched/n_total*100, 1), "%)\n")
cat("\nBy compliance type:\n")
cat("  Compliers:", n_compliers, "(", round(pi_c*100, 1), "%)\n")
cat("  Always-takers:", n_always_takers, "(", round(pi_a*100, 1), "%)\n")
cat("  Never-takers:", n_never_takers, "(", round(pi_n*100, 1), "%)\n")
cat("\nPolicy-relevant subset:\n")
cat("  Batched compliers (avoidable):", n_batched_compliers, "\n")
cat("  As % of total sample:", round(n_batched_compliers/n_total*100, 1), "%\n")
cat("  As % of all batched:", round(n_batched_compliers/n_batched*100, 1), "%\n")

#=========================================================================
# STEP 2: Effect sizes from our UJIVE estimates (Table 4, Column 5)
#=========================================================================

# Primary effects
effect_LOS_log <- 0.503           # log points
effect_disp_log <- 0.522          # log points  
effect_imaging <- 1.174           # additional tests

# By modality
effect_xray <- 0.959
effect_us <- 0.087
effect_ct_no <- 0.053
effect_ct_yes <- 0.075

# Baseline values (sequenced patients, Column 1 of Table 4)
baseline_LOS_log <- 5.490
baseline_LOS_min <- exp(baseline_LOS_log)

baseline_disp_log <- 5.237
baseline_disp_min <- exp(baseline_disp_log)

# Calculate actual changes
LOS_pct_increase <- exp(effect_LOS_log) - 1
LOS_min_increase <- baseline_LOS_min * LOS_pct_increase
LOS_hrs_increase <- LOS_min_increase / 60

cat("\n=== STEP 2: EFFECT SIZES (UJIVE, Full Controls) ===\n")
cat("\nTime effects:\n")
cat("  Baseline LOS (sequenced):", round(baseline_LOS_min), "minutes\n")
cat("  LOS increase:", round(LOS_pct_increase*100, 1), "%\n")
cat("  LOS increase:", round(LOS_min_increase), "minutes =", round(LOS_hrs_increase, 2), "hours\n")
cat("\nImaging effects:\n")
cat("  Additional tests per batched complier:", round(effect_imaging, 2), "\n")
cat("    - X-rays:", round(effect_xray, 3), "\n")
cat("    - Ultrasound:", round(effect_us, 3), "\n")
cat("    - CT without contrast:", round(effect_ct_no, 3), "\n")
cat("    - CT with contrast:", round(effect_ct_yes, 3), "\n")

#=========================================================================
# STEP 3: Cost parameters (from literature/Medicare fee schedules)
#=========================================================================

# Imaging costs (Medicare Physician Fee Schedule 2024, professional + facility)
# Source: CMS Fee Schedule Search Tool
cost_xray <- c(low = 60, mean = 80, high = 100)
cost_us <- c(low = 150, mean = 200, high = 250)
cost_ct <- c(low = 300, mean = 400, high = 500)  # applies to both contrast and non

# ED bed-hour costs (from literature)
# Sources: Bamezai et al. 2005, Caldwell et al. 2013, Lee et al. 2012
cost_bedhour <- c(low = 75, mean = 100, high = 125)

cat("\n=== STEP 3: COST PARAMETERS ===\n")
cat("\nImaging costs (Medicare 2024, professional + facility):\n")
cat("  X-ray: $", cost_xray["low"], "- $", cost_xray["high"], " (mean $", cost_xray["mean"], ")\n")
cat("  Ultrasound: $", cost_us["low"], "- $", cost_us["high"], " (mean $", cost_us["mean"], ")\n")
cat("  CT scan: $", cost_ct["low"], "- $", cost_ct["high"], " (mean $", cost_ct["mean"], ")\n")
cat("\nED operational costs:\n")
cat("  Bed-hour: $", cost_bedhour["low"], "- $", cost_bedhour["high"], " (mean $", cost_bedhour["mean"], ")\n")

#=========================================================================
# STEP 4: Calculate per-patient costs
#=========================================================================

# Excess imaging cost per batched complier
calc_imaging_cost <- function(cost_level) {
  effect_xray * cost_xray[cost_level] +
    effect_us * cost_us[cost_level] +
    (effect_ct_no + effect_ct_yes) * cost_ct[cost_level]
}

imaging_cost_low <- calc_imaging_cost("low")
imaging_cost_mean <- calc_imaging_cost("mean")
imaging_cost_high <- calc_imaging_cost("high")

# Excess capacity cost per batched complier
capacity_cost_low <- LOS_hrs_increase * cost_bedhour["low"]
capacity_cost_mean <- LOS_hrs_increase * cost_bedhour["mean"]
capacity_cost_high <- LOS_hrs_increase * cost_bedhour["high"]

# Total cost per batched complier
total_cost_low <- imaging_cost_low + capacity_cost_low
total_cost_mean <- imaging_cost_mean + capacity_cost_mean
total_cost_high <- imaging_cost_high + capacity_cost_high

cat("\n=== STEP 4: PER-PATIENT COSTS ===\n")
cat("\nExcess imaging cost per batched complier:\n")
cat("  Low: $", round(imaging_cost_low), "\n")
cat("  Mean: $", round(imaging_cost_mean), "\n")
cat("  High: $", round(imaging_cost_high), "\n")
cat("\nExcess capacity cost per batched complier:\n")
cat("  Additional bed-hours:", round(LOS_hrs_increase, 2), "\n")
cat("  Low: $", round(capacity_cost_low), "\n")
cat("  Mean: $", round(capacity_cost_mean), "\n")
cat("  High: $", round(capacity_cost_high), "\n")
cat("\nTOTAL cost per batched complier:\n")
cat("  Low: $", round(total_cost_low), "\n")
cat("  Mean: $", round(total_cost_mean), "\n")
cat("  High: $", round(total_cost_high), "\n")

#=========================================================================
# STEP 5: Scale to study site (Mayo Clinic)
#=========================================================================

# Study period: October 2018 - December 2019 = 15 months
# Annualize: multiply by 12/15
annualization_factor <- 12/15

# Total ED volume during study period
total_ed_volume_study <- 48854  # from Table 1

# Annual equivalents
annual_ed_volume <- total_ed_volume_study * annualization_factor
annual_imaging_encounters <- n_total * annualization_factor
annual_batched_compliers <- n_batched_compliers * annualization_factor

# Annual costs
annual_cost_low <- annual_batched_compliers * total_cost_low
annual_cost_mean <- annual_batched_compliers * total_cost_mean
annual_cost_high <- annual_batched_compliers * total_cost_high

cat("\n=== STEP 5: ANNUAL COSTS (MAYO CLINIC) ===\n")
cat("\nStudy period: 15 months\n")
cat("Total ED encounters (study period):", format(total_ed_volume_study, big.mark=","), "\n")
cat("Imaging-relevant encounters (study period):", format(n_total, big.mark=","), "\n")
cat("  (", round(n_total/total_ed_volume_study*100, 1), "% of total)\n")
cat("\nAnnualized figures:\n")
cat("  Annual ED volume:", format(round(annual_ed_volume), big.mark=","), "\n")
cat("  Annual imaging-relevant encounters:", format(round(annual_imaging_encounters), big.mark=","), "\n")
cat("  Annual batched compliers:", format(round(annual_batched_compliers), big.mark=","), "\n")
cat("\nANNUAL COST OF DISCRETIONARY BATCHING:\n")
cat("  Low: $", format(round(annual_cost_low), big.mark=","), "\n")
cat("  Mean: $", format(round(annual_cost_mean), big.mark=","), "\n")
cat("  High: $", format(round(annual_cost_high), big.mark=","), "\n")

#=========================================================================
# STEP 6: Scale to different ED sizes (following Feizi et al. 2025)
#=========================================================================

# Share of encounters that are imaging-relevant
imaging_share <- n_total / total_ed_volume_study

# Share of imaging encounters that are batched compliers
batched_complier_share <- n_batched_compliers / n_total

# ED size categories (annual volumes)
ed_sizes <- data.frame(
  Type = c("Small (Rural/Community)", "Medium (Suburban)", "Large (Urban)", "Mayo Clinic (Study Site)"),
  Annual_Volume = c(20000, 40000, 60000, round(annual_ed_volume))
)

# Calculate for each ED size
ed_sizes$Imaging_Encounters <- round(ed_sizes$Annual_Volume * imaging_share)
ed_sizes$Batched_Compliers <- round(ed_sizes$Imaging_Encounters * batched_complier_share)
ed_sizes$Cost_Low <- ed_sizes$Batched_Compliers * total_cost_low
ed_sizes$Cost_Mean <- ed_sizes$Batched_Compliers * total_cost_mean
ed_sizes$Cost_High <- ed_sizes$Batched_Compliers * total_cost_high

cat("\n=== STEP 6: ANNUAL COSTS BY ED SIZE ===\n")
cat("\nAssumptions:\n")
cat("  Imaging-relevant share:", round(imaging_share*100, 1), "% of ED encounters\n")
cat("  Batched complier share:", round(batched_complier_share*100, 1), "% of imaging encounters\n")
cat("\n")

# Print table
cat(sprintf("%-30s %10s %12s %15s %20s\n", 
            "ED Type", "Annual Vol", "Imaging Enc", "Batched Compl", "Annual Cost (Mean)"))
cat(paste(rep("-", 90), collapse=""), "\n")
for (i in 1:nrow(ed_sizes)) {
  cat(sprintf("%-30s %10s %12s %15s %20s\n",
              ed_sizes$Type[i],
              format(ed_sizes$Annual_Volume[i], big.mark=","),
              format(ed_sizes$Imaging_Encounters[i], big.mark=","),
              format(ed_sizes$Batched_Compliers[i], big.mark=","),
              paste0("$", format(round(ed_sizes$Cost_Mean[i]), big.mark=","))))
}

#=========================================================================
# STEP 7: Create summary table for manuscript
#=========================================================================

cat("\n\n=== TABLE FOR MANUSCRIPT ===\n")
cat("Table X: Estimated Annual Cost of Discretionary Batch Ordering by ED Size\n\n")

summary_table <- data.frame(
  ED_Type = ed_sizes$Type,
  Annual_Volume = format(ed_sizes$Annual_Volume, big.mark=","),
  Imaging_Relevant = format(ed_sizes$Imaging_Encounters, big.mark=","),
  Batched_Compliers = format(ed_sizes$Batched_Compliers, big.mark=","),
  Cost_Mean = paste0("$", format(round(ed_sizes$Cost_Mean), big.mark=",")),
  Cost_Range = paste0("($", format(round(ed_sizes$Cost_Low), big.mark=","), 
                      " - $", format(round(ed_sizes$Cost_High), big.mark=","), ")")
)

print(summary_table, row.names = FALSE)

#=========================================================================
# STEP 8: Additional metrics for discussion
#=========================================================================

cat("\n\n=== ADDITIONAL METRICS ===\n")

# Bed-hours freed if batching eliminated
annual_bedhours_freed <- annual_batched_compliers * LOS_hrs_increase
cat("\nCapacity implications (Mayo Clinic annually):\n")
cat("  Bed-hours freed if discretionary batching eliminated:", 
    format(round(annual_bedhours_freed), big.mark=","), "\n")

# Additional visits that could be served (using baseline LOS)
additional_visits <- annual_bedhours_freed / (baseline_LOS_min / 60)
cat("  Potential additional visits served:", format(round(additional_visits), big.mark=","), "\n")
cat("  Capacity improvement:", round(additional_visits / annual_ed_volume * 100, 2), "%\n")

# Imaging tests avoided
annual_tests_avoided <- annual_batched_compliers * effect_imaging
cat("\nResource utilization:\n")
cat("  Imaging tests avoided annually:", format(round(annual_tests_avoided), big.mark=","), "\n")

# Cost per percentage point reduction in batching
cat("\nCost-effectiveness:\n")
cat("  Cost per batched complier: $", round(total_cost_mean), "\n")
cat("  If intervention reduces discretionary batching by 50%:\n")
cat("    Patients affected:", round(annual_batched_compliers * 0.5), "\n")
cat("    Annual savings: $", format(round(annual_cost_mean * 0.5), big.mark=","), "\n")



#=========================================================================
# REVISED COST ANALYSIS: X-RAY ONLY (statistically significant effect)
#=========================================================================

# Only X-ray effect (statistically significant)
effect_xray <- 0.959

# X-ray costs (Medicare 2024)
cost_xray <- c(low = 60, mean = 80, high = 100)

# Revised imaging costs (X-ray only)
imaging_cost_low <- effect_xray * cost_xray["low"]
imaging_cost_mean <- effect_xray * cost_xray["mean"]
imaging_cost_high <- effect_xray * cost_xray["high"]

cat("=== REVISED IMAGING COSTS (X-Ray Only) ===\n")
cat("Effect: 0.959 additional X-rays (p < 0.001)\n")
cat("  Low: $", round(imaging_cost_low), "\n")
cat("  Mean: $", round(imaging_cost_mean), "\n")
cat("  High: $", round(imaging_cost_high), "\n")

# Capacity costs remain the same
cat("\n=== CAPACITY COSTS (unchanged) ===\n")
cat("  Low: $", round(capacity_cost_low), "\n")
cat("  Mean: $", round(capacity_cost_mean), "\n")
cat("  High: $", round(capacity_cost_high), "\n")

# Revised total costs
total_cost_low <- imaging_cost_low + capacity_cost_low
total_cost_mean <- imaging_cost_mean + capacity_cost_mean
total_cost_high <- imaging_cost_high + capacity_cost_high

cat("\n=== REVISED TOTAL COST PER BATCHED COMPLIER ===\n")
cat("  Low: $", round(total_cost_low), "\n")
cat("  Mean: $", round(total_cost_mean), "\n")
cat("  High: $", round(total_cost_high), "\n")

# Revised annual costs
annual_cost_low <- annual_batched_compliers * total_cost_low
annual_cost_mean <- annual_batched_compliers * total_cost_mean
annual_cost_high <- annual_batched_compliers * total_cost_high

cat("\n=== REVISED ANNUAL COSTS (MAYO CLINIC) ===\n")
cat("  Low: $", format(round(annual_cost_low), big.mark=","), "\n")
cat("  Mean: $", format(round(annual_cost_mean), big.mark=","), "\n")
cat("  High: $", format(round(annual_cost_high), big.mark=","), "\n")

# Revised table by ED size
ed_sizes$Cost_Low <- ed_sizes$Batched_Compliers * total_cost_low
ed_sizes$Cost_Mean <- ed_sizes$Batched_Compliers * total_cost_mean
ed_sizes$Cost_High <- ed_sizes$Batched_Compliers * total_cost_high

cat("\n=== REVISED TABLE BY ED SIZE ===\n")
summary_table <- data.frame(
  ED_Type = ed_sizes$Type,
  Annual_Volume = format(ed_sizes$Annual_Volume, big.mark=","),
  Batched_Compliers = format(ed_sizes$Batched_Compliers, big.mark=","),
  Cost_Mean = paste0("$", format(round(ed_sizes$Cost_Mean), big.mark=",")),
  Cost_Range = paste0("($", format(round(ed_sizes$Cost_Low), big.mark=","), 
                      " - $", format(round(ed_sizes$Cost_High), big.mark=","), ")")
)
print(summary_table, row.names = FALSE)

# Comparison
cat("\n=== COMPARISON: ALL TESTS vs X-RAY ONLY ===\n")
cat("Imaging cost per complier:\n")
cat("  All tests (point estimates): $145\n")
cat("  X-ray only (significant):    $", round(imaging_cost_mean), "\n")
cat("\nTotal cost per complier:\n")
cat("  All tests: $409\n")
cat("  X-ray only: $", round(total_cost_mean), "\n")
cat("\nNote: Capacity cost ($264) dominates in both cases\n")