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
complaints_less_than_500 <- names(complaint_counts[complaint_counts < 1000])
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
