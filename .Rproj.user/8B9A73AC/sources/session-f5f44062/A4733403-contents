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
    separate(
      col,
      into = c("hrs", "mins"),
      sep = ":",
      fill = "right",
      remove = TRUE
    ) %>%
    mutate(
      across(c(hrs, mins), ~ suppressWarnings(as.numeric(.))),
      !!col := hrs * 60 + mins
    ) %>%
    select(-hrs, -mins)
}

data$admit <- ifelse(data$ED_DISPOSITION == 'Admit', 1, 0)

data <- data %>%
  mutate(
    dispo_time = case_when(
      !is.na(ED_DISCHARGE_DT_REL) ~ ED_DISCHARGE_DT_REL,
      !is.na(ADMIT_OBS_ORD_DTTM_REL) ~ ADMIT_OBS_ORD_DTTM_REL,
      !is.na(ADMIT_INP_ORD_DTTM_REL) ~ ADMIT_INP_ORD_DTTM_REL,
      TRUE ~ NA_real_
    )
  )

# create time to disposition variable
data$time_to_dispo <- data$dispo_time - data$ARRIVAL_DTTM_REL
data$time_to_dispo <- ifelse(data$time_to_dispo < 0, NA, data$time_to_dispo)

# Filter data for correct times
data <- data %>%
  filter(time_to_dispo <= 1440,
         !is.na(time_to_dispo),
         time_to_dispo > 0,
         ED_LOS <= 1440,
         ED_LOS > 0,
         !is.na(ED_LOS),
         time_to_dispo <= ED_LOS)


# create waiting time variable
data$wait_time <- data$TRIAGE_COMPLETED_REL - data$ARRIVAL_DTTM_REL
data <- filter(data, wait_time >= 0)

calculate_waiting_patients <- function(time, arrivals, first_contacts) {
  sum(arrivals <= time & first_contacts > time, na.rm = TRUE)
}

data <- data %>%
  mutate(
    # number of patients waiting at this patient's arrival time
    patients_waiting = map_dbl(
      ARRIVAL_DTTM_REL,
      ~calculate_waiting_patients(.x, ARRIVAL_DTTM_REL, TRIAGE_COMPLETED_REL)
    ),
    # classify capacity state
    capacity_level = case_when(
      wait_time > 90 | patients_waiting > 20 ~ "Major Overcapacity",
      (wait_time >= 21 & wait_time <= 90) | patients_waiting >= 10 ~ "Minor Overcapacity",
      wait_time < 20 & patients_waiting < 10 ~ "Normal Operations",
      TRUE ~ "Normal Operations"
    )
  )

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
  mutate(race = case_when(
    grepl('black', PATIENT_RACE, fixed = TRUE) ~ "black",
    grepl('african', PATIENT_RACE, fixed = TRUE) ~ "black",
    grepl('asian', PATIENT_RACE, fixed = TRUE) ~ "asian",
    grepl('pacific islander', PATIENT_RACE, fixed = TRUE) ~ "asian",
    grepl('native', PATIENT_RACE, fixed = TRUE)~ "native",
    grepl('samoan', PATIENT_RACE, fixed = TRUE) ~ "other",
    grepl('guamanian or chamorro', PATIENT_RACE, fixed = TRUE) ~ "other",
    grepl('white', PATIENT_RACE, fixed = TRUE) ~ "white",
    grepl('unknown', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    grepl('choose not to disclose', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    grepl('unable to provide', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    grepl('other', PATIENT_RACE, fixed = TRUE) ~ "other",
    grepl('', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    TRUE ~ PATIENT_RACE))

data$age <- ifelse(
  data$ARRIVAL_AGE_DI == '85+', '85', data$ARRIVAL_AGE_DI
)
data$age <- as.numeric(data$age)

data <- data %>% 
  filter(ARRIVAL_AGE_DI >= 18)

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

