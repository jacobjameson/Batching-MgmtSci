#=========================================================================
# ALTERNATIVE WITHIN-PROVIDER LENIENCY INSTRUMENTS
# Run AFTER residual_batch has been created
#=========================================================================

library(slider)

# Ensure chronological ordering
data <- data %>%
  arrange(ED_PROVIDER, actual_date)

#=========================================================================
# 1. ROLLING WITHIN-PROVIDER LENIENCY
#    Main recommended specification
#=========================================================================

data <- data %>%
  group_by(ED_PROVIDER) %>%
  mutate(
    provider_case_order = row_number(),
    
    # Rolling averages using only PRIOR encounters
    rolling_batch_25 = slide_dbl(
      residual_batch,
      ~mean(.x, na.rm = TRUE),
      .before = 25,
      .after = -1
    ),
    
    rolling_batch_50 = slide_dbl(
      residual_batch,
      ~mean(.x, na.rm = TRUE),
      .before = 50,
      .after = -1
    ),
    
    rolling_batch_100 = slide_dbl(
      residual_batch,
      ~mean(.x, na.rm = TRUE),
      .before = 100,
      .after = -1
    ),
    
    provider_mean_resid_batch = mean(residual_batch, na.rm = TRUE),
    
    rolling_batch_25_within =
      rolling_batch_25 - provider_mean_resid_batch,
    
    rolling_batch_50_within =
      rolling_batch_50 - provider_mean_resid_batch,
    
    rolling_batch_100_within =
      rolling_batch_100 - provider_mean_resid_batch
  ) %>%
  ungroup()


#=========================================================================
# 2. PRIOR SHIFT-LEVEL LENIENCY
#=========================================================================

data <- data %>%
  arrange(ED_PROVIDER, shift_id, actual_date) %>%
  group_by(ED_PROVIDER, shift_id) %>%
  mutate(
    shift_case_order = row_number(),
    
    prior_shift_batch_tendency =
      if_else(
        shift_case_order > 1,
        lag(cummean(residual_batch)),
        NA_real_
      )
  ) %>%
  ungroup() %>%
  group_by(ED_PROVIDER) %>%
  mutate(
    provider_mean_resid_batch = mean(residual_batch, na.rm = TRUE),
    
    prior_shift_batch_tendency_within =
      prior_shift_batch_tendency -
      provider_mean_resid_batch
  ) %>%
  ungroup()


#=========================================================================
# 3. PROVIDER × COMPLAINT LENIENCY
#=========================================================================

data <- data %>%
  group_by(ED_PROVIDER, complaint_esi) %>%
  mutate(
    n_provider_complaint = n(),
    
    sum_provider_complaint_resid =
      sum(residual_batch, na.rm = TRUE),
    
    provider_complaint_batch_tendency =
      if_else(
        n_provider_complaint > 1,
        (sum_provider_complaint_resid - residual_batch) /
          (n_provider_complaint - 1),
        NA_real_
      )
  ) %>%
  ungroup() %>%
  group_by(ED_PROVIDER) %>%
  mutate(
    provider_mean_resid_batch = mean(residual_batch, na.rm = TRUE),
    
    provider_complaint_batch_tendency_within =
      provider_complaint_batch_tendency -
      provider_mean_resid_batch
  ) %>%
  ungroup()


#=========================================================================
# 4. HOUR-OF-SHIFT LENIENCY
#=========================================================================

data <- data %>%
  mutate(
    hrs_in_shift_bin = cut(
      hrs_in_shift,
      breaks = c(-Inf, 2, 4, 6, 8, Inf),
      labels = c("0-2", "2-4", "4-6", "6-8", "8+"),
      right = FALSE
    )
  ) %>%
  group_by(ED_PROVIDER, hrs_in_shift_bin) %>%
  mutate(
    n_provider_shift_hour = n(),
    
    sum_provider_shift_hour_resid =
      sum(residual_batch, na.rm = TRUE),
    
    provider_shift_hour_batch_tendency =
      if_else(
        n_provider_shift_hour > 1,
        (sum_provider_shift_hour_resid - residual_batch) /
          (n_provider_shift_hour - 1),
        NA_real_
      )
  ) %>%
  ungroup() %>%
  group_by(ED_PROVIDER) %>%
  mutate(
    provider_mean_resid_batch = mean(residual_batch, na.rm = TRUE),
    
    provider_shift_hour_batch_tendency_within =
      provider_shift_hour_batch_tendency -
      provider_mean_resid_batch
  ) %>%
  ungroup()


#=========================================================================
# EXAMPLE FIRST STAGES WITH PROVIDER FIXED EFFECTS
#=========================================================================

# Rolling leniency (recommended)

fs_roll <- felm(
  batched ~
    tachycardic + tachypneic + febrile + hypotensive + age |
    ED_PROVIDER + dayofweekt + month_of_year +
    complaint_esi + race + GENDER |
    (batched ~ rolling_batch_50_within) |
    ED_PROVIDER,
  data = data
)

summary(fs_roll)


# Shift-level leniency

fs_shift <- felm(
  batched ~
    tachycardic + tachypneic + febrile + hypotensive + age |
    ED_PROVIDER + dayofweekt + month_of_year +
    complaint_esi + race + GENDER |
    (batched ~ prior_shift_batch_tendency_within) |
    ED_PROVIDER,
  data = data
)

summary(fs_shift)


# Provider × complaint leniency

fs_complaint <- felm(
  batched ~
    tachycardic + tachypneic + febrile + hypotensive + age |
    ED_PROVIDER + dayofweekt + month_of_year +
    complaint_esi + race + GENDER |
    (batched ~ provider_complaint_batch_tendency_within) |
    ED_PROVIDER,
  data = data
)

summary(fs_complaint)


# Hour-of-shift leniency

fs_hour <- felm(
  batched ~
    tachycardic + tachypneic + febrile + hypotensive + age |
    ED_PROVIDER + dayofweekt + month_of_year +
    complaint_esi + race + GENDER |
    (batched ~ provider_shift_hour_batch_tendency_within) |
    ED_PROVIDER,
  data = data
)

summary(fs_hour)


#=========================================================================
# FIRST-STAGE COMPARISON OF ALTERNATIVE INSTRUMENTS
#=========================================================================

#--------------------------------------------------------------------------
# Rolling 50-case leniency
#--------------------------------------------------------------------------

fs_roll <- feols(
  batched ~ rolling_batch_50_within +
    tachycardic + tachypneic + febrile + hypotensive +
    EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
    age + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER +
    dayofweekt + month_of_year +
    complaint_esi + race + GENDER,
  vcov = "HC1",
  data = final
)

wald(fs_roll, keep = "rolling_batch_50_within")


#--------------------------------------------------------------------------
# Shift-level leniency
#--------------------------------------------------------------------------

fs_shift <- feols(
  batched ~ prior_shift_batch_tendency_within +
    tachycardic + tachypneic + febrile + hypotensive +
    EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
    age + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER +
    dayofweekt + month_of_year +
    complaint_esi + race + GENDER,
  vcov = "HC1",
  data = final
)

wald(fs_shift, keep = "prior_shift_batch_tendency_within")


#--------------------------------------------------------------------------
# Provider × complaint leniency
#--------------------------------------------------------------------------

fs_complaint <- feols(
  batched ~ provider_complaint_batch_tendency_within +
    tachycardic + tachypneic + febrile + hypotensive +
    EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
    age + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER +
    dayofweekt + month_of_year +
    complaint_esi + race + GENDER,
  vcov = "HC1",
  data = final
)

wald(fs_complaint, keep = "provider_complaint_batch_tendency_within")


#--------------------------------------------------------------------------
# Hour-of-shift leniency
#--------------------------------------------------------------------------

fs_hour <- feols(
  batched ~ provider_shift_hour_batch_tendency_within +
    tachycardic + tachypneic + febrile + hypotensive +
    EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
    age + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER +
    dayofweekt + month_of_year +
    complaint_esi + race + GENDER,
  vcov = "HC1",
  data = final
)

wald(fs_hour, keep = "provider_shift_hour_batch_tendency_within")


#=========================================================================
# COMPARISON TABLE
#=========================================================================

etable(
  fs_roll,
  fs_shift,
  fs_complaint,
  fs_hour,
  keep = c(
    "rolling_batch_50_within",
    "prior_shift_batch_tendency_within",
    "provider_complaint_batch_tendency_within",
    "provider_shift_hour_batch_tendency_within"
  )
)



#=========================================================================
# TRUE FIRST-STAGE TESTS FOR ALTERNATIVE WITHIN-PROVIDER INSTRUMENTS
#=========================================================================

library(lfe)

#--------------------------------------------------------------------------
# Rolling 50-case within-provider leniency
#--------------------------------------------------------------------------

fs_roll <- felm(
  batched ~ rolling_batch_50_within +
    tachycardic + tachypneic + febrile + hypotensive +
    EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
    age + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER + dayofweekt + month_of_year +
    complaint_esi + race + GENDER |
    0 |
    ED_PROVIDER,
  data = final
)

summary(fs_roll)


#--------------------------------------------------------------------------
# Shift-level within-provider leniency
#--------------------------------------------------------------------------

fs_shift <- felm(
  batched ~ prior_shift_batch_tendency_within +
    tachycardic + tachypneic + febrile + hypotensive +
    EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
    age + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER + dayofweekt + month_of_year +
    complaint_esi + race + GENDER |
    0 |
    ED_PROVIDER,
  data = final
)

summary(fs_shift)


#--------------------------------------------------------------------------
# Provider × complaint within-provider leniency
#--------------------------------------------------------------------------

fs_complaint <- felm(
  batched ~ provider_complaint_batch_tendency_within +
    tachycardic + tachypneic + febrile + hypotensive +
    EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
    age + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER + dayofweekt + month_of_year +
    complaint_esi + race + GENDER |
    0 |
    ED_PROVIDER,
  data = final
)

summary(fs_complaint)


#--------------------------------------------------------------------------
# Provider × hour-of-shift within-provider leniency
#--------------------------------------------------------------------------

fs_hour <- felm(
  batched ~ provider_shift_hour_batch_tendency_within +
    tachycardic + tachypneic + febrile + hypotensive +
    EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
    age + capacity_level + lab.tendency + admit.tendency |
    ED_PROVIDER + dayofweekt + month_of_year +
    complaint_esi + race + GENDER |
    0 |
    ED_PROVIDER,
  data = final
)

summary(fs_hour)


#=========================================================================
# Quick comparison table
#=========================================================================

stargazer::stargazer(
  fs_roll,
  fs_shift,
  fs_complaint,
  fs_hour,
  type = "text",
  keep = c(
    "rolling_batch_50_within",
    "prior_shift_batch_tendency_within",
    "provider_complaint_batch_tendency_within",
    "provider_shift_hour_batch_tendency_within"
  ),
  column.labels = c(
    "Rolling 50",
    "Prior shift",
    "Provider x complaint",
    "Provider x hour"
  )
)
