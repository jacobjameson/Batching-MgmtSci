# --- Panel A: Patient Severity ---
panel_a <- data %>%
  summarise(
    tachycardic = mean(tachycardic, na.rm = TRUE) * 100,
    tachypneic  = mean(tachypneic, na.rm = TRUE) * 100,
    febrile     = mean(febrile, na.rm = TRUE) * 100,
    hypotensive = mean(hypotensive, na.rm = TRUE) * 100,
    ESI_median  = median(ESI, na.rm = TRUE),
    ESI_IQR     = paste0("[", quantile(ESI, 0.25, na.rm = TRUE), ", ", 
                         quantile(ESI, 0.75, na.rm = TRUE), "]")
  )

# --- Panel B: Patient Demographics ---
panel_b <- data %>%
  summarise(
    male     = mean(GENDER == "Male", na.rm = TRUE) * 100,
    race_white = mean(race == "white", na.rm = TRUE) * 100,
    race_black = mean(race == "black", na.rm = TRUE) * 100,
    race_asian = mean(race == "asian", na.rm = TRUE) * 100,
    age_median = median(age, na.rm = TRUE),
    age_IQR    = paste0("[", quantile(age, 0.25, na.rm = TRUE), ", ",
                        quantile(age, 0.75, na.rm = TRUE), "]")
  )

# --- Panel C: Diagnostic Tests, Outcomes, and Timings ---
panel_c <- data %>%
  summarise(
    # --- imaging/tests performed ---
    xray_perf       = mean(PLAIN_XRAY == 1, na.rm = TRUE) * 100,
    us_perf         = mean(US_PERF == 1, na.rm = TRUE) * 100,
    noncon_ct_perf  = mean(NON_CON_CT_PERF == 1, na.rm = TRUE) * 100,
    con_ct_perf     = mean(CON_CT_PERF == 1, na.rm = TRUE) * 100,
    labs_ordered    = mean(LAB_PERF == 1, na.rm = TRUE) * 100,
    
    # --- timing metrics ---
    wait_median = median(wait_time, na.rm = TRUE),
    wait_iqr    = paste0("[", quantile(wait_time, 0.25, na.rm = TRUE), ", ",
                         quantile(wait_time, 0.75, na.rm = TRUE), "]"),
    
    los_median  = median(ED_LOS, na.rm = TRUE),
    los_iqr     = paste0("[", quantile(ED_LOS, 0.25, na.rm = TRUE), ", ",
                         quantile(ED_LOS, 0.75, na.rm = TRUE), "]"),
    
    # --- disposition ---
    discharged   = mean(discharge == 1, na.rm = TRUE) * 100,
    admitted     = mean(admit == 1, na.rm = TRUE) * 100,
    observation  = mean(observation == 1, na.rm = TRUE) * 100,
    
    # --- revisits ---
    rtn_72_any   = mean(RTN_72_HR == 1, na.rm = TRUE) * 100,
    rtn_72_admit = mean(RTN_72_HR_ADMIT == 1, na.rm = TRUE) * 100,
    
    # --- turnaround times (order → result) ---
    tat_xray_median = median(PLAIN_XRAY_TAT, na.rm = TRUE),
    tat_xray_iqr    = paste0("[", quantile(PLAIN_XRAY_TAT, 0.25, na.rm = TRUE), ", ",
                             quantile(PLAIN_XRAY_TAT, 0.75, na.rm = TRUE), "]"),
    
    tat_us_median = median(US_TAT, na.rm = TRUE),
    tat_us_iqr    = paste0("[", quantile(US_TAT, 0.25, na.rm = TRUE), ", ",
                           quantile(US_TAT, 0.75, na.rm = TRUE), "]"),
    
    tat_ct_noncon_median = median(CT_WITHOUT_CONTR_TAT, na.rm = TRUE),
    tat_ct_noncon_iqr    = paste0("[", quantile(CT_WITHOUT_CONTR_TAT, 0.25, na.rm = TRUE), ", ",
                                  quantile(CT_WITHOUT_CONTR_TAT, 0.75, na.rm = TRUE), "]"),
    
    tat_ct_con_median = median(CT_WITH_CONTR_TAT, na.rm = TRUE),
    tat_ct_con_iqr    = paste0("[", quantile(CT_WITH_CONTR_TAT, 0.25, na.rm = TRUE), ", ",
                               quantile(CT_WITH_CONTR_TAT, 0.75, na.rm = TRUE), "]"),
    
    tat_lab_median = median(LAB_TAT, na.rm = TRUE),
    tat_lab_iqr    = paste0("[", quantile(LAB_TAT, 0.25, na.rm = TRUE), ", ",
                            quantile(LAB_TAT, 0.75, na.rm = TRUE), "]")
  )
# --- Combine and clean for display ---
summary_table <- bind_rows(
  panel_a %>% mutate(panel = "Panel A. Patient Severity"),
  panel_b %>% mutate(panel = "Panel B. Patient Demographics"),
  panel_c %>% mutate(panel = "Panel C. Diagnostic Tests and Outcomes")
) %>%
  janitor::clean_names()

sink("outputs/tables/table1_summary_statistics_mayo.txt")
panel_a
panel_b
panel_c
sink()
