################################################################################
# Decomposition Analysis: Section 4.3
# Run after: source("src/clean.R")  — requires `final` to be defined
################################################################################

library(fixest)

sink('outputs/tables/decomposition_results.txt')

# ==============================================================================
# APPROACH 1: Product-of-Coefficients Decomposition
# Uses 2SLS with full controls (consistent with Table 4 Column 3)
# ==============================================================================

# --- IV: Batching -> Mediators (causal) ---------------------------------------

iv_imaging <- feols(
  imgTests ~ tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level |
    batched ~ batch.tendency,
  data = final, vcov = "HC1"
)

iv_admit <- feols(
  admit ~ tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level |
    batched ~ batch.tendency,
  data = final, vcov = "HC1"
)

# --- IV: Batching -> Final Outcomes (causal, total effect) --------------------

iv_los <- feols(
  ln_ED_LOS ~ tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level |
    batched ~ batch.tendency,
  data = final, vcov = "HC1"
)

iv_disp <- feols(
  ln_disp_time ~ tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level |
    batched ~ batch.tendency,
  data = final, vcov = "HC1"
)

# --- OLS: Mediator -> Outcome (controlled associations, NOT causal) -----------

ols_los <- feols(
  ln_ED_LOS ~ imgTests + admit + batched +
    tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level,
  data = final, vcov = "HC1"
)

ols_disp <- feols(
  ln_disp_time ~ imgTests + batched +
    tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level,
  data = final, vcov = "HC1"
)

# --- Extract and decompose ----------------------------------------------------

alpha_img  <- coef(iv_imaging)["fit_batched"]
alpha_adm  <- coef(iv_admit)["fit_batched"]
total_los  <- coef(iv_los)["fit_batched"]
total_disp <- coef(iv_disp)["fit_batched"]
beta_img_los  <- coef(ols_los)["imgTests"]
beta_adm_los  <- coef(ols_los)["admit"]
beta_img_disp <- coef(ols_disp)["imgTests"]

ind_img_los  <- alpha_img * beta_img_los
ind_adm_los  <- alpha_adm * beta_adm_los
ind_img_disp <- alpha_img * beta_img_disp
resid_los    <- total_los - ind_img_los - ind_adm_los
resid_disp   <- total_disp - ind_img_disp

cat("================================================================\n")
cat("PANEL A: PRODUCT-OF-COEFFICIENTS DECOMPOSITION\n")
cat("================================================================\n\n")

cat("Total effects (2SLS):\n")
cat(sprintf("  LOS:   %.4f (SE %.4f)\n", total_los, se(iv_los)["fit_batched"]))
cat(sprintf("  Disp:  %.4f (SE %.4f)\n\n", total_disp, se(iv_disp)["fit_batched"]))

cat("Effects on mediators (2SLS):\n")
cat(sprintf("  -> Imaging: %.4f (SE %.4f)\n", alpha_img, se(iv_imaging)["fit_batched"]))
cat(sprintf("  -> Admit:   %.4f (SE %.4f)\n\n", alpha_adm, se(iv_admit)["fit_batched"]))

cat("OLS associations (mediator -> outcome):\n")
cat(sprintf("  Imaging -> LOS:  %.4f (SE %.4f)\n", beta_img_los, se(ols_los)["imgTests"]))
cat(sprintf("  Admit -> LOS:    %.4f (SE %.4f)\n", beta_adm_los, se(ols_los)["admit"]))
cat(sprintf("  Imaging -> Disp: %.4f (SE %.4f)\n\n", beta_img_disp, se(ols_disp)["imgTests"]))

cat("--- LOS Decomposition ---\n")
cat(sprintf("  Via imaging:   %.3f x %.3f = %.4f  (%4.0f%%)\n",
            alpha_img, beta_img_los, ind_img_los, 100*ind_img_los/total_los))
cat(sprintf("  Via admission: %.3f x %.3f = %.4f  (%4.0f%%)\n",
            alpha_adm, beta_adm_los, ind_adm_los, 100*ind_adm_los/total_los))
cat(sprintf("  Residual:                    %.4f  (%4.0f%%)\n",
            resid_los, 100*resid_los/total_los))
cat(sprintf("  Total:                       %.4f\n\n", total_los))

cat("--- Disposition Time Decomposition ---\n")
cat(sprintf("  Via imaging:   %.3f x %.3f = %.4f  (%4.0f%%)\n",
            alpha_img, beta_img_disp, ind_img_disp, 100*ind_img_disp/total_disp))
cat(sprintf("  Residual:                    %.4f  (%4.0f%%)\n",
            resid_disp, 100*resid_disp/total_disp))
cat(sprintf("  Total:                       %.4f\n\n", total_disp))


# ==============================================================================
# APPROACH 2: Reduced-Form Attenuation
# ==============================================================================

cat("\n================================================================\n")
cat("PANEL B: REDUCED-FORM ATTENUATION\n")
cat("================================================================\n\n")

# --- LOS ----------------------------------------------------------------------
rf_los_base <- feols(
  ln_ED_LOS ~ batch.tendency +
    tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level,
  data = final, vcov = "HC1"
)

rf_los_img <- feols(
  ln_ED_LOS ~ batch.tendency + imgTests +
    tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level,
  data = final, vcov = "HC1"
)

rf_los_both <- feols(
  ln_ED_LOS ~ batch.tendency + imgTests + admit +
    tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level,
  data = final, vcov = "HC1"
)

# --- Disposition time ---------------------------------------------------------
rf_disp_base <- feols(
  ln_disp_time ~ batch.tendency +
    tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level,
  data = final, vcov = "HC1"
)

rf_disp_img <- feols(
  ln_disp_time ~ batch.tendency + imgTests +
    tachycardic + tachypneic + febrile + hypotensive +
    age + EXPERIENCE + hrs_in_shift + LAB_PERF |
    dayofweekt + month_of_year + complaint_esi + race + GENDER +
    PROVIDER_SEX + capacity_level,
  data = final, vcov = "HC1"
)

# --- Print attenuation --------------------------------------------------------

b0_los <- coef(rf_los_base)["batch.tendency"]
b1_los <- coef(rf_los_img)["batch.tendency"]
b2_los <- coef(rf_los_both)["batch.tendency"]
b0_disp <- coef(rf_disp_base)["batch.tendency"]
b1_disp <- coef(rf_disp_img)["batch.tendency"]

cat("--- LOS ---\n")
cat(sprintf("  Baseline:              %.4f (SE %.4f)\n", b0_los, se(rf_los_base)["batch.tendency"]))
cat(sprintf("  + Imaging:             %.4f (SE %.4f)  [%.1f%% attenuation]\n",
            b1_los, se(rf_los_img)["batch.tendency"], 100*(1 - b1_los/b0_los)))
cat(sprintf("  + Imaging & Admission: %.4f (SE %.4f)  [%.1f%% attenuation]\n\n",
            b2_los, se(rf_los_both)["batch.tendency"], 100*(1 - b2_los/b0_los)))

cat("  Imaging coef (in full spec):   %.4f (SE %.4f)\n",
    coef(rf_los_both)["imgTests"], se(rf_los_both)["imgTests"])
cat("  Admission coef (in full spec): %.4f (SE %.4f)\n\n",
    coef(rf_los_both)["admit"], se(rf_los_both)["admit"])

cat("--- Disposition Time ---\n")
cat(sprintf("  Baseline:              %.4f (SE %.4f)\n", b0_disp, se(rf_disp_base)["batch.tendency"]))
cat(sprintf("  + Imaging:             %.4f (SE %.4f)  [%.1f%% attenuation]\n\n",
            b1_disp, se(rf_disp_img)["batch.tendency"], 100*(1 - b1_disp/b0_disp)))

cat("  Imaging coef: %.4f (SE %.4f)\n\n",
    coef(rf_disp_img)["imgTests"], se(rf_disp_img)["imgTests"])


# --- Regression tables (for appendix) -----------------------------------------

cat("\n================================================================\n")
cat("REGRESSION TABLES FOR APPENDIX\n")
cat("================================================================\n\n")

cat("--- LOS Attenuation ---\n")
etable(rf_los_base, rf_los_img, rf_los_both,
       keep = c("batch.tendency", "imgTests", "admit"),
       headers = c("Baseline", "+ Imaging", "+ Both"))

cat("\n--- Disposition Time Attenuation ---\n")
etable(rf_disp_base, rf_disp_img,
       keep = c("batch.tendency", "imgTests"),
       headers = c("Baseline", "+ Imaging"))

cat("\n--- OLS Mediator Regressions ---\n")
etable(ols_los, ols_disp,
       keep = c("imgTests", "admit", "batched"),
       headers = c("Log LOS", "Log Dispo Time"))

sink()
cat("Results saved to outputs/tables/decomposition_results.txt\n")