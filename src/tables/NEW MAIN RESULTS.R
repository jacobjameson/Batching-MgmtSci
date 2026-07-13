

## =====================================================================
## 2SLS vs 2SRI (control-function) comparison across outcomes
##
##   continuous   : 2SRI coef == 2SLS coef; report the 2SLS (HC1) SE
##                  (correct for the generated regressor by construction)
##   count/binary : 2SRI AME via an explicit 0 -> 1 contrast; SE via
##                  bootstrap that re-estimates the FIRST stage on every
##                  resample, so it reflects cf_resid (generated-regressor)
##                  uncertainty. The analytic HC1 SE is kept only as a
##                  naive reference.
##
##   `final` is assumed to be your loaded data frame.
## =====================================================================

library(fixest)
library(marginaleffects)
library(dplyr)
library(tibble)
library(purrr)

## ---- configuration ---------------------------------------------------
TREAT   <- "batched"          # endogenous 0/1 regressor
INSTR   <- "batch.tendency"   # excluded instrument
FE      <- "0"                # fixed-effects block ("0" = none)
LINCTRL <- paste(
  "dayofweekt", "month_of_year", "complaint_esi", "race", "GENDER",
  "PROVIDER_SEX", "capacity_level", "tachycardic", "tachypneic",
  "febrile", "hypotensive", "lab.tendency", "admit.tendency",
  "age", "EXPERIENCE", "hrs_in_shift",
  sep = " + "
)

## ---- average 0 -> 1 contrast on the response scale -------------------
## For a binary treatment this is the AME: mean[ f(x, T=1) - f(x, T=0) ].
## Equals the slope coefficient in the linear case.
contrast_01 <- function(model, dat, treat = TREAT) {
  d0 <- dat; d0[[treat]] <- 0
  d1 <- dat; d1[[treat]] <- 1
  p0 <- predict(model, newdata = d0, type = "response")
  p1 <- predict(model, newdata = d1, type = "response")
  mean(p1 - p0, na.rm = TRUE)
}

## ---- point estimates: 2SLS + 2SRI, with analytic SEs -----------------
run_iv_compare <- function(data, y_var,
                           type = c("continuous", "count", "binary"),
                           link = "probit") {
  type <- match.arg(type)
  d <- as.data.frame(data)
  
  ## first stage (full controls) -> control-function residual
  fs_form <- as.formula(paste0(TREAT, " ~ ", INSTR, " + ", LINCTRL, " | ", FE))
  fs <- feols(fs_form, data = d, vcov = "HC1")
  d$cf_resid <- resid(fs)
  
  ## 2SLS
  iv_form <- as.formula(paste0(y_var, " ~ ", LINCTRL, " | ", FE,
                               " | ", TREAT, " ~ ", INSTR))
  tsls <- feols(iv_form, data = d, vcov = "HC1")
  fit_name <- paste0("fit_", TREAT)
  b_2sls  <- unname(coef(tsls)[fit_name])
  se_2sls <- unname(se(tsls)[fit_name])
  fstat   <- fitstat(tsls, "ivf")[[1]]$stat   # first-stage F (1 endog / 1 instr)
  
  ## 2SRI second stage (control function)
  cf_form <- as.formula(paste0(y_var, " ~ ", TREAT, " + cf_resid + ",
                               LINCTRL, " | ", FE))
  
  if (type == "continuous") {
    cf <- feols(cf_form, data = d, vcov = "HC1")
    est_2sri <- unname(coef(cf)[TREAT])   # == b_2sls by construction
    se_naive <- se_2sls                   # 2SLS SE is analytically correct here
  } else {
    cf <- if (type == "count") {
      fepois(cf_form, data = d, vcov = "HC1")
    } else {
      feglm(cf_form, data = d, family = binomial(link), vcov = "HC1")
    }
    ## Force the 0 -> 1 contrast. NOTE: avg_comparisons' default for a
    ## numeric var is a +1 step (evaluating T = 1 and T = 2), which is
    ## wrong for a 0/1 treatment. setNames(list(c(0,1)), TREAT) pins it
    ## to lo = 0, hi = 1, matching contrast_01() used in the bootstrap.
    ac <- avg_comparisons(cf, variables = setNames(list(c(0, 1)), TREAT))
    est_2sri <- ac$estimate
    se_naive <- ac$std.error              # ignores first-stage noise -> bootstrap
  }
  
  ## endogeneity check: significance of cf_resid
  ## exact Wu-Hausman for the linear model; heuristic CF test otherwise.
  ct   <- coeftable(cf)
  pcol <- grep("Pr", colnames(ct))[1]
  haus_p <- unname(ct["cf_resid", pcol])
  
  tibble(
    outcome = y_var, type = type,
    first_stage_F = unname(fstat),
    b_2sls = b_2sls, se_2sls = se_2sls,
    est_2sri = est_2sri, se_2sri_naive = se_naive,
    hausman_p = haus_p,
    N_2sls = nobs(tsls), N_2sri = nobs(cf)
  )
}

## ---- bootstrap SE for the nonlinear 2SRI AME -------------------------
## Re-estimates first stage + second stage on each resample so the SE
## reflects the generated-regressor (cf_resid) uncertainty.
boot_2sri_se <- function(data, y_var, type, link = "probit",
                         B = 199, seed = 1) {
  set.seed(seed)
  d <- as.data.frame(data); n <- nrow(d)
  fs_form <- as.formula(paste0(TREAT, " ~ ", INSTR, " + ", LINCTRL, " | ", FE))
  cf_form <- as.formula(paste0(y_var, " ~ ", TREAT, " + cf_resid + ",
                               LINCTRL, " | ", FE))
  fam <- if (type == "binary") binomial(link) else NULL
  
  est <- vapply(seq_len(B), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    db  <- d[idx, , drop = FALSE]
    tryCatch({
      fs <- feols(fs_form, data = db)
      db$cf_resid <- resid(fs)
      m <- if (type == "count") fepois(cf_form, data = db)
      else                 feglm(cf_form, data = db, family = fam)
      contrast_01(m, db)
    }, error = function(e) NA_real_)
  }, numeric(1))
  
  c(se = sd(est, na.rm = TRUE), valid = sum(is.finite(est)))
}

## ---- outcomes --------------------------------------------------------
specs <- tribble(
  ~outcome,          ~type,
  "ln_treat_time",   "continuous",
  "ln_disp_time",    "continuous",
  "ln_ED_LOS",       "continuous",
  "imgTests",        "count",
  "RTN_72_HR_ADMIT", "binary",
  "RTN_72_HR",       "binary",
  "PLAIN_XRAY",      "binary",
  "US_PERF",         "binary",
  "NON_CON_CT_PERF", "binary",
  "CON_CT_PERF",     "binary",
  "admit",           "binary",
  "upgrade",         "binary",
  "downgrade",       "binary"
) %>% filter(outcome %in% names(final))

## ---- point estimates -------------------------------------------------
res <- pmap_dfr(list(specs$outcome, specs$type),
                ~ run_iv_compare(final, ..1, ..2))

## ---- bootstrap SEs for count + binary only ---------------------------
nl <- specs %>% filter(type %in% c("count", "binary"))
boot <- pmap_dfr(list(nl$outcome, nl$type), function(y, t) {
  bs <- boot_2sri_se(final, y, t, B = 199)
  tibble(outcome = y, se_boot = unname(bs["se"]),
         valid_reps = unname(bs["valid"]))
})

## ---- final table: correct SE per outcome type ------------------------
## continuous -> 2SLS SE ; count/binary -> bootstrap SE
res_final <- res %>%
  left_join(boot, by = "outcome") %>%
  mutate(se_2sri = coalesce(se_boot, se_2sri_naive))

fmt <- function(b, s, d = 3) sprintf(paste0("%.", d, "f (%.", d, "f)"), b, s)

res_final %>% transmute(
  Outcome         = outcome,
  `First-stage F` = sprintf("%.1f", first_stage_F),
  `2SLS`          = fmt(b_2sls, se_2sls),
  `2SRI (AME)`    = fmt(est_2sri, se_2sri),
  `Hausman p`     = sprintf("%.3f", hausman_p),
  N               = N_2sls
) %>% print(n = Inf)



## =====================================================================
## Testing whether batching -> LOS operates through imaging
## Kwon & Roth (2026), "Testing Mechanisms" -- sharp null of full mediation
##
##   D = batched            (binary treatment, endogenous)
##   M = imaging            (mediator; discrete -- count or modality)
##   Y = ln_ED_LOS          (outcome; discretized into quantile bins)
##   Z = batch.tendency     (instrument, continuous -- IV mode)
##
## The test asks: is the effect of batching on LOS *fully* explained by
## imaging? If rejected, lb_frac_affected() lower-bounds the fraction of
## patients whose imaging is UNCHANGED by batching but whose LOS is still
## moved -- i.e. a non-imaging channel (e.g. physician slowdown).
##
## NOTE: this delivers a TEST + LOWER BOUND, not a "% mediated" number.
## =====================================================================

## ---- install / load --------------------------------------------------
# install.packages("devtools")
library(TestMechs)
library(fixest)
library(dplyr)

## ---- controls / instrument (mirrors your 2SLS spec) ------------------
TREAT   <- "batched"
INSTR   <- "batch.tendency"
CLUSTER <- "ED_PROVIDER"       # <-- TODO: set to your physician/cluster id
LINCTRL <- paste(
  "dayofweekt", "month_of_year", "complaint_esi", "race", "GENDER",
  "PROVIDER_SEX", "capacity_level", "tachycardic", "tachypneic",
  "febrile", "hypotensive", "lab.tendency", "admit.tendency",
  "age", "EXPERIENCE", "hrs_in_shift",
  sep = " + "
)

## IV reg_formula: no LHS (package fills the compound outcome in),
## controls on the exogenous side, batched instrumented by tendency.
## Matches your feols IV structure exactly.
REG_IV <- paste0("~ ", LINCTRL, " | ", TREAT, " ~ ", INSTR)

## ---- STEP 0: sanity-check the monotonicity direction -----------------
## The package's default monotonicity is that treatment INCREASES M.
## Confirm batching raises imaging (positive first stage on the mediator).
feols(as.formula(paste0("imgTests ~ ", LINCTRL, " | ", TREAT, " ~ ", INSTR)),
      data = final, vcov = "HC1") |> summary()
## If the coefficient on fit_batched is < 0, imaging FALLS with batching:
## recode the mediator so that "more" points the monotone direction, or
## rely on max_defiers_share below.

## ---- STEP 1: build discrete mediators --------------------------------
final <- final %>% mutate(
  ## (a) simplest: binned imaging count with point mass preserved
  img_bin = pmin(imgTests, 2),                       # {0, 1, 2+}
  
  ## (b) binary "any imaging" -- used only for the density plot
  any_img = as.integer(imgTests > 0),
  
  ## (c) OPTIONAL modality-intensity mediator (separates turnaround-heavy
  ##     advanced imaging from plain film). Ordinal 0 < 1 < 2.
  img_mode = case_when(
    NON_CON_CT_PERF == 1 | CON_CT_PERF == 1 | US_PERF == 1 ~ 2L,  # advanced
    PLAIN_XRAY == 1                                        ~ 1L,  # plain film
    TRUE                                                  ~ 0L    # none
  )
)

## ---- STEP 2: visual evidence (binary M only) -------------------------
## Under the sharp null + monotonicity, the treated "never-taker" mass
## P(Y=y, M=0 | D=1) should sit BELOW the control mass for every y.
## Bumps above the control line = evidence of a non-imaging channel.
partial_density_plot(
  df = final, d = TREAT, m = "any_img", y = "ln_ED_LOS",
  num_Ybins = 5, plot_nts = TRUE,
  reg_formula = REG_IV,
  density_1_label = "Treated: P(Y, no imaging | D=1)",
  density_0_label = "Control: P(Y, no imaging | D=0)"
)

## ---- STEP 3: test the sharp null (imaging count as mediator) ---------
res_count <- test_sharp_null(
  df = final, d = TREAT, m = "img_bin", y = "ln_ED_LOS",
  method = "CS",              # Cox-Shi: recommended default
  num_Ybins = 5,
  reg_formula = REG_IV,       # IV mode: batched ~ batch.tendency
  cluster = CLUSTER
)
res_count$pval                 # smallest alpha at which the null rejects

## ---- STEP 4: how large is the non-imaging channel? -------------------
## Pooled lower bound on the fraction of patients whose imaging is
## unchanged by batching but whose LOS is still affected.
lb_count <- lb_frac_affected(
  df = final, d = TREAT, m = "img_bin", y = "ln_ED_LOS",
  num_Ybins = 5, at_group = NULL,      # NULL = pool across always-taker groups
  reg_formula = REG_IV
)
lb_count

## ---- STEP 5: robustness to monotonicity violations -------------------
## Allow a share of "defiers" (patients batching pushes to FEWER images).
## Re-run raising max_defiers_share until the test no longer rejects; the
## breakpoint is your robustness statement (cf. their "defiers per complier").
test_sharp_null(
  df = final, d = TREAT, m = "img_bin", y = "ln_ED_LOS",
  method = "CS", num_Ybins = 5, reg_formula = REG_IV, cluster = CLUSTER,
  max_defiers_share = 0.02
)$pval

## ---- STEP 6 (optional): modality mediator ----------------------------
## Tests full mediation through imaging *type* (captures turnaround),
## not just count.
test_sharp_null(
  df = final, d = TREAT, m = "img_mode", y = "ln_ED_LOS",
  method = "CS", num_Ybins = 5, reg_formula = REG_IV, cluster = CLUSTER
)$pval

## ---- STEP 7 (optional): combination of mechanisms --------------------
## Pass a vector to m to test whether count + modality *together* explain
## the effect (their grandmother + relationship-quality analogue).
test_sharp_null(
  df = final, d = TREAT, m = c("img_bin", "img_mode"), y = "ln_ED_LOS",
  method = "CS", num_Ybins = 5, reg_formula = REG_IV, cluster = CLUSTER
)$pval

## ---- notes -----------------------------------------------------------
## * Log vs level of LOS is irrelevant: Y enters only via quantile bins.
## * Estimand is over instrument-compliers (batching moved by tendency).
## * Cell-count heuristic (>=15 per Y x M x D cell) is easy at ED sample
##   sizes; you could raise num_Ybins if you want a finer Y partition.
## * For an OLS-adjusted robustness check that treats batching as
##   unconfounded given controls, swap REG_IV for:
##       paste0("~ ", TREAT, " + ", LINCTRL)












### STOP




library(fixest)
library(marginaleffects)
library(dplyr)
library(tibble)
library(purrr)

# ---- shared spec (matches Table 4 full-controls) ---------------------------
PRECISION <- paste(
  "complaint_esi","race","GENDER","PROVIDER_SEX","capacity_level",
  "tachycardic","tachypneic","febrile","hypotensive",
  "lab.tendency","admit.tendency","age","EXPERIENCE","hrs_in_shift", 
  "dayofweekt", "month_of_year",
  sep = " + ")
FE <- "0"   # absorbed; precision controls enter linearly above

# ---- one outcome: 2SLS + 2SRI ----------------------------------------------
run_iv_pair <- function(y, type = c("continuous","count","binary"),
                        data = final, link = "probit") {
  type <- match.arg(type)
  
  # First stage (full controls) -> control-function residual
  fs <- feols(as.formula(paste0("batched ~ batch.tendency + ", PRECISION, " | ", FE)),
              data = data, vcov = "HC1")
  d  <- data
  d$cf_resid <- resid(fs)
  
  # 2SLS (full controls)
  tsls <- feols(as.formula(paste0(y, " ~ ", PRECISION, " | ", FE,
                                  " | batched ~ batch.tendency")),
                data = d, vcov = "HC1")
  
  # 2SRI second stage: batched + residual (NOT batched_hat)
  cf_form <- as.formula(paste0(y, " ~ batched + cf_resid + ", PRECISION, " | ", FE))
  cf <- switch(type,
               continuous = feols(cf_form, data = d, vcov = "HC1"),
               count      = fepois(cf_form, data = d, vcov = "HC1"),
               binary     = feglm(cf_form, data = d, family = binomial(link), vcov = "HC1"))
  
  # 2SRI effect on the outcome scale (AME/avg contrast of batched 0->1)
  if (type == "continuous") {
    ct <- coeftable(cf)["batched", ]
    ame_est <- ct["Estimate"]; ame_se <- ct["Std. Error"]
    ame_p   <- ct[grep("Pr", names(ct))[1]]
  } else {
    ac <- avg_comparisons(cf, variables = "batched")
    ame_est <- ac$estimate[1]; ame_se <- ac$std.error[1]; ame_p <- ac$p.value[1]
  }
  
  ct_cf <- coeftable(cf); pcol <- grep("Pr", colnames(ct_cf))[1]
  
  tibble(
    outcome        = y,
    type           = type,
    n              = nobs(cf),
    first_stage_F  = fitstat(tsls, "ivf")[[1]]$stat,
    
    tsls_coef      = coef(tsls)["fit_batched"],
    tsls_se        = se(tsls)["fit_batched"],
    tsls_p         = coeftable(tsls)["fit_batched", pcol],
    
    sri_ame        = unname(ame_est),
    sri_ame_se     = unname(ame_se),
    sri_ame_p      = unname(ame_p),
    
    sri_index_coef = coef(cf)["batched"],
    sri_index_se   = se(cf)["batched"],
    
    cf_resid_coef  = coef(cf)["cf_resid"],       # endogeneity test:
    cf_resid_se    = se(cf)["cf_resid"],         # significant => batched endogenous,
    cf_resid_p     = ct_cf["cf_resid", pcol]     #               2SRI warranted
  )
}

# ---- run the four Part-1 outcomes ------------------------------------------
specs <- tribble(
  ~y,                 ~type,
  "ln_disp_time",     "continuous",
  "ln_ED_LOS",        "continuous",
  "imgTests",         "count",
  "RTN_72_HR_ADMIT",  "binary")

iv_results <- pmap_dfr(specs, ~ run_iv_pair(..1, ..2))

# ---- readable comparison ----------------------------------------------------
iv_results %>%
  transmute(
    Outcome        = outcome,
    `First-stage F`= sprintf("%.1f", first_stage_F),
    `2SLS`         = sprintf("%.4f (%.4f)", tsls_coef, tsls_se),
    `2SRI (AME)`   = sprintf("%.4f (%.4f)", sri_ame, sri_ame_se),
    `CF resid p`   = sprintf("%.3f", cf_resid_p),
    N = n) %>%
  print(width = Inf)



library(fixest)
library(marginaleffects)
library(dplyr)
library(tibble)
library(purrr)

## ---- control structure (full controls) --------------------------------
FE       <- "0"
LINCTRL  <- "dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX + capacity_level + tachycardic + tachypneic + febrile + hypotensive + lab.tendency + admit.tendency + age + EXPERIENCE + hrs_in_shift"

## ---- p10->p90 contrast of a 0/1 regressor on the response scale --------
## (equals coef for a linear model; the AME for Poisson/probit)
contrast_01 <- function(model, dat, treat = "batched") {
  d0 <- dat; d0[[treat]] <- 0
  d1 <- dat; d1[[treat]] <- 1
  p0 <- predict(model, newdata = d0, type = "response")
  p1 <- predict(model, newdata = d1, type = "response")
  mean(p1 - p0, na.rm = TRUE)
}

## ---- one outcome: 2SLS vs 2SRI (analytic SEs) --------------------------
run_iv_compare <- function(data, y_var, type = c("continuous","count","binary"),
                           link = "probit") {
  type <- match.arg(type)
  d <- as.data.frame(data)
  
  # first stage (full controls) -> control-function residual
  fs <- feols(as.formula(paste0("batched ~ batch.tendency + ", LINCTRL, " | ", FE)),
              data = d, vcov = "HC1")
  d$cf_resid <- resid(fs)
  
  # 2SLS (full controls)
  tsls <- feols(as.formula(paste0(y_var, " ~ ", LINCTRL, " | ", FE,
                                  " | batched ~ batch.tendency")),
                data = d, vcov = "HC1")
  b_2sls  <- coef(tsls)["fit_batched"]
  se_2sls <- se(tsls)["fit_batched"]
  fstat   <- fitstat(tsls, "ivf")[[1]]$stat
  
  # 2SRI second stage
  cf_form <- as.formula(paste0(y_var, " ~ batched + cf_resid + ", LINCTRL, " | ", FE))
  
  if (type == "continuous") {
    cf <- feols(cf_form, data = d, vcov = "HC1")
    est_2sri <- coef(cf)["batched"]          # identical to 2SLS by construction
    se_2sri  <- se_2sls                       # report the 2SLS SE (analytically correct)
  } else if (type == "count") {
    cf <- fepois(cf_form, data = d, vcov = "HC1")
    ac <- avg_comparisons(cf, variables = "batched")
    est_2sri <- ac$estimate; se_2sri <- ac$std.error   # HC1 placeholder -> bootstrap
  } else {
    cf <- feglm(cf_form, data = d, family = binomial(link), vcov = "HC1")
    ac <- avg_comparisons(cf, variables = "batched")
    est_2sri <- ac$estimate; se_2sri <- ac$std.error
  }
  
  # Wu-Hausman endogeneity test = t-test on the control-function residual
  ct <- coeftable(cf); pcol <- grep("Pr", colnames(ct))[1]
  haus_p <- ct["cf_resid", pcol]
  
  tibble(
    outcome = y_var, type = type,
    first_stage_F = unname(fstat),
    b_2sls = unname(b_2sls), se_2sls = unname(se_2sls),
    est_2sri = unname(est_2sri), se_2sri = unname(se_2sri),
    hausman_p = unname(haus_p),
    N_2sls = nobs(tsls), N_2sri = nobs(cf)
  )
}

## ---- outcome list ------------------------------------------------------
specs <- tribble(
  ~outcome,          ~type,
  "ln_treat_time",   "continuous",
  "ln_disp_time",    "continuous",
  "ln_ED_LOS",       "continuous",
  "imgTests",        "count",
  "RTN_72_HR_ADMIT", "binary",
  "RTN_72_HR",       "binary",
  "PLAIN_XRAY",      "binary",
  "US_PERF",         "binary",
  "NON_CON_CT_PERF", "binary",
  "CON_CT_PERF",     "binary",
  "admit",           "binary",
  "upgrade",         "binary",
  "downgrade",       "binary"
) %>% filter(outcome %in% names(final))

res <- pmap_dfr(list(specs$outcome, specs$type),
                ~ run_iv_compare(final, ..1, ..2))

## ---- readable comparison table ----------------------------------------
fmt <- function(b, s, d = 3) sprintf(paste0("%.", d, "f (%.", d, "f)"), b, s)

res_tbl <- res %>% transmute(
  Outcome        = outcome,
  `First-stage F`= sprintf("%.1f", first_stage_F),
  `2SLS`         = fmt(b_2sls, se_2sls),
  `2SRI (AME)`   = fmt(est_2sri, se_2sri),
  `Hausman p`    = sprintf("%.3f", hausman_p),
  N              = N_2sls
)
print(res_tbl, n = Inf)


## ---- bootstrap SE for the 2SRI AME (count + binary only) ---------------
boot_2sri_se <- function(data, y_var, type, B = 199, seed = 1) {
  set.seed(seed)
  d <- as.data.frame(data); n <- nrow(d)
  cf_form <- as.formula(paste0(y_var, " ~ batched + cf_resid + ", LINCTRL, " | ", FE))
  fam <- if (type == "binary") binomial("probit") else NULL
  
  est <- vapply(seq_len(B), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    db  <- d[idx, , drop = FALSE]
    out <- tryCatch({
      fs <- feols(as.formula(paste0("batched ~ batch.tendency + ", LINCTRL, " | ", FE)), db)
      db$cf_resid <- resid(fs)
      m <- if (type == "count") fepois(cf_form, db) else feglm(cf_form, db, family = fam)
      contrast_01(m, db)
    }, error = function(e) NA_real_)
    out
  }, numeric(1))
  
  c(se = sd(est, na.rm = TRUE), valid = sum(is.finite(est)))
}

nl <- specs %>% filter(type %in% c("count","binary"))
boot <- pmap_dfr(list(nl$outcome, nl$type), function(y, t) {
  bs <- boot_2sri_se(final, y, t, B = 199)
  tibble(outcome = y, se_boot = bs["se"], valid_reps = bs["valid"])
})

## ---- merge bootstrap SEs into the table --------------------------------
res_final <- res %>% left_join(boot, by = "outcome") %>%
  mutate(se_2sri_report = coalesce(se_boot, se_2sri))

res_final %>% transmute(
  Outcome        = outcome,
  `First-stage F`= sprintf("%.1f", first_stage_F),
  `2SLS`         = fmt(b_2sls, se_2sls),
  `2SRI (AME)`   = fmt(est_2sri, se_2sri_report),
  `Hausman p`    = sprintf("%.3f", hausman_p),
  N              = N_2sls
) %>% print(n = Inf)




fmt <- function(b, s, d = 3) ifelse(is.na(s), sprintf(paste0("%.", d, "f"), b),
                                    sprintf(paste0("%.", d, "f (%.", d, "f)"), b, s))

# tier logic for the note flag (prevalence-based)
tier <- function(o) dplyr::case_when(
  o %in% c("PLAIN_XRAY")                 ~ "\\ddagger",  # LPM overshoots, probit preferred
  o %in% c("RTN_72_HR_ADMIT","RTN_72_HR")~ "\\dagger",   # rare, LPM preferred
  TRUE                                    ~ "")

table_iv <- res_final %>%
  mutate(
    flag  = tier(outcome),
    `2SLS`       = fmt(b_2sls,  se_2sls),
    `2SRI`       = paste0(fmt(est_2sri, se_2sri_report), flag)
  ) %>%
  select(outcome, type, `First-stage F` = first_stage_F,
         `2SLS`, `2SRI`, `Hausman p` = hausman_p, N = N_2sls)

print(table_iv, n = Inf)


library(ggplot2); library(ggh4x); library(dplyr)

lab <- c(ln_treat_time="Ln treatment time", ln_disp_time="Ln time to disposition",
         ln_ED_LOS="Ln ED length of stay", imgTests="Distinct imaging tests",
         PLAIN_XRAY="Any plain x-ray", US_PERF="Any ultrasound",
         NON_CON_CT_PERF="Any non-contrast CT", CON_CT_PERF="Any contrast CT",
         admit="Admission", RTN_72_HR_ADMIT="72-hr return w/ admit",
         RTN_72_HR="72-hr return")
panel <- c(continuous="Timing (log points)", count="Imaging count (tests)",
           binary="Binary outcomes (percentage points)")

long <- bind_rows(
  res_final %>% transmute(outcome, type, estimator="2SLS", est=b_2sls,   se=se_2sls),
  res_final %>% transmute(outcome, type, estimator="2SRI", est=est_2sri, se=se_2sri_report)
) %>%
  mutate(k = if_else(type=="binary", 100, 1),
         p = est*k, lo = (est-1.96*se)*k, hi = (est+1.96*se)*k,
         estimator = factor(estimator, levels=c("2SLS","2SRI")),
         group   = factor(panel[type], levels=panel),
         Outcome = factor(lab[outcome], levels=rev(lab)),
         flag = if_else(outcome=="PLAIN_XRAY" & estimator=="2SRI", "\u2020", ""))

ggplot(long, aes(p, Outcome, colour=estimator, shape=estimator)) +
  geom_vline(xintercept=0, linewidth=0.4, colour="grey60") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=0,
                 position=position_dodge(0.55), linewidth=0.5, na.rm=TRUE) +
  geom_point(aes(fill=estimator), position=position_dodge(0.55),
             size=2.5, stroke=0.9, na.rm=TRUE) +
  facet_grid2(rows=vars(group), scales="free", independent="x",
              space="free_y", switch="y") +
  scale_colour_manual(values=c("2SLS"="#2a5db0","2SRI"="#158a63")) +
  scale_fill_manual(values  =c("2SLS"="#2a5db0","2SRI"="white")) +
  scale_shape_manual(values =c("2SLS"=21,"2SRI"=21)) +
  labs(title="Effect of batching on patient outcomes: 2SLS vs. 2SRI",
       x="Effect of batching (log points, test count, or percentage points by panel)",
       y=NULL, colour=NULL, fill=NULL, shape=NULL) +
  theme_bw(base_size = 13) +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 11),
    strip.text = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  )




library(fixest)
library(ManyIV)
library(dplyr)
library(purrr)
library(tibble)

## ---- controls: same variation as the main 2SLS/2SRI spec --------------
## ManyIV needs controls as explicit regressors (no FE absorption), so wrap
## categoricals in factor(); feols absorbs the identical set as FE.
UJIVE_CTRL <- paste(
  "factor(dayofweekt)","factor(month_of_year)","factor(complaint_esi)",
  "factor(race)","factor(GENDER)","factor(capacity_level)",
  "tachycardic","tachypneic","febrile","hypotensive",
  "age","hrs_in_shift",
  sep = " + ")

FE      <- "dayofweekt + month_of_year + complaint_esi + race + GENDER + PROVIDER_SEX + capacity_level"
LINCTRL <- "tachycardic + tachypneic + febrile + hypotensive + lab.tendency + admit.tendency + age + EXPERIENCE + hrs_in_shift"

## ---- one outcome: UJIVE | 2SLS(leniency) | 2SLS(provider IVs) | OLS ----
run_ujive_row <- function(data, y_var) {
  d <- as.data.frame(data)
  
  ## (1) UJIVE — one-step, provider dummies as instruments (leave-one-out)
  uj <- tryCatch({
    f <- as.formula(paste0(y_var, " ~ batched + ", UJIVE_CTRL,
                           " | factor(ED_PROVIDER) + ", UJIVE_CTRL))
    fit <- ujive(f, data = d)
    c(b = fit$estimate["ujive","estimate"],
      s = fit$estimate["ujive","se_hte"])
  }, error = function(e) { message("UJIVE failed ", y_var, ": ", e$message); c(b=NA,s=NA) })
  
  ## (2) 2SLS with the single leave-out leniency measure (your main IV)
  t_len <- feols(as.formula(paste0(y_var, " ~ ", LINCTRL, " | ", FE,
                                   " | batched ~ batch.tendency")),
                 data = d, vcov = "HC1")
  
  ## (3) 2SLS instrumenting with the full set of provider dummies (many-IV)
  t_many <- feols(as.formula(paste0(y_var, " ~ ", LINCTRL, " | ", FE,
                                    " | batched ~ factor(ED_PROVIDER)")),
                  data = d, vcov = "HC1")
  
  ## (4) OLS
  ols <- feols(as.formula(paste0(y_var, " ~ batched + ", LINCTRL, " | ", FE)),
               data = d, vcov = "HC1")
  
  tibble(
    outcome = y_var,
    b_ujive = unname(uj["b"]),                    se_ujive = unname(uj["s"]),
    b_len   = coef(t_len)["fit_batched"],         se_len   = se(t_len)["fit_batched"],
    b_many  = coef(t_many)["fit_batched"],        se_many  = se(t_many)["fit_batched"],
    b_ols   = coef(ols)["batched"],               se_ols   = se(ols)["batched"],
    N       = nobs(t_len)
  )
}

## ---- outcomes (same set as the main table) ----------------------------
ujive_specs <- c("ln_treat_time","ln_disp_time","ln_ED_LOS","imgTests",
                 "RTN_72_HR_ADMIT","RTN_72_HR","PLAIN_XRAY","US_PERF",
                 "NON_CON_CT_PERF","CON_CT_PERF","admit")
ujive_specs <- ujive_specs[ujive_specs %in% names(final)]

ujive_tbl <- map_dfr(ujive_specs, ~ run_ujive_row(final, .x))
print(ujive_tbl, n = Inf, width = Inf)


library(fixest)
library(marginaleffects)
library(dplyr)
library(tibble)
library(purrr)

## ---- WITHIN-PHYSICIAN control structure ------------------------------------
## Drop physician-constant controls (absorbed by ED_PROVIDER FE):
## lab.tendency, admit.tendency, EXPERIENCE, PROVIDER_SEX.
## Keep patient/encounter-level controls; add ED_PROVIDER to the FE.
W_LINCTRL <- "complaint_esi + race + GENDER + capacity_level + tachycardic + tachypneic + febrile + hypotensive + age + hrs_in_shift + dayofweekt + month_of_year + ED_PROVIDER"
W_FE      <- "0"

contrast_01 <- function(model, dat, treat = "batched") {
  d0 <- dat; d0[[treat]] <- 0
  d1 <- dat; d1[[treat]] <- 1
  p0 <- predict(model, newdata = d0, type = "response")
  p1 <- predict(model, newdata = d1, type = "response")
  mean(p1 - p0, na.rm = TRUE)
}

run_within_compare <- function(data, y_var, type = c("continuous","count","binary"),
                               link = "probit") {
  type <- match.arg(type)
  d <- as.data.frame(data)
  
  ## first stage: within-physician complaint instrument, physician FE
  fs <- feols(as.formula(paste0(
    "batched ~ batch.tendency.complaint + ", W_LINCTRL, " | ", W_FE)),
    data = d, vcov = "HC1")
  d$cf_resid <- resid(fs)
  
  ## 2SLS (within-physician)
  tsls <- feols(as.formula(paste0(y_var, " ~ ", W_LINCTRL, " | ", W_FE,
                                  " | batched ~ batch.tendency.complaint")), data = d, vcov = "HC1")
  b_2sls  <- coef(tsls)["fit_batched"]; se_2sls <- se(tsls)["fit_batched"]
  fstat   <- fitstat(tsls, "ivf")[[1]]$stat
  
  ## 2SRI second stage
  cf_form <- as.formula(paste0(y_var, " ~ batched + cf_resid + ", W_LINCTRL, " | ", W_FE))
  if (type == "continuous") {
    cf <- feols(cf_form, data = d, vcov = "HC1")
    est_2sri <- coef(cf)["batched"]; se_2sri <- se_2sls
  } else if (type == "count") {
    cf <- fepois(cf_form, data = d, vcov = "HC1")
    ac <- avg_comparisons(cf, variables = "batched")
    est_2sri <- ac$estimate; se_2sri <- ac$std.error
  } else {
    cf <- feglm(cf_form, data = d, family = binomial(link), vcov = "HC1")
    ac <- avg_comparisons(cf, variables = "batched")
    est_2sri <- ac$estimate; se_2sri <- ac$std.error
  }
  
  ct <- coeftable(cf); pcol <- grep("Pr", colnames(ct))[1]
  tibble(
    outcome = y_var, type = type,
    first_stage_F = unname(fstat),
    b_2sls = unname(b_2sls), se_2sls = unname(se_2sls),
    est_2sri = unname(est_2sri), se_2sri = unname(se_2sri),
    hausman_p = unname(ct["cf_resid", pcol]),
    N_2sls = nobs(tsls), N_2sri = nobs(cf)
  )
}

specs <- tribble(
  ~outcome,          ~type,
  "ln_treat_time",   "continuous",
  "ln_disp_time",    "continuous",
  "ln_ED_LOS",       "continuous",
  "imgTests",        "count",
  "RTN_72_HR_ADMIT", "binary",
  "RTN_72_HR",       "binary",
  "PLAIN_XRAY",      "binary",
  "US_PERF",         "binary",
  "NON_CON_CT_PERF", "binary",
  "CON_CT_PERF",     "binary",
  "admit",           "binary",
  "upgrade",         "binary",
  "downgrade",       "binary"
) %>% filter(outcome %in% names(final))

within_res <- pmap_dfr(list(specs$outcome, specs$type),
                       ~ run_within_compare(final, ..1, ..2))

within_res %>% transmute(
  Outcome = outcome,
  `First-stage F` = sprintf("%.1f", first_stage_F),
  `2SLS (within)` = sprintf("%.3f (%.3f)", b_2sls, se_2sls),
  `2SRI (within)` = sprintf("%.3f (%.3f)", est_2sri, se_2sri),
  `Hausman p` = sprintf("%.3f", hausman_p),
  N = N_2sls
) %>% print(n = Inf)

run_within_ols <- function(data, y_var, type = c("continuous","count","binary"),
                           link = "probit") {
  type <- match.arg(type)
  d <- as.data.frame(data)
  form <- as.formula(paste0(y_var, " ~ batched + ", W_LINCTRL))  # W_FE = "0"
  
  if (type == "continuous") {
    m <- feols(form, data = d, vcov = "HC1")
    est <- coef(m)["batched"]; se <- se(m)["batched"]
  } else if (type == "count") {
    m <- fepois(form, data = d, vcov = "HC1")
    ac <- avg_comparisons(m, variables = "batched")
    est <- ac$estimate; se <- ac$std.error
  } else {
    m <- feglm(form, data = d, family = binomial(link), vcov = "HC1")
    ac <- avg_comparisons(m, variables = "batched")
    est <- ac$estimate; se <- ac$std.error
  }
  tibble(outcome = y_var, b_ols = unname(est), se_ols = unname(se), N_ols = nobs(m))
}

within_ols <- pmap_dfr(list(specs$outcome, specs$type),
                       ~ run_within_ols(final, ..1, ..2))

## merge OLS into the within results
within_all <- within_res %>% left_join(within_ols, by = "outcome")

within_all %>% transmute(
  Outcome = outcome,
  `First-stage F` = sprintf("%.1f", first_stage_F),
  `Within OLS`    = sprintf("%.3f (%.3f)", b_ols, se_ols),
  `Within 2SLS`   = sprintf("%.3f (%.3f)", b_2sls, se_2sls),
  `Within 2SRI`   = sprintf("%.3f (%.3f)", est_2sri, se_2sri),
  `Hausman p`     = sprintf("%.3f", hausman_p),
  N = N_2sls
) %>% print(n = Inf)




###### Stratified by admission comment

library(fixest); library(dplyr); library(purrr); library(tibble)

## same control structure as the main models
CTRL <- "tachycardic + tachypneic + febrile + hypotensive + age + hrs_in_shift + EXPERIENCE + PROVIDER_SEX + lab.tendency + admit.tendency + capacity_level"
FE   <- "dayofweekt + month_of_year + complaint_esi + race + GENDER"

## descriptive OLS of outcome on batched, within an admission subgroup
run_strat <- function(y, subset_expr, label) {
  d <- final %>% filter({{ subset_expr }})
  m <- feols(as.formula(paste0(y, " ~ batched + ", CTRL, " | ", FE)),
             data = d, vcov = "HC1")
  tibble(outcome = y, group = label,
         b = coef(m)["batched"], se = se(m)["batched"], n = nobs(m))
}

## outcomes that make sense to stratify (LOS is the reviewer's main concern)
outs <- c("ln_disp_time", "ln_ED_LOS", "imgTests")

strat <- bind_rows(
  map_dfr(outs, ~ run_strat(.x, admit == 1, "Admitted")),
  map_dfr(outs, ~ run_strat(.x, admit == 0, "Discharged"))
)

strat %>%
  mutate(est = sprintf("%.3f (%.3f)", b, se)) %>%
  select(outcome, group, est, n) %>%
  tidyr::pivot_wider(names_from = group, values_from = c(est, n)) %>%
  print(width = Inf)









## =====================================================================
## ASSOCIATIONAL mediation: is imaging a plausible channel for
## batching -> LOS?   *** NOT CAUSAL ***
##
## This re-expresses conditional associations (severity-adjusted). It does
## NOT remove unobserved-severity confounding of the imaging->LOS path, and
## an SEM would do exactly the same thing. Use it as a CONSISTENCY /
## plausibility argument, not as evidence of mechanism.
##
## Because severity pushes BOTH imaging and LOS up, the associational
## b-path is biased UPWARD -> the association-based indirect effect is an
## UPPER-ish bound on the true mediated share. State it that way.
## =====================================================================

library(fixest); library(dplyr); library(purrr); library(tibble)

TREAT   <- "batched"; MED <- "imgTests"; OUT <- "ln_ED_LOS"; FE <- "0"
CLUSTER <- NULL                      # <- set to your physician-ID column
LINCTRL <- paste(
  "dayofweekt","month_of_year","complaint_esi","race","GENDER",
  "PROVIDER_SEX","capacity_level","tachycardic","tachypneic","febrile",
  "hypotensive","lab.tendency","admit.tendency","age","EXPERIENCE",
  "hrs_in_shift", sep = " + ")

## your credible IV numbers (fill in from the earlier run)
ALPHA_IV <- 0.911   # CAUSAL batching -> imaging count (IV, complier LATE)
TOTAL_IV <- 0.604   # CAUSAL batching -> log-LOS       (IV, complier LATE)

## ---- associational path decomposition -------------------------------
assoc_mediation <- function(d) {
  d <- as.data.frame(d)
  
  ## a-path (associational): batching -> imaging
  a  <- coef(feols(as.formula(paste0(MED," ~ ",TREAT," + ",LINCTRL," | ",FE)), d))[TREAT]
  
  ## outcome model: b-path (imaging -> LOS | batching, X) and direct (c')
  yf <- feols(as.formula(paste0(OUT," ~ ",MED," + ",TREAT," + ",LINCTRL," | ",FE)), d)
  b       <- coef(yf)[MED]           # <- THE b-path. 100*b = % change in LOS per test.
  direct  <- coef(yf)[TREAT]         # c'
  
  ## total association
  c_tot <- coef(feols(as.formula(paste0(OUT," ~ ",TREAT," + ",LINCTRL," | ",FE)), d))[TREAT]
  
  indirect <- unname(a * b)          # == c_tot - direct (adds up in the linear model)
  
  tibble(
    a_assoc      = unname(a),
    b_assoc      = unname(b),
    total_assoc  = unname(c_tot),
    direct_assoc = unname(direct),
    indirect_assoc      = indirect,
    prop_mediated_assoc = indirect / unname(c_tot),
    ## coherence bridge: CAUSAL alpha (IV) x ASSOCIATIONAL per-test b
    implied_indirect_IVa   = ALPHA_IV * unname(b),
    share_of_causal_total  = ALPHA_IV * unname(b) / TOTAL_IV
  )
}

pt <- assoc_mediation(final)
cat("\n--- associational decomposition (descriptive) ---\n"); print(pt)

## ---- cluster bootstrap CIs (resample physicians as blocks) ----------
boot_assoc <- function(d, B = 999, seed = 1, cluster = CLUSTER) {
  set.seed(seed); d <- as.data.frame(d)
  ids <- if (is.null(cluster)) NULL else split(seq_len(nrow(d)), d[[cluster]])
  draws <- map_dfr(seq_len(B), function(b) {
    idx <- if (is.null(ids)) sample.int(nrow(d), replace = TRUE)
    else unlist(ids[sample.int(length(ids), replace = TRUE)], use.names = FALSE)
    tryCatch(assoc_mediation(d[idx, , drop = FALSE]), error = function(e) NULL)
  })
  draws %>% summarise(across(everything(), list(
    lo = ~quantile(.x, .025, na.rm = TRUE),
    hi = ~quantile(.x, .975, na.rm = TRUE))))
}
cat("\n--- bootstrap 95% CIs ---\n"); print(boot_assoc(final, B = 999))

## ---- reference: OLS b-path vs the exploded IV beta ------------------
## Shows the IV 5.39 was weak-instrument blow-up, not a real per-test effect.
cat("\nassociational per-test LOS effect (b):", round(pt$b_assoc, 4),
    "  => ", round(100 * pt$b_assoc, 2), "% longer LOS per additional test\n")

