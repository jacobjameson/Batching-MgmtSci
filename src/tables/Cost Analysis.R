#=========================================================================
# COST-BENEFIT ANALYSIS OF DISCRETIONARY BATCH ORDERING
# Conservative Approach: X-Ray Costs Only (Statistically Significant Effect)
# 
# Citations for cost parameters:
#
# ED BED-HOUR COSTS:
#   - Schreyer KE, Martin R. (2017). "The Economics of an Admissions Holding 
#     Unit." Western Journal of Emergency Medicine, 18(4):553-558.
#     --> Found ED personnel costs of $58.20/bed-hour (2010-2011 data)
#
#   - Canellas MM, et al. (2024). "Measurement of Cost of Boarding in the 
#     Emergency Department Using Time-Driven Activity-Based Costing." 
#     Annals of Emergency Medicine, 84(4):410-419.
#     --> Found daily ED boarding costs of $1,856 (~$77/hour)
#
# X-RAY COSTS:
#   - Journal of the American College of Radiology (2025). "Practice Expense 
#     and Its Impact on Radiology Reimbursement."
#     --> CPT 71046 (2-view chest X-ray) in ED setting: $98.53 technical component
#
#   - CMS Medicare Physician Fee Schedule 2024
#     --> Professional + facility components for chest radiograph: $60-100
#
#   - Iyeke et al. (2022). "Reducing Unnecessary 'Admission' Chest X-rays." 
#     Cureus, 14(10):e29817.
#     --> CXR imaging and interpretation costs $150-1,200 (hospital setting)
#
#=========================================================================

sink('outputs/tables/costs.txt')

#=========================================================================
# STEP 1: Establish the key parameters
#=========================================================================

# Complier shares (from our local linear regression analysis)
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
n_batched_compliers <- n_batched - n_always_takers

cat("═══════════════════════════════════════════════════════════════════\n")
cat("       COST-BENEFIT ANALYSIS OF DISCRETIONARY BATCH ORDERING       \n")
cat("═══════════════════════════════════════════════════════════════════\n")

cat("\n=== STEP 1: SAMPLE COMPOSITION ===\n")
cat("Total encounters in analytical sample:", format(n_total, big.mark=","), "\n")
cat("Total batched encounters:", format(n_batched, big.mark=","), 
    "(", round(n_batched/n_total*100, 1), "%)\n")
cat("\nBy compliance type:\n")
cat("  Compliers:", format(n_compliers, big.mark=","), "(", round(pi_c*100, 1), "%)\n")
cat("  Always-takers:", format(n_always_takers, big.mark=","), "(", round(pi_a*100, 1), "%)\n")
cat("  Never-takers:", format(n_never_takers, big.mark=","), "(", round(pi_n*100, 1), "%)\n")
cat("\nPolicy-relevant subset:\n")
cat("  Batched compliers (potentially avoidable):", format(n_batched_compliers, big.mark=","), "\n")
cat("  As % of total sample:", round(n_batched_compliers/n_total*100, 1), "%\n")
cat("  As % of all batched:", round(n_batched_compliers/n_batched*100, 1), "%\n")

#=========================================================================
# STEP 2: Effect sizes from UJIVE estimates (Table 4, Column 5)
#=========================================================================

# LOS effect
effect_LOS_log <- 0.503           # log points (p < 0.001)

# Imaging effect - X-RAY ONLY (statistically significant)
effect_xray <- 0.959              # additional X-rays (p < 0.001)

# Note: Other modalities not statistically significant in UJIVE specification
# Ultrasound: 0.087 (n.s.), CT w/o contrast: 0.053 (n.s.), CT w/ contrast: 0.075 (n.s.)

# Baseline LOS (sequenced patients)
baseline_LOS_log <- 5.490
baseline_LOS_min <- exp(baseline_LOS_log)  # ~242 minutes

# Calculate LOS changes
LOS_pct_increase <- exp(effect_LOS_log) - 1  # 65.4%
LOS_min_increase <- baseline_LOS_min * LOS_pct_increase
LOS_hrs_increase <- LOS_min_increase / 60

cat("\n=== STEP 2: EFFECT SIZES (UJIVE, Full Controls) ===\n")
cat("\nLength of Stay:\n")
cat("  Baseline LOS (sequenced patients):", round(baseline_LOS_min), "minutes\n")
cat("  LOS increase from batching:", round(LOS_pct_increase*100, 1), "%\n")
cat("  LOS increase:", round(LOS_min_increase), "minutes =", 
    round(LOS_hrs_increase, 2), "hours\n")
cat("\nImaging (statistically significant effects only):\n")
cat("  Additional X-rays per batched complier:", round(effect_xray, 3), "(p < 0.001)\n")
cat("  [Other modalities not statistically significant]\n")

#=========================================================================
# STEP 3: Cost parameters from literature (WITH VERIFIED CITATIONS)
#=========================================================================

# X-RAY COSTS
# Source: Dabus et al. (2025) JACR "Practice Expense and Its Impact on 
#         Radiology Reimbursement" doi:10.1016/j.jacr.2025.02.047
#   - CPT 71046 (2-view chest X-ray) in ED setting:
#     - Technical component (HOPPS): $98.53
#     - Professional component (radiologist read): $30-40
#     - Total Medicare-allowed: ~$120-140
#   - We use $100-160 range (mean $130) for sensitivity analysis
cost_xray <- c(low = 100, mean = 130, high = 160)

# ED BED-HOUR COSTS
# Sources:
#   - Schreyer & Martin (2017) WJEM: $58.20/bed-hour in 2010-11
#     Inflation adjusted to 2024: $58.20 × 1.42 = ~$83/hour
#   - Canellas et al. (2024) Ann Emerg Med: $1,856/day = $77/hour
#   - We use $75-100 range based on these two studies
cost_bedhour <- c(low = 75, mean = 85, high = 100)

cat("\n=== STEP 3: COST PARAMETERS (Literature-Based) ===\n")
cat("\nX-ray costs (Medicare-allowed, ED setting):\n")
cat("  Source: Dabus et al. (2025) JACR doi:10.1016/j.jacr.2025.02.047\n")
cat("  Technical (HOPPS): $98.53 + Professional: $30-40 = $120-140 total\n")
cat("  Range used: $", cost_xray["low"], "- $", cost_xray["high"], 
    " (mean $", cost_xray["mean"], ")\n")
cat("\nED bed-hour costs:\n")
cat("  Source 1: Schreyer & Martin (2017) WJEM - $58.20/hr (2010-11)\n")
cat("            Inflation-adjusted to 2024: ~$83/hr\n")
cat("  Source 2: Canellas et al. (2024) Ann Emerg Med - $77/hr\n")
cat("  Range: $", cost_bedhour["low"], "- $", cost_bedhour["high"], 
    " (mean $", cost_bedhour["mean"], ")\n")

#=========================================================================
# STEP 4: Calculate per-patient costs
#=========================================================================

# Excess imaging cost per batched complier (X-ray only)
imaging_cost_low <- effect_xray * cost_xray["low"]
imaging_cost_mean <- effect_xray * cost_xray["mean"]
imaging_cost_high <- effect_xray * cost_xray["high"]

# Excess capacity cost per batched complier
capacity_cost_low <- LOS_hrs_increase * cost_bedhour["low"]
capacity_cost_mean <- LOS_hrs_increase * cost_bedhour["mean"]
capacity_cost_high <- LOS_hrs_increase * cost_bedhour["high"]

# Total cost per batched complier
total_cost_low <- imaging_cost_low + capacity_cost_low
total_cost_mean <- imaging_cost_mean + capacity_cost_mean
total_cost_high <- imaging_cost_high + capacity_cost_high

cat("\n=== STEP 4: PER-PATIENT COSTS ===\n")
cat("\nExcess imaging cost (X-ray only):\n")
cat("  Low: $", round(imaging_cost_low), "\n")
cat("  Mean: $", round(imaging_cost_mean), "\n")
cat("  High: $", round(imaging_cost_high), "\n")
cat("\nExcess capacity cost:\n")
cat("  Additional bed-hours:", round(LOS_hrs_increase, 2), "\n")
cat("  Low: $", round(capacity_cost_low), "\n")
cat("  Mean: $", round(capacity_cost_mean), "\n")
cat("  High: $", round(capacity_cost_high), "\n")
cat("\nTOTAL cost per batched complier:\n")
cat("  Low: $", round(total_cost_low), "\n")
cat("  Mean: $", round(total_cost_mean), "\n")
cat("  High: $", round(total_cost_high), "\n")
cat("\nCost decomposition (mean estimates):\n")
cat("  Imaging share:", round(imaging_cost_mean/total_cost_mean*100, 1), "%\n")
cat("  Capacity share:", round(capacity_cost_mean/total_cost_mean*100, 1), "%\n")

#=========================================================================
# STEP 5: Scale to study site (Mayo Clinic)
#=========================================================================

# Study period: October 2018 - December 2019 = 15 months
annualization_factor <- 12/15

# Total ED volume during study period
total_ed_volume_study <- 48854

# Annual equivalents
annual_ed_volume <- total_ed_volume_study * annualization_factor
annual_imaging_encounters <- n_total * annualization_factor
annual_batched_compliers <- n_batched_compliers * annualization_factor

# Annual costs
annual_cost_low <- annual_batched_compliers * total_cost_low
annual_cost_mean <- annual_batched_compliers * total_cost_mean
annual_cost_high <- annual_batched_compliers * total_cost_high

cat("\n=== STEP 5: ANNUAL COSTS (STUDY SITE) ===\n")
cat("\nStudy period: 15 months (October 2018 - December 2019)\n")
cat("Total ED encounters (study period):", format(total_ed_volume_study, big.mark=","), "\n")
cat("Imaging-relevant encounters (study period):", format(n_total, big.mark=","), 
    "(", round(n_total/total_ed_volume_study*100, 1), "% of total)\n")
cat("\nAnnualized figures:\n")
cat("  Annual ED volume:", format(round(annual_ed_volume), big.mark=","), "\n")
cat("  Annual imaging-relevant encounters:", format(round(annual_imaging_encounters), big.mark=","), "\n")
cat("  Annual batched compliers:", format(round(annual_batched_compliers), big.mark=","), "\n")
cat("\nANNUAL COST OF DISCRETIONARY BATCHING:\n")
cat("  Mean: $", format(round(annual_cost_mean), big.mark=","), "\n")
cat("  Range: $", format(round(annual_cost_low), big.mark=","), 
    " - $", format(round(annual_cost_high), big.mark=","), "\n")

#=========================================================================
# STEP 6: Scale to different ED sizes
#=========================================================================

# Share of encounters that are imaging-relevant
imaging_share <- n_total / total_ed_volume_study

# Share of imaging encounters that are batched compliers
batched_complier_share <- n_batched_compliers / n_total

# ED size categories (annual volumes)
ed_sizes <- data.frame(
  Type = c("Small (Rural/Community)", 
           "Medium (Suburban)", 
           "Large (Urban)", 
           "Study Site (Mayo Clinic)"),
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
cat("  Imaging-relevant encounters:", round(imaging_share*100, 1), "% of total ED volume\n")
cat("  Batched compliers:", round(batched_complier_share*100, 1), "% of imaging encounters\n")

#=========================================================================
# STEP 7: Create publication-ready table
#=========================================================================

cat("\n\n")
cat("═══════════════════════════════════════════════════════════════════════════════════════\n")
cat("        TABLE: Estimated Annual Cost of Discretionary Batch Ordering by ED Size        \n")
cat("═══════════════════════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("%-28s %12s %12s %15s %22s\n", 
            "ED Type", "Annual Vol.", "Batched", "Annual Cost", "Range"))
cat(sprintf("%-28s %12s %12s %15s %22s\n", 
            "", "", "Compliers", "(Mean)", ""))
cat(paste(rep("─", 95), collapse=""), "\n")

for (i in 1:nrow(ed_sizes)) {
  cat(sprintf("%-28s %12s %12s %15s %22s\n",
              ed_sizes$Type[i],
              format(ed_sizes$Annual_Volume[i], big.mark=","),
              format(ed_sizes$Batched_Compliers[i], big.mark=","),
              paste0("$", format(round(ed_sizes$Cost_Mean[i]), big.mark=",")),
              paste0("($", format(round(ed_sizes$Cost_Low[i]), big.mark=","), 
                     " - $", format(round(ed_sizes$Cost_High[i]), big.mark=","), ")")))
}

cat(paste(rep("─", 95), collapse=""), "\n")

#=========================================================================
# STEP 8: Additional metrics for discussion
#=========================================================================

# Bed-hours freed
annual_bedhours_freed <- annual_batched_compliers * LOS_hrs_increase

# Additional visits that could be served
additional_visits <- annual_bedhours_freed / (baseline_LOS_min / 60)

# Capacity improvement
capacity_improvement_pct <- additional_visits / annual_ed_volume * 100

# Days of ED capacity
days_of_capacity <- annual_bedhours_freed / 24

# Imaging tests avoided (X-rays only)
annual_xrays_avoided <- annual_batched_compliers * effect_xray

cat("\n=== ADDITIONAL METRICS (Study Site, Annualized) ===\n")

cat("\nCapacity implications:\n")
cat("  Bed-hours freed annually:", format(round(annual_bedhours_freed), big.mark=","), "\n")
cat("  Equivalent to:", round(days_of_capacity), "24-hour days of ED capacity\n")
cat("  Additional visits enabled:", format(round(additional_visits), big.mark=","), "\n")
cat("  Capacity improvement:", round(capacity_improvement_pct, 2), "%\n")

cat("\nResource utilization:\n")
cat("  X-rays avoided annually:", format(round(annual_xrays_avoided), big.mark=","), "\n")

cat("\nIntervention scenarios:\n")
cat("  If intervention reduces discretionary batching by 25%:\n")
cat("    Annual savings: $", format(round(annual_cost_mean * 0.25), big.mark=","), "\n")
cat("  If intervention reduces discretionary batching by 50%:\n")
cat("    Annual savings: $", format(round(annual_cost_mean * 0.50), big.mark=","), "\n")

#=========================================================================
# STEP 9: Conservative nature of estimates
#=========================================================================

cat("\n=== CONSERVATIVE NATURE OF ESTIMATES ===\n")
cat("\nExcluded cost categories (would increase estimates):\n")
cat("  1. Non-significant imaging effects (US, CT): would add ~$50-80/patient\n")
cat("  2. Radiologist interpretation time\n")
cat("  3. Physician cognitive burden\n")
cat("  4. Patient time costs and lost productivity\n")
cat("  5. Downstream admission costs (40pp increase × ~$10K/admission)\n")
cat("  6. Incidental finding workup costs\n")
cat("  7. Radiation exposure costs\n")
cat("  8. Spillover congestion effects on other patients\n")

# What would costs be with all imaging (for footnote)
# Using conservative estimates for US ($150) and CT ($350)
effect_us <- 0.087
effect_ct_total <- 0.053 + 0.075  # CT with and without contrast
cost_us <- 150
cost_ct <- 350

imaging_cost_all <- imaging_cost_mean + (effect_us * cost_us) + (effect_ct_total * cost_ct)
total_cost_all <- imaging_cost_all + capacity_cost_mean

cat("\nSensitivity: Including all imaging point estimates:\n")
cat("  Additional US cost: $", round(effect_us * cost_us), "\n")
cat("  Additional CT cost: $", round(effect_ct_total * cost_ct), "\n")
cat("  Total imaging cost would be: $", round(imaging_cost_all), " (vs. $", round(imaging_cost_mean), ")\n")
cat("  Total cost would be: $", round(total_cost_all), " (vs. $", round(total_cost_mean), ")\n")
cat("  Increase:", round((total_cost_all - total_cost_mean)/total_cost_mean * 100, 1), "%\n")

#=========================================================================
# STEP 10: Summary statistics for manuscript
#=========================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("                    SUMMARY FOR MANUSCRIPT                         \n")
cat("═══════════════════════════════════════════════════════════════════\n")

cat("\nKey figures to report:\n")
cat("  Complier share: 20%\n")
cat("  Batched compliers (share of batched that is discretionary): 51%\n")
cat("  Cost per batched complier: $", round(total_cost_mean), 
    " ($", round(total_cost_low), "-$", round(total_cost_high), ")\n")
cat("    - Imaging component: $", round(imaging_cost_mean), 
    " (", round(imaging_cost_mean/total_cost_mean*100), "%)\n")
cat("    - Capacity component: $", round(capacity_cost_mean), 
    " (", round(capacity_cost_mean/total_cost_mean*100), "%)\n")
cat("  Annual cost (medium ED, 40K visits): $", 
    format(round(ed_sizes$Cost_Mean[2]), big.mark=","), "\n")
cat("  Annual cost (large ED, 60K visits): $", 
    format(round(ed_sizes$Cost_Mean[3]), big.mark=","), "\n")
cat("  Bed-hours freed (study site): ", round(annual_bedhours_freed), "/year\n")
cat("  Additional visits enabled: ", round(additional_visits), "/year\n")

#=========================================================================
# STEP 11: Literature citations for manuscript
#=========================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("                    CITATIONS FOR MANUSCRIPT                       \n")
cat("═══════════════════════════════════════════════════════════════════\n")

cat("\nED Bed-Hour Costs:\n")
cat("  1. Schreyer KE, Martin R. The Economics of an Admissions Holding Unit.\n")
cat("     Western Journal of Emergency Medicine. 2017;18(4):553-558.\n")
cat("     doi:10.5811/westjem.2017.4.32740\n")
cat("     --> Personnel costs: $58.20/bed-hour (2010-2011 data)\n")
cat("\n")
cat("  2. Canellas MM, Jewell M, Edwards JL, et al. Measurement of Cost of\n")
cat("     Boarding in the Emergency Department Using Time-Driven Activity-Based\n")
cat("     Costing. Annals of Emergency Medicine. 2024;84(4):410-419.\n")
cat("     doi:10.1016/j.annemergmed.2024.05.013\n")
cat("     --> Daily boarding cost: $1,856 (~$77/hour)\n")

cat("\nX-Ray Costs:\n")
cat("  3. Dabus G, Hirsch JA, Booker MT, Silva E. Practice Expense and Its\n")
cat("     Impact on Radiology Reimbursement. Journal of the American College\n")
cat("     of Radiology. 2025. doi:10.1016/j.jacr.2025.02.047\n")
cat("     --> CPT 71046 in ED: $98.53 (technical) + $30-40 (professional)\n")
cat("     --> Total Medicare-allowed: $120-140 per study\n")

sink()

cat("\n*** Cost analysis complete. Output saved to outputs/tables/costs.txt ***\n")