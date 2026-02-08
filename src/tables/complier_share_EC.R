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

