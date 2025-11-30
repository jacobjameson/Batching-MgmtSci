library(fixest)
library(ggplot2)
library(dplyr)

final$CHIEF_COMPLAINT <- as.character(final$CHIEF_COMPLAINT)

run_heterogeneity_by_complaint <- function(data, outcome_var) {
  
  complaints <- trimws(unique(na.omit(data$CHIEF_COMPLAINT)))
  complaints <- complaints[complaints != ""]
  
  results <- data.frame(
    chief_complaint = character(),
    outcome = character(),
    coefficient = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    p_value = numeric(),
    n_obs = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(complaints)) {
    complaint <- complaints[i]
    message("Running ", i, "/", length(complaints), ": ", complaint)
    
    complaint_data <- data[data$CHIEF_COMPLAINT == complaint, ]
    
    model <- tryCatch({
      feols(
        as.formula(paste0(
          outcome_var,
          " ~ tachycardic + tachypneic + febrile + hypotensive + hrs_in_shift + ",
          "EXPERIENCE + age + LAB_PERF | ",
          "dayofweekt + month_of_year + race + ESI + GENDER + PROVIDER_SEX + capacity_level | ",
          "batched ~ batch.tendency"
        )),
        vcov = 'HC1',
        data = complaint_data
      )
    }, error = function(e) {
      message("   Error: ", e$message)
      NULL
    })
    
    if (is.null(model)) next
    
    if (!"fit_batched" %in% names(coef(model))) {
      message("   Skipped: 'fit_batched' missing in model coefficients.")
      next
    }
    
    coef_val <- coef(model)["fit_batched"]
    se_val   <- se(model)["fit_batched"]
    ci_lower <- coef_val - 1.96 * se_val
    ci_upper <- coef_val + 1.96 * se_val
    t_stat   <- coef_val / se_val
    p_val    <- 2 * (1 - pnorm(abs(t_stat)))
    
    results[nrow(results) + 1, ] <- list(
      chief_complaint = complaint,
      outcome = outcome_var,
      coefficient = coef_val,
      se = se_val,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_val,
      n_obs = nrow(complaint_data)
    )
  }
  
  results
}
# Run for each outcome
los_results <- run_heterogeneity_by_complaint(final, "ln_ED_LOS")
tests_results <- run_heterogeneity_by_complaint(final, "imgTests")
return_results <- run_heterogeneity_by_complaint(final, "RTN_72_HR_ADMIT")

# Combine all results
all_results <- bind_rows(
  los_results %>% mutate(outcome_label = "Log ED LOS"),
  tests_results %>% mutate(outcome_label = "Imaging Tests Performed"),
  return_results %>% mutate(outcome_label = "72hr Return with Admission")
)

# Print summary
print(all_results)

# Create coefficient plots
create_coef_plot <- function(results_df, outcome_name, title) {
  
  # Filter to specific outcome
  plot_data <- results_df %>%
    filter(outcome_label == outcome_name) %>%
    arrange(coefficient)
  
  # Create plot
  p <- ggplot(plot_data, aes(x = reorder(chief_complaint, coefficient), y = coefficient)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = title,
      x = "Chief Complaint",
      y = "2SLS Coefficient (95% CI)",
      caption = "Error bars show 95% confidence intervals. Standard errors clustered at physician level."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.title = element_text(size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# First, calculate batch rates by chief complaint
batch_rates <- final %>%
  group_by(CHIEF_COMPLAINT) %>%
  summarise(
    batch_rate = mean(batched, na.rm = TRUE) * 100,
    n_total = n()
  ) %>%
  ungroup()

# Merge batch rates with results
all_results <- all_results %>%
  left_join(batch_rates, by = c("chief_complaint" = "CHIEF_COMPLAINT"))

# Create enhanced labels with batch rate
all_results <- all_results %>%
  mutate(
    complaint_label = paste0(chief_complaint, " (", round(batch_rate, 1), "%)"),
    batch_rate_category = cut(batch_rate, 
                              breaks = c(0, 20, 40, 60, 100),
                              labels = c("<20%", "20-40%", "40-60%", ">60%"))
  )

plot_combined <- ggplot(all_results, 
                        aes(x = reorder(complaint_label, batch_rate),  # Changed this line
                            y = coefficient)) +
  geom_point(aes(color = batch_rate), size = 3.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = batch_rate), 
                width = 0.3, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_color_gradient(
    low = "#2972b6",    # Your red for low batch rates
    high = "#d8031c",   # Blue for high batches
    name = "Batching\nRate",
    labels = function(x) paste0(round(x, 0), "%")
  ) +
  coord_flip() +
  facet_wrap(~outcome_label, scales = "free_x", ncol = 3) +
  labs(
    x = "Chief Complaint (Batching Rate)\n",
    y = "2SLS Coefficient (95% CI)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0),
    strip.text = element_text(size = 14, face = "bold"))

print(plot_combined)

ggsave("outputs/figures/heterogeneity_by_complaint.png", 
       plot_combined, width = 14, height = 4)

