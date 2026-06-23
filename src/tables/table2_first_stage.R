################################################################################
# First stage: Table 2
################################################################################
library(fixest)
sink('outputs/tables/Table2.txt') 

print('First-Stage Results: Batch Tendency and Batching')

# First-stage model 1: Baseline model ------------------------------------------
fs_model_1 <- feols(batched ~ batch.tendency | 
                      dayofweekt + month_of_year, 
                    vcov = 'HC1', data = final)

wald(fs_model_1, keep = "batch.tendency")

# First-stage model 2: Controls  -----------------------------------------------
fs_model_2 <- feols(batched ~ batch.tendency  +
                      tachycardic + tachypneic + febrile + hypotensive + 
                      EXPERIENCE + PROVIDER_SEX + hrs_in_shift +
                      age + capacity_level + lab.tendency + admit.tendency | 
                      dayofweekt + month_of_year + complaint_esi + race + GENDER, 
                    vcov = 'HC1', data = final)


wald(fs_model_2, keep = "batch.tendency")
# ------------------------------------------------------------------------------
# Table results

etable(fs_model_1, fs_model_2, keep = c("batch.tendency"))

print(paste('Sample mean:', mean(data$batched)))
print(paste('Sample sd:', sd(data$batched)))

quantile(data$batch.tendency, probs = seq(0, 1, 0.1))[c(2,10)]

sink()

# scatterplpot batch.tendency by ED_PROVIDER ordered by batch tendency

ggplot(final, aes(x = reorder(ED_PROVIDER, batch.tendency), y = batch.tendency)) +
  geom_point(color = "#1a365d", size = 2.5, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Physician Batch Tendency",
    title = "Distribution of Physician Batch Tendency"
  )

