################################################################################
# First stage: Table 2
################################################################################
library(fixest)
sink('outputs/tables/first_stage.txt') 

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


###################################
##### Alternate first-stage models for within physician #####
###################################

# First-stage model 1: Baseline model ------------------------------------------
fs_model_1 <- feols(batched ~ batch.tendency.complaint | 
                      dayofweekt + month_of_year + ED_PROVIDER, 
                    vcov = 'HC1', data = final)

wald(fs_model_1, keep = "batch.tendency")

# First-stage model 2: Controls  -----------------------------------------------
fs_model_2 <- feols(batched ~ batch.tendency.complaint  +
                      tachycardic + tachypneic + febrile + hypotensive + 
                      hrs_in_shift + age + capacity_level | 
                      dayofweekt + month_of_year + ED_PROVIDER + complaint_esi + race + GENDER, 
                    vcov = 'HC1', data = final)


wald(fs_model_2, keep = "batch.tendency")
# ------------------------------------------------------------------------------
# Table results

etable(fs_model_1, fs_model_2, keep = c("batch.tendency"))

print(paste('Sample mean:', mean(data$batched)))
print(paste('Sample sd:', sd(data$batched)))

sink()


