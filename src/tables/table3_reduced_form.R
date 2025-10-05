################################################################################
#-------------------------------------------------------------------------------
# Reduced Form
#-------------------------------------------------------------------------------
################################################################################

sink('outputs/tables/Table3.txt')

# ------------------------------------------------------------------------------

rf_model_disp <- feols(
  ln_disp_time ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level + LAB_PERF + # ED variables
    EXPERIENCE + PROVIDER_SEX | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  cluster = ~ED_PROVIDER, data = data)

rf_model_los <- feols(
  ln_ED_LOS ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level + LAB_PERF + # ED variables
    EXPERIENCE + PROVIDER_SEX | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  cluster = ~ED_PROVIDER, data = data)

rf_model_img <- feols(
  imgTests ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level + LAB_PERF + # ED variables
    EXPERIENCE + PROVIDER_SEX | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  cluster = ~ED_PROVIDER, data = data)

rf_model_ra <- feols(
  RTN_72_HR_ADMIT ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level + LAB_PERF + # ED variables
    EXPERIENCE + PROVIDER_SEX | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  cluster = ~ED_PROVIDER, data = data)

# ------------------------------------------------------------------------------

etable(rf_model_disp, rf_model_los,rf_model_img, rf_model_ra, 
       cluster = "ED_PROVIDER", se = "cluster", 
       keep = c("batch.tendency"))


quantile(data$batch.tendency, probs = seq(0, 1, 0.1))[c(2,10)]

# Calculate F-statistics for reduced-form models
wald_rf_1 <- wald(rf_model_disp, cluster = "ED_PROVIDER")
wald_rf_2 <- wald(rf_model_los, cluster = "ED_PROVIDER")
wald_rf_3 <- wald(rf_model_img, cluster = "ED_PROVIDER")
wald_rf_4 <- wald(rf_model_ra, cluster = "ED_PROVIDER")


print(paste('ln_disp_time mean:', mean(data$ln_disp_time)))
print(paste('ln_disp_time sd:', sd(data$ln_disp_time)))

print(paste('ln_ED_LOS mean:', mean(data$ln_ED_LOS)))
print(paste('ln_ED_LOS sd:', sd(data$ln_ED_LOS)))

print(paste('imgTests mean:', mean(data$imgTests)))
print(paste('imgTests sd:', sd(data$imgTests)))

print(paste('RTN_72_HR_ADMIT mean:', mean(data$RTN_72_HR_ADMIT)))
print(paste('RTN_72_HR_ADMIT sd:', sd(data$RTN_72_HR_ADMIT)))

sink()