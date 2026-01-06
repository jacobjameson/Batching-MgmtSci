# Standardize continuous predictors
plot <- data %>%
  mutate(across(c(age, EXPERIENCE), scale))

# Run models
mod1 <- felm(batched ~ CHIEF_COMPLAINT + as.factor(ESI) + tachycardic + tachypneic +
               febrile + hypotensive + age + GENDER + race + EXPERIENCE + PROVIDER_SEX |
               dayofweekt + month_of_year | 0 | ED_PROVIDER, data = plot)
mod2 <- felm(batch.tendency ~ CHIEF_COMPLAINT + as.factor(ESI) + tachycardic + tachypneic +
               febrile + hypotensive + age + GENDER + race + EXPERIENCE + PROVIDER_SEX |
               dayofweekt + month_of_year | 0 | ED_PROVIDER, data = plot)

# Pull coefficients
extract_felm <- function(model, label){
  data.frame(
    var = names(coef(model)),
    coef = coef(model),
    conf.low = confint(model)[,1],
    conf.high = confint(model)[,2],
    model = label
  )
}

p1 <- extract_felm(mod1, "Batched Tests")
p2 <- extract_felm(mod2, "Physician Batch Tendency")
panels <- bind_rows(p1, p2)

# create a variable called category that is either patient characteristic, physician characteristic, or other
panels$category <- dplyr::recode(panels$var,
                                 "CHIEF_COMPLAINTAbnormal Test Results" = "Patient characteristic",
                                 "CHIEF_COMPLAINTBack or Flank Pain" = "Patient characteristic",
                                 "CHIEF_COMPLAINTCardiac Arrhythmias" = "Patient characteristic",
                                 "CHIEF_COMPLAINTChest Pain" = "Patient characteristic",
                                 "CHIEF_COMPLAINTDizziness/Lightheadedness/Syncope" = "Patient characteristic",
                                 "CHIEF_COMPLAINTExtremity Complaints" = "Patient characteristic",
                                 "CHIEF_COMPLAINTFalls, MVA, Assaults, and Trauma" = "Patient characteristic",
                                 "CHIEF_COMPLAINTFatigue and Weakness" = "Patient characteristic",
                                 "CHIEF_COMPLAINTFevers, Sweats or Chills" = "Patient characteristic",
                                 "CHIEF_COMPLAINTGastrointestinal Issues" = "Patient characteristic",
                                 "CHIEF_COMPLAINTNeurological Issue" = "Patient characteristic",
                                 "CHIEF_COMPLAINTShortness of Breath" = "Patient characteristic",
                                 "CHIEF_COMPLAINTSkin Complaints" = "Patient characteristic",
                                 "CHIEF_COMPLAINTUpper Respiratory Symptoms" = "Patient characteristic",
                                 "CHIEF_COMPLAINTUrinary Complaints" = "Patient characteristic",
                                 
                                 # ESI Levels
                                 "as.factor(ESI)2" = "Patient characteristic",
                                 "as.factor(ESI)3" = "Patient characteristic",
                                 "as.factor(ESI)4" = "Patient characteristic",
                                 "as.factor(ESI)5" = "Patient characteristic",
                                 
                                 # Vitals
                                 "tachycardic" = "Patient characteristic",
                                 "tachypneic" = "Patient characteristic",
                                 "febrile" = "Patient characteristic",
                                 "hypotensive" = "Patient characteristic",
                                 
                                 # Age Variable
                                 "age" = "Patient characteristic",
                                 
                                 # Capacity Level
                                 "capacity_levelMinor Overcapacity" = "ED characteristic",
                                 "capacity_levelNormal Operations" = "ED characteristic",
                                 
                                 # Gender
                                 "GENDERMale" = "Patient characteristic",
                                 
                                 # Race Variables
                                 "raceblack" = "Patient characteristic",
                                 "racenative" = "Patient characteristic",
                                 "raceother" = "Patient characteristic",
                                 "raceunknown" = "Patient characteristic",
                                 "racewhite" = "Patient characteristic",
                                 
                                 'PROVIDER_SEXM' = 'Physician characteristic',
                                 'EXPERIENCE' = 'Physician characteristic',
)

panels$var <- dplyr::recode(panels$var,
                            # Chief Complaint Variables
                            "CHIEF_COMPLAINTAbnormal Test Results" = "Abnormal Test Results",
                            "CHIEF_COMPLAINTBack or Flank Pain" = "Back or Flank Pain",
                            "CHIEF_COMPLAINTCardiac Arrhythmias" = "Cardiac Arrhythmias",
                            "CHIEF_COMPLAINTChest Pain" = "Chest Pain",
                            "CHIEF_COMPLAINTDizziness/Lightheadedness/Syncope" = "Dizziness/Lightheadedness",
                            "CHIEF_COMPLAINTExtremity Complaints" = "Extremity Complaints",
                            "CHIEF_COMPLAINTFalls, MVA, Assaults, and Trauma" = "Falls/Crashes/Assaults/Trauma",
                            "CHIEF_COMPLAINTFatigue and Weakness" = "Fatigue and Weakness",
                            "CHIEF_COMPLAINTFevers, Sweats or Chills" = "Fevers/Sweats/Chills",
                            "CHIEF_COMPLAINTGastrointestinal Issues" = "Gastrointestinal Issues",
                            "CHIEF_COMPLAINTNeurological Issue" = "Neurological Issue",
                            "CHIEF_COMPLAINTShortness of Breath" = "Shortness of Breath",
                            "CHIEF_COMPLAINTSkin Complaints" = "Skin Complaints",
                            "CHIEF_COMPLAINTUpper Respiratory Symptoms" = "Upper Respiratory Symptoms",
                            "CHIEF_COMPLAINTUrinary Complaints" = "Urinary Complaints",
                            
                            # ESI Levels
                            "as.factor(ESI)2" = "ESI Level 2",
                            "as.factor(ESI)3" = "ESI Level 3",
                            "as.factor(ESI)4" = "ESI Level 4",
                            "as.factor(ESI)5" = "ESI Level 5",
                            
                            # Vitals
                            "tachycardic" = "Tachycardic",
                            "tachypneic" = "Tachypneic",
                            "febrile" = "Febrile",
                            "hypotensive" = "Hypotensive",
                            
                            # Age Variable
                            "age" = "Arrival Age",
                            
                            # Capacity Level
                            "capacity_levelMinor Overcapacity" = "ED Capacity: Minor Overcapacity",
                            "capacity_levelNormal Operations" = "ED Capacity: Normal Operations",
                            
                            # Gender
                            "GENDERMale" = "Sex: Male",
                            
                            # Race Variables
                            "raceblack" = "Race: Black",
                            "racenative" = "Race: Native",
                            "raceother" = "Race: Other",
                            "raceunknown" = "Race: Unknown",
                            "racewhite" = "Race: White",
                            
                            'PROVIDER_SEXM' = 'Physician Male',
                            'EXPERIENCE' = 'Physician Experience',
)

ggplot(panels, aes(x = reorder(var, coef),
                   y = coef, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_pointrange(color = '#d8031c', size = 0.6, fatten = 2) +
  facet_wrap(~model, scales = "free_y") +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12, color = "black"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(y = "Standardized Coefficient Estimate (95% CI)", x = NULL) 

ggsave("outputs/figures/fig2_panel_batched_standardized.png", width = 12, 
       height = 7, units = "in", bg = "white")

