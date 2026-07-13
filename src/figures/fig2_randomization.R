library(dplyr)
library(ggplot2)
library(tibble)
library(lfe)
library(grid)

# ------------------------------------------------------------
# Prepare data
# ------------------------------------------------------------

plot <- data %>%
  mutate(
    age = as.numeric(scale(age))
  )

# ------------------------------------------------------------
# Run models
# ------------------------------------------------------------

mod1 <- felm(
  batched ~ CHIEF_COMPLAINT + as.factor(ESI) + tachycardic + tachypneic +
    febrile + hypotensive + age + GENDER + race |
    dayofweekt + month_of_year | 0 | ED_PROVIDER,
  data = plot
)

mod2 <- felm(
  batch.tendency ~ CHIEF_COMPLAINT + as.factor(ESI) + tachycardic + tachypneic +
    febrile + hypotensive + age + GENDER + race |
    dayofweekt + month_of_year | 0 | ED_PROVIDER,
  data = plot
)

# ------------------------------------------------------------
# Extract coefficients
# ------------------------------------------------------------

extract_felm <- function(model, label) {
  ci <- confint(model)
  
  tibble(
    var = names(coef(model)),
    coef = as.numeric(coef(model)),
    conf.low = as.numeric(ci[, 1]),
    conf.high = as.numeric(ci[, 2]),
    model = label
  )
}

panels <- bind_rows(
  extract_felm(mod1, "Batched Tests"),
  extract_felm(mod2, "Physician Batch Tendency")
)

# ------------------------------------------------------------
# Clean labels
# ------------------------------------------------------------

label_map <- c(
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
  "as.factor(ESI)2" = "ESI Level 2",
  "as.factor(ESI)3" = "ESI Level 3",
  "as.factor(ESI)4" = "ESI Level 4",
  "as.factor(ESI)5" = "ESI Level 5",
  "tachycardic" = "Tachycardic",
  "tachypneic" = "Tachypneic",
  "febrile" = "Febrile",
  "hypotensive" = "Hypotensive",
  "age" = "Arrival Age",
  "GENDERMale" = "Sex: Male",
  "raceblack" = "Race: Black",
  "racenative" = "Race: Native",
  "raceother" = "Race: Other",
  "raceunknown" = "Race: Unknown",
  "racewhite" = "Race: White"
)

panels <- panels %>%
  mutate(
    label = dplyr::recode(var, !!!label_map, .default = var),
    model = factor(
      model,
      levels = c("Batched Tests", "Physician Batch Tendency")
    )
  ) %>%
  group_by(model) %>%
  arrange(coef, .by_group = TRUE) %>%
  mutate(label_ordered = factor(label, levels = unique(label))) %>%
  ungroup()

# ------------------------------------------------------------
# Management Science style plot
# ------------------------------------------------------------

p <- ggplot(panels, aes(x = coef, y = label_ordered)) +
  
  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.35
  ) +
  
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    linewidth = 0.55,
    color = "#d8031c"
  ) +
  
  geom_point(
    shape = 21,
    fill = "#d8031c",
    color = "white",
    size = 2.5,
    stroke = 0.25
  ) +
  
  facet_wrap(
    ~ model,
    scales = "free_y",
    nrow = 1
  ) +
  
  labs(
    x = "Coefficient estimate (95% CI)",
    y = NULL
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.25),
    
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.35),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    
    axis.title.x = element_text(size = 12, color = "black", margin = margin(t = 7)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10.5, color = "black"),
    axis.text.y = element_text(size = 10.2, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.30),
    axis.ticks.length = grid::unit(2.2, "pt"),
    
    legend.position = "none",
    panel.spacing.x = grid::unit(1.1, "lines"),
    plot.margin = margin(8, 12, 8, 8)
  ) +
  
  coord_cartesian(clip = "off")

p

ggsave(
  "outputs/figures/fig2_panel_batched_management_science.png",
  p,
  width = 10.5,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)
