################################################################################
#-------------------------------------------------------------------------------
# Reduced Form
#-------------------------------------------------------------------------------
################################################################################

library(fixest)
library(marginaleffects)
library(sandwich)

sink('outputs/tables/Table3.txt')

# ------------------------------------------------------------------------------

rf_model_disp <- feols(
  ln_disp_time ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level + # ED variables
    lab.tendency + admit.tendency + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = final, vcov = "HC1")

rf_model_los <- feols(
  ln_ED_LOS ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level +  # ED variables
    lab.tendency + admit.tendency + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
   data = final, vcov = "HC1")

rf_model_img <- feols(
  imgTests ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level  + # ED variables
    lab.tendency + admit.tendency  + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
   data = final, vcov = "HC1")

rf_model_ra <- feols(
  RTN_72_HR_ADMIT ~ batch.tendency + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level  + # ED variables
    lab.tendency + admit.tendency  + EXPERIENCE + hrs_in_shift  + PROVIDER_SEX  | # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = final, vcov = "HC1")

# ------------------------------------------------------------------------------

etable(rf_model_disp, rf_model_los,rf_model_img, rf_model_ra, 
       keep = c("batch.tendency"))


quantile(final$batch.tendency, probs = seq(0, 1, 0.1))[c(2,10)]

print(paste('ln_disp_time mean:', mean(final$ln_disp_time)))
print(paste('ln_disp_time sd:', sd(final$ln_disp_time)))

print(paste('ln_ED_LOS mean:', mean(final$ln_ED_LOS)))
print(paste('ln_ED_LOS sd:', sd(final$ln_ED_LOS)))

print(paste('imgTests mean:', mean(final$imgTests)))
print(paste('imgTests sd:', sd(final$imgTests)))

print(paste('RTN_72_HR_ADMIT mean:', mean(final$RTN_72_HR_ADMIT)))
print(paste('RTN_72_HR_ADMIT sd:', sd(final$RTN_72_HR_ADMIT)))


################################################################################
#-------------------------------------------------------------------------------
# Reduced Form + Nonlinear Reduced Form AMEs
#-------------------------------------------------------------------------------
################################################################################


ctrl <- "batch.tendency + tachycardic + tachypneic + febrile + hypotensive +
  age + capacity_level + lab.tendency + admit.tendency +
  EXPERIENCE + hrs_in_shift + PROVIDER_SEX + dayofweekt + month_of_year + complaint_esi + race + GENDER"
FE <- "0"
f  <- function(y) as.formula(paste(y, "~", ctrl, "|", FE))

# Linear RF (continuous / logged outcomes)
disp <- feols (f("ln_disp_time"),   final, vcov = "HC1")
los  <- feols (f("ln_ED_LOS"),      final, vcov = "HC1")

# Nonlinear RF (count + binary)
img  <- fepois(f("imgTests"),        final, vcov = "HC1")
ra   <- feglm (f("RTN_72_HR_ADMIT"), final, family = binomial("probit"), vcov = "HC1")

etable(disp, los, img, ra, keep = "batch.tendency")


bt <- quantile(final$batch.tendency, c(.1, .9))
avg_comparisons(img, variables = list(batch.tendency = bt))
avg_comparisons(ra,  variables = list(batch.tendency = bt))


library(dplyr)
library(tibble)
library(ggplot2)
library(ggh4x)
library(marginaleffects)

bt       <- quantile(final$batch.tendency, c(.1, .9))
mean_img <- mean(final$imgTests, na.rm = TRUE)

# p10 -> p90 contrast on the model's native response scale
contrast <- function(model) {
  ac <- avg_comparisons(model, variables = list(batch.tendency = bt))
  c(est = ac$estimate[1], lo = ac$conf.low[1], hi = ac$conf.high[1])
}

c_disp  <- contrast(rf_model_disp)   # log points
c_los   <- contrast(rf_model_los)    # log points
c_img_l <- contrast(rf_model_img)    # count, linear
c_img_p <- contrast(img)             # count, Poisson
c_ra_l  <- contrast(rf_model_ra)     # probability, linear
c_ra_p  <- contrast(ra)              # probability, probit

# unit transforms (applied to est, lo, hi together)
pct     <- function(v) v * 100              # log points  -> % change
pctmean <- function(v) v / mean_img * 100   # count       -> % of mean
pp      <- function(v) v * 100              # probability -> percentage points

row <- function(name, grp, mdl, v) tibble(
  outcome = name, group = grp, model = mdl,
  p = v["est"], lo = v["lo"], hi = v["hi"])

dat <- bind_rows(
  row("Time to disposition", "Throughput", "Linear Model",    pct(c_disp)),
  row("ED length of stay",   "Throughput", "Linear Model",    pct(c_los)),
  row("Number of Imaging tests",       "Throughput", "Linear Model",    pctmean(c_img_l)),
  row("Number of Imaging tests",       "Throughput", "Nonlinear Model", pctmean(c_img_p)),
  row("72-hr readmission",   "Safety",     "Linear Model",    pp(c_ra_l)),
  row("72-hr readmission",   "Safety",     "Nonlinear Model", pp(c_ra_p))
) %>%
  mutate(
    outcome = factor(outcome, levels = rev(c(
      "Time to disposition","ED length of stay","Number of Imaging tests","72-hr readmission"))),
    group   = factor(group, levels = c("Throughput","Safety")),
    model   = factor(model, levels = c("Linear Model","Nonlinear Model")))

# data-driven x-ranges (always include 0, pad 8%)
pad <- function(d, frac = 0.08) {
  r <- range(c(0, d$lo, d$hi), na.rm = TRUE); w <- diff(r)
  c(r[1] - w * frac, r[2] + w * frac)
}
lim_tp <- pad(filter(dat, group == "Throughput"))
lim_sf <- pad(filter(dat, group == "Safety"))

lab_pct <- \(x) paste0(formatC(x, format = "f", digits = 1), "%")
lab_pp  <- \(x) paste0(formatC(x, format = "f", digits = 2), " pp")

ggplot(dat, aes(p, outcome, colour = model)) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "#1a365d") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.16,
                 position = position_dodge(0.5), linewidth = 0.55, na.rm = TRUE) +
  geom_point(aes(fill = model), position = position_dodge(0.5),
             size = 3.5, color = "white",
             shape = 21,
             stroke = 0.5) +
  facet_grid2(rows = vars(group), scales = "free", independent = "x",
              space = "free_y", switch = "y") +
  facetted_pos_scales(x = list(
    scale_x_continuous(limits = lim_tp, labels = lab_pct),
    scale_x_continuous(limits = lim_sf, labels = lab_pp)
  )) +
  scale_colour_manual(values = c(`Linear Model` = "#2a5db0", `Nonlinear Model` = "#158a63")) +
  scale_fill_manual(values  = c(`Linear Model` = "#2a5db0", `Nonlinear Model` = "#158a63")) +
  labs(
    x = "\nEffect of moving from a low- to high-batching physician\n(10th vs. 90th percentile in batching tendency)",
    y = NULL, colour = NULL, fill = NULL) +
  theme_bw(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.25),
    
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.35),
    strip.text = element_blank(),
    
    axis.title.x = element_text(size = 12, color = "black", margin = margin(t = 7)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10.5, color = "black"),
    axis.text.y = element_text(size = 10.2, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.30),
    axis.ticks.length = grid::unit(2.2, "pt"),
    
    legend.position = "right",
    panel.spacing.x = grid::unit(1.1, "lines"),
    plot.margin = margin(8, 12, 8, 8)
  )

ggsave("outputs/figures/reduced_form.png", width = 9.5, height = 4.4)


#######
# WITHIN PHYSICIAN


rf_model_disp <- feols(
  ln_disp_time ~ batch.tendency.complaint + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level +  # ED variables
    hrs_in_shift  | 
    ED_PROVIDER + # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = final, vcov = "HC1")

rf_model_los <- feols(
  ln_ED_LOS ~ batch.tendency.complaint + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level +  # ED variables
    hrs_in_shift  | 
    ED_PROVIDER + # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = final, vcov = "HC1")

rf_model_img <- feols(
  imgTests ~ batch.tendency.complaint + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level +  # ED variables
    hrs_in_shift  | 
    ED_PROVIDER + # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = final, vcov = "HC1")

rf_model_ra <- feols(
  RTN_72_HR_ADMIT ~ batch.tendency.complaint + # instrument
    tachycardic + tachypneic + febrile + hypotensive + # patient variables
    age + # patient variables
    capacity_level +  # ED variables
    hrs_in_shift  | 
    ED_PROVIDER + # physician variables
    dayofweekt + month_of_year + # time FE
    complaint_esi + race + GENDER, # patient variables
  data = final, vcov = "HC1")

# ------------------------------------------------------------------------------

etable(rf_model_disp, rf_model_los,rf_model_img, rf_model_ra, 
       keep = c("batch.tendency"))
