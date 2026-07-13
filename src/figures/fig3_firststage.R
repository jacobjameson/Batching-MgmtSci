

max_density <- max(density(final$batch.tendency)$y)


# Calculate scaling factor once
max_density <- max(density(final$batch.tendency)$y)
scale_factor <- max_density * (100 / 30)

ggplot(final, aes(x = batch.tendency)) +
  geom_histogram(
    aes(y = after_stat(density)),
    fill = "grey80",
    color = "grey30",
    bins = 10,
    alpha = 0.7
  ) +
  geom_smooth(
    aes(y = batched * scale_factor),
    method = "loess",
    span = 0.82,
    linewidth = 1,
    color = "#d8031c",
    fill = "#d8031c"
  ) +
  scale_y_continuous(
    name = "Percentage of Sample",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Probability of Batching",
      breaks = seq(0, 0.5, 0.1),
      labels = scales::percent_format(scale = 100)
    )
  ) +
  labs(x = "Batch Tendency") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.title.y.right = element_text(color = "#d8031c"),
    axis.text.y.right = element_text(color = "#d8031c")
  )


max_density <- max(density(final$batch.tendency)$y)
scale_factor <- max_density * (100 / 30)

p <- ggplot(final, aes(x = batch.tendency)) +
  
  # Histogram
  geom_histogram(
    aes(y = after_stat(density)),
    fill = "grey80",
    color = "#1a365d",
    bins = 10,
    alpha = 0.7
  ) +
  
  # LOESS smooth
  geom_smooth(
    aes(y = batched * scale_factor),
    method = "loess",
    span = 0.82,
    linewidth = 0.9,
    color = "#dc2626",
    fill = "#dc2626",
    alpha = 0.15
  ) +
  
  # Axes
  scale_y_continuous(
    name = "Percentage of Sample",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Probability of Batching",
      breaks = seq(0, 0.5, 0.1),
      labels = scales::percent_format(scale = 100)
    )
  ) +
  
  labs(x = "Batch Tendency") +
  
  # Theme
  theme_bw(base_size = 13) +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y.right = element_text(color = "#dc2626", size = 13),
    axis.text.y.right = element_text(color = "#dc2626", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 10)
  )

print(p)


ggsave('outputs/figures/Fig3_firststage.png', width = 8, height = 6)


library(ggplot2)
library(dplyr)
library(tibble)
library(scales)

# ------------------------------------------------------------
# Prepare data
# ------------------------------------------------------------

plot_df <- final %>%
  filter(!is.na(batch.tendency), !is.na(batched)) %>%
  mutate(
    batch.tendency = as.numeric(batch.tendency),
    batched = as.numeric(batched)
  )

# ------------------------------------------------------------
# Smooth first-stage curve
# Keep LOESS because the purpose is visual monotonicity.
# ------------------------------------------------------------

loess_fit <- loess(
  batched ~ batch.tendency,
  data = plot_df,
  span = 0.7,
  degree = 1,
  family = "gaussian",
  control = loess.control(surface = "direct")
)

pred_df <- tibble(
  batch.tendency = seq(
    min(plot_df$batch.tendency),
    max(plot_df$batch.tendency),
    length.out = 300
  )
)

loess_pred <- predict(
  loess_fit,
  newdata = pred_df,
  se = TRUE
)

pred_df <- pred_df %>%
  mutate(
    fit = pmin(pmax(loess_pred$fit, 0), 1),
    lo  = pmin(pmax(loess_pred$fit - 1.96 * loess_pred$se.fit, 0), 1),
    hi  = pmin(pmax(loess_pred$fit + 1.96 * loess_pred$se.fit, 0), 1)
  )

# ------------------------------------------------------------
# Scaling for secondary axis
# Histogram is percent of sample.
# First-stage probability is scaled onto same plotting space.
# ------------------------------------------------------------

hist_tmp <- ggplot_build(
  ggplot(plot_df, aes(x = batch.tendency)) +
    geom_histogram(
      aes(y = after_stat(count / sum(count) * 100)),
      bins = 12
    )
)$data[[1]]

hist_max <- max(hist_tmp$y, na.rm = TRUE)
prob_max <- max(pred_df$hi, na.rm = TRUE)

scale_factor <- hist_max / prob_max

# ------------------------------------------------------------
# Shared axis limits
# ------------------------------------------------------------

x_limits <- range(plot_df$batch.tendency, na.rm = TRUE)

# ------------------------------------------------------------
# 1. Manuscript style matching your paper
# ------------------------------------------------------------

p_my_style <- ggplot(plot_df, aes(x = batch.tendency)) +
  
  geom_histogram(
    aes(y = after_stat(count / sum(count) * 100)),
    bins = 12,
    fill = "grey85",
    color = "grey85",
    linewidth = 0.30,
    alpha = 0.90
  ) +
  
  geom_ribbon(
    data = pred_df,
    aes(
      x = batch.tendency,
      ymin = lo * scale_factor,
      ymax = hi * scale_factor
    ),
    inherit.aes = FALSE,
    fill = "#d8031c",
    alpha = 0.12
  ) +
  
  geom_line(
    data = pred_df,
    aes(x = batch.tendency, y = fit * scale_factor),
    inherit.aes = FALSE,
    color = "#d8031c",
    linewidth = 1.05
  ) +
  
  scale_x_continuous(
    limits = x_limits,
    breaks = pretty_breaks(n = 6)
  ) +
  
  scale_y_continuous(
    name = "Percentage of Sample",
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.06)),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Probability of Batching",
      labels = label_percent(accuracy = 1)
    )
  ) +
  
  labs(
    x = "Batch Tendency"
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 13, color = "black"),
    axis.title.y.right = element_text(color = "#d8031c", size = 13),
    axis.text.y.right = element_text(color = "#d8031c", size = 12),
    axis.ticks.y.right = element_line(color = "#d8031c"),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 10)
  )

p_my_style


# ------------------------------------------------------------
# 2. Hard-science / Nature-Cell-ish style
# Cleaner, more compact, less grid, sharper ink.
# ------------------------------------------------------------

p_science_style <- ggplot(plot_df, aes(x = batch.tendency)) +
  
  geom_histogram(
    aes(y = after_stat(count / sum(count) * 100)),
    bins = 20,
    fill = "grey92",
    color = "grey85",
    linewidth = 0.25,
    alpha = 1
  ) +
  
  geom_ribbon(
    data = pred_df,
    aes(
      x = batch.tendency,
      ymin = lo * scale_factor,
      ymax = hi * scale_factor
    ),
    inherit.aes = FALSE,
    fill = "#b2182b",
    alpha = 0.08
  ) +
  
  geom_line(
    data = pred_df,
    aes(x = batch.tendency, y = fit * scale_factor),
    inherit.aes = FALSE,
    color = "#b2182b",
    linewidth = 0.90
  ) +
  
  scale_x_continuous(
    limits = c(-0.04, 0.04),
    breaks = pretty_breaks(n = 6)
  ) +
  
  scale_y_continuous(
    name = "Percentage of sample",
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.045)),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Probability of batching",
      labels = label_percent(accuracy = 1)
    )
  ) +
  
  labs(
    x = "Batch tendency"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.35),
    axis.ticks = element_line(color = "black", linewidth = 0.30),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.title.y.right = element_text(color = "#b2182b", size = 12),
    axis.text.y.right = element_text(color = "#b2182b", size = 11),
    axis.ticks.y.right = element_line(color = "#b2182b", linewidth = 0.30),
    legend.position = "none",
    plot.margin = margin(8, 12, 8, 8)
  )

p_science_style



