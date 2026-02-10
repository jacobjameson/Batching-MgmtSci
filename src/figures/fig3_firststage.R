

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

