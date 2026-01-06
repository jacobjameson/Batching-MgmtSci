

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

ggsave('outputs/figures/Fig3_firststage.png', width = 8, height = 6)

