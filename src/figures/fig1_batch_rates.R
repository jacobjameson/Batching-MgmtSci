
plot_data <- data %>%
  group_by(CHIEF_COMPLAINT, ED_PROVIDER) %>%
  summarise(avg_batched = mean(batched, na.rm = TRUE), .groups = "drop") %>%
  group_by(CHIEF_COMPLAINT) %>%
  mutate(variance = var(avg_batched, na.rm = TRUE)) %>%
  ungroup()

ggplot(plot_data, aes(x = reorder(CHIEF_COMPLAINT, -variance), y = avg_batched)) +
  geom_jitter(aes(color = variance), width = 0.2, size = 2.5, alpha = 0.9) +
  geom_boxplot(aes(fill = variance), color = "black", alpha = 0.7, outlier.shape = NA, size = 0.6) +
  scale_color_gradient(
    low = "#2972b6", high = "#d8031c",
    name = "Spread of Batched Rates",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 0.8,
      label = FALSE  # Suppress numeric labels
    )
  ) +
  scale_fill_gradient(
    low = "#2972b6", high = "#d8031c",
    name = "Spread of Batched Rates",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 0.8,
      ticks = FALSE,  # Remove ticks
      label = FALSE
    )
  ) +
  guides(
    fill = guide_colorbar(
      title = "Spread of Batched Rates",
      label = TRUE,
      label.hjust = -0.5,
      barwidth = 15,
      barheight = 0.8,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    y = "Physician Batch Rate",
  ) +
  labs(
    x = "Chief Complaint",
    y = "\nPhysician Batch Rate",
  ) +
  # Add annotations
  annotate(
    "text", x = 13.5, y = 0.32,
    label = "Lower intra-complaint \nphysician variance", color = "#2972b6", 
    angle = 0, hjust = 0, size = 5, fontface = "bold"
  ) +
  annotate(
    "text", x = 4.5, y = 0.32,
    label = "Higher intra-complaint \nphysician variance", color = "#d8031c",
    angle = 0, hjust = 0, size = 5, fontface = "bold"
  ) +
  annotate(
    "segment", x = 6, xend = 3,
    y = 0.52, yend = 0.52,
    size = 1.5,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = "#d8031c"
  ) +
  annotate(
    "segment", x = 12, xend = 15,
    y = 0.52, yend = 0.52,
    size = 1.5,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = "#2972b6"
  )

ggsave("outputs/figures/fig1_boxplot.png", width = 10, height = 6, dpi = 300, bg= 'white')

