
plot_data <- data %>%
  group_by(CHIEF_COMPLAINT, ED_PROVIDER) %>%
  summarise(avg_batched = mean(batched, na.rm = TRUE), .groups = "drop") %>%
  group_by(CHIEF_COMPLAINT) %>%
  mutate(variance = var(avg_batched, na.rm = TRUE)) %>%
  ungroup()



ggplot(plot_data, aes(x = reorder(CHIEF_COMPLAINT, -variance), y = avg_batched)) +
  
  # Jittered points
  geom_jitter(
    aes(color = variance),
    width = 0.2,
    size = 2.5,
    alpha = 0.9
  ) +
  
  # Boxplots
  geom_boxplot(
    aes(fill = variance),
    color = "#1a365d",
    alpha = 0.7,
    outlier.shape = NA,
    linewidth = 0.6
  ) +
  
  # Color scales
  scale_color_gradient(
    low = "#2972b6", high = "#dc2626",
    guide = "none"
  ) +
  scale_fill_gradient(
    low = "#2972b6", high = "#dc2626",
    guide = "none"
  ) +
  
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  
  coord_flip(clip = "off") +
  
  labs(
    x = NULL,
    y = "Physician Batch Rate"
  ) +
  
  # Annotations
  annotate(
    "text", x = 13.5, y = 0.32,
    label = "Lower intra-complaint\nphysician variance",
    color = "#2972b6",
    hjust = 0, size = 4.2, fontface = "italic"
  ) +
  annotate(
    "text", x = 4.5, y = 0.32,
    label = "Higher intra-complaint\nphysician variance",
    color = "#dc2626",
    hjust = 0, size = 4.2, fontface = "italic"
  ) +
  annotate(
    "segment", x = 12, xend = 15,
    y = 0.47, yend = 0.47,
    linewidth = 0.9,
    arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed"),
    color = "#2972b6"
  ) +
  annotate(
    "segment", x = 6, xend = 3,
    y = 0.47, yend = 0.47,
    linewidth = 0.9,
    arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed"),
    color = "#dc2626"
  ) +
  
  # Theme
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
  ) 


ggsave("outputs/figures/fig1_boxplot.png", width = 10, height = 6, dpi = 300, bg= 'white')
