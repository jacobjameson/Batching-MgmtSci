

max_density <- max(density(final$batch.tendency)$y)

ggplot(final, aes(x = batch.tendency)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "grey", color = "black", 
                 bins = 10, alpha = 0.7) +
  geom_smooth(aes(y = batched * max(density(batch.tendency)$y) * (100/30)), 
              method = "loess", span = 0.82,
              linetype = "solid", size = 1, color = "#d8031c", fill ='#d8031c') +
  labs(
    x = "Batch Tendency"
  ) +
  scale_y_continuous(
    name = "Percentage of Sample",
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(~./max(density(data$batch.tendency)$y) * (30/100), 
                        name = "Probability of Batching",
                        breaks = seq(0, .5, .1),
                        labels = scales::percent_format(scale = 100))
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right = element_text(color = "black"),
    legend.position = "none"
  ) 


ggsave('outputs/figures/Fig3_firststage.png', width = 8, height = 6)

