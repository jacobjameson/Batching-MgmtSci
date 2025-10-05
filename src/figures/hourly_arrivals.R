library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork) # For combining plots
library(scales) # For nice formatting

# Calculate hourly volumes instead of density
hourly_overall <- data %>%
  group_by(hour_of_day) %>%
  summarise(
    patients = n(),
    patients_per_hour = n() / n_distinct(date(actual_date))
  )

hourly_weekday <- data %>%
  group_by(hour_of_day, is_weekend) %>%
  summarise(
    patients = n(),
    patients_per_hour = n() / n_distinct(date(actual_date))
  )

hourly_monthly <- data %>%
  group_by(hour_of_day, month) %>%
  summarise(
    patients = n(),
    patients_per_hour = n() / n_distinct(date(actual_date))
  )

# shared y-scale
y_limits <- c(0, 10)
y_breaks <- seq(0, 10, 2)
common_theme <- theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

p1 <- ggplot(hourly_overall, aes(x = hour_of_day, y = patients_per_hour)) +
  geom_line(color = "#2C3E50", size = 1) +
  geom_point(color = "#2C3E50", size = 2) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(
    limits = y_limits,
    breaks = y_breaks,
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(title = "Overall Hourly ED Arrivals", x = NULL, y = "Avg Patients/Hr") +
  common_theme

p2 <- ggplot(hourly_weekday, aes(x = hour_of_day, y = patients_per_hour, color = is_weekend)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(
    limits = y_limits,
    breaks = y_breaks,
    labels = scales::number_format(accuracy = 0.1)
  ) +
  scale_color_manual(values = c("grey40", "black")) +
  labs(title = "Weekday vs Weekend Arrivals", x = NULL, y = "Avg Patients/Hr", color = "Day Type") +
  common_theme

p3 <- ggplot(hourly_monthly, aes(x = hour_of_day, y = patients_per_hour, group = month)) +
  geom_line(size = 0.75, alpha = 0.7) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(
    limits = y_limits,
    breaks = y_breaks,
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(title = "Monthly Arrival Patterns", x = "Hour of Day", y = "Avg Patients/Hr", color = "Month") +
  common_theme

combined_plot <- (p1 / p2 / p3) +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

combined_plot

ggsave("outputs/figures/appendix_fig1a.png",
       plot = combined_plot, width = 9, height = 10)
