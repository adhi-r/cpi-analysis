#!/usr/bin/env Rscript

library(targets)
library(tidyverse)
library(patchwork)
library(cli)

# Load volume data
volume_ts <- tar_read(volume_ts)

cli::cli_h1("Volume Trends by CPI Type (Separate)")

# Create separate plots for each CPI type
plot_by_type <- function(df, type_name, type_label, color) {
  df |>
    filter(cpi_type == type_name) |>
    ggplot(aes(x = cpi_month, y = total_volume)) +
    geom_line(linewidth = 1.2, color = color) +
    geom_point(size = 2.5, color = color, alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, color = "gray40",
                linetype = "dashed", linewidth = 0.8, alpha = 0.15) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
    labs(
      title = type_label,
      x = NULL,
      y = "Contracts"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12)
    )
}

# Create individual plots
p1 <- plot_by_type(volume_ts, "headline_mom", "Headline MoM", "#1b9e77")
p2 <- plot_by_type(volume_ts, "headline_yoy", "Headline YoY", "#d95f02")
p3 <- plot_by_type(volume_ts, "core_mom", "Core MoM", "#7570b3")
p4 <- plot_by_type(volume_ts, "core_yoy", "Core YoY", "#e7298a")

# Combine into 2x2 grid
combined <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Kalshi CPI Market Volume Over Time (by Type)",
    subtitle = "Each panel shows one CPI market type independently"
  )

print(combined)

# Save
ggsave("output/volume_by_type_grid.png", combined, width = 14, height = 10, dpi = 300)
cli::cli_alert_success("Saved to output/volume_by_type_grid.png")

# Print summary stats by type
cli::cli_h2("Summary Statistics by Type")
volume_ts |>
  mutate(period = case_when(
    cpi_month < "2023-01-01" ~ "2021-2022",
    cpi_month >= "2023-01-01" & cpi_month < "2025-01-01" ~ "2023-2024",
    TRUE ~ "2025+"
  )) |>
  group_by(cpi_type, period) |>
  summarise(
    n_months = n(),
    avg_vol = mean(total_volume),
    median_vol = median(total_volume),
    .groups = "drop"
  ) |>
  arrange(cpi_type, period) |>
  print()
