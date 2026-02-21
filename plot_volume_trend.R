#!/usr/bin/env Rscript

library(targets)
library(tidyverse)
library(cli)

# Load volume data
volume_ts <- tar_read(volume_ts)

# Aggregate: total volume across all CPI types per month
monthly_volume <- volume_ts |>
  group_by(cpi_month) |>
  summarise(
    total_contracts = sum(total_volume),
    n_market_types = n(),
    .groups = "drop"
  ) |>
  arrange(cpi_month)

cli::cli_h1("Monthly Volume Trend (All CPI Types Combined)")

# Print summary statistics
cli::cli_h2("Summary by Year")
monthly_volume |>
  mutate(year = year(cpi_month)) |>
  group_by(year) |>
  summarise(
    months = n(),
    total_vol = sum(total_contracts),
    avg_per_month = mean(total_contracts),
    median_per_month = median(total_contracts)
  ) |>
  print()

# Create the plot
p <- ggplot(monthly_volume, aes(x = cpi_month, y = total_contracts)) +
  geom_line(linewidth = 1.2, color = "#2c7bb6") +
  geom_point(size = 3, color = "#2c7bb6", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "#d7191c", linetype = "dashed", alpha = 0.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "Total Kalshi CPI Market Volume Over Time",
    subtitle = "Sum of all contracts traded across all CPI market types per month",
    x = NULL,
    y = "Total contracts traded"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p)

# Save the plot
ggsave("output/volume_trend_full_timeline.png", p, width = 12, height = 7, dpi = 300)
cli::cli_alert_success("Saved to output/volume_trend_full_timeline.png")

# Show the actual numbers
cli::cli_h2("Monthly Volume Data (Full Timeline)")
monthly_volume |>
  mutate(
    contracts = scales::comma(total_contracts),
    date = format(cpi_month, "%b %Y")
  ) |>
  select(date, contracts, n_market_types) |>
  print(n = Inf)
