#!/usr/bin/env Rscript

library(targets)
library(tidyverse)
library(cli)

# Load the volume time series from targets cache
volume_ts <- tar_read(volume_ts)

cli::cli_h1("Volume Time Series Summary")

cli::cli_h2("Overall Statistics")
cli::cli_inform("Total event-months: {nrow(volume_ts)}")
cli::cli_inform("Date range: {min(volume_ts$cpi_month)} to {max(volume_ts$cpi_month)}")
cli::cli_inform("Total volume (all time): {scales::comma(sum(volume_ts$total_volume))}")

cli::cli_h2("Volume by CPI Type")
volume_ts |>
  group_by(cpi_type) |>
  summarise(
    n_events = n(),
    total_vol = sum(total_volume),
    avg_vol = mean(total_volume),
    median_vol = median(total_volume),
    max_vol = max(total_volume)
  ) |>
  mutate(across(where(is.numeric), ~scales::comma(.))) |>
  print()

cli::cli_h2("Volume Trend (first vs last 6 months)")

early <- volume_ts |> filter(cpi_month < "2023-01-01")
recent <- volume_ts |> filter(cpi_month >= "2025-01-01")

cli::cli_inform("Early (2022): avg {scales::comma(round(mean(early$total_volume)))} per event")
cli::cli_inform("Recent (2025+): avg {scales::comma(round(mean(recent$total_volume)))} per event")
cli::cli_inform("Change: {round((mean(recent$total_volume) / mean(early$total_volume) - 1) * 100, 1)}%")

cli::cli_h2("Sample Data (10 most recent events)")
volume_ts |>
  arrange(desc(cpi_month)) |>
  head(10) |>
  select(cpi_type, cpi_month, total_volume, num_strikes) |>
  print()

# Save
write_csv(volume_ts, "output/volume_timeseries.csv")
cli::cli_alert_success("Saved to output/volume_timeseries.csv")
