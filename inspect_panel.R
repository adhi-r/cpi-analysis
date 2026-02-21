#!/usr/bin/env Rscript

library(targets)
library(tidyverse)
library(cli)

panel <- tar_read(panel)

cli::cli_h1("Panel Data Inspection")

cli::cli_h2("Overall Stats")
cli::cli_inform("Total events: {nrow(panel)}")
cli::cli_inform("Date range: {min(panel$cpi_month)} to {max(panel$cpi_month)}")

# MAE by CPI type (this is the right way to look at it)
cli::cli_h2("Accuracy by CPI Type")
panel |>
  group_by(cpi_type) |>
  summarise(
    n_events = n(),
    MAE = round(mean(abs_error, na.rm = TRUE), 3),
    median_abs_error = round(median(abs_error, na.rm = TRUE), 3),
    mean_z = round(mean(abs(z_score), na.rm = TRUE), 2),
    RMSE = round(sqrt(mean(error^2, na.rm = TRUE)), 3)
  ) |>
  print()

# Best and worst forecasts
cli::cli_h2("Best Forecasts (lowest error)")
panel |>
  arrange(abs_error) |>
  select(cpi_month, cpi_type, forecast_mean, actual_print, abs_error, total_volume) |>
  head(5) |>
  print()

cli::cli_h2("Worst Forecasts (highest error)")
panel |>
  arrange(desc(abs_error)) |>
  select(cpi_month, cpi_type, forecast_mean, actual_print, abs_error, total_volume) |>
  head(5) |>
  print()

# Sample panel data
cli::cli_h2("Sample Panel Data (10 random events)")
panel |>
  select(cpi_month, cpi_type, total_volume, forecast_mean, forecast_sigma,
         actual_print, abs_error, r_squared) |>
  slice_sample(n = 10) |>
  print()

cli::cli_alert_success("Panel ready for Phase 7 (plots)")
