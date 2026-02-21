#!/usr/bin/env Rscript

library(targets)
library(tidyverse)
library(cli)

# Load trades from targets cache
trades <- tar_read(trades)

cli::cli_h1("Trade Data Inspection")

cli::cli_h2("Overall Stats")
cli::cli_inform("Total trades (closing day only): {scales::comma(nrow(trades))}")
cli::cli_inform("Unique tickers: {length(unique(trades$ticker))}")
cli::cli_inform("Date range: {min(trades$created_time)} to {max(trades$created_time)}")

# Check coverage
cli::cli_h2("Coverage by CPI Type")
trades |>
  filter(!is.na(yes_price)) |>
  group_by(cpi_type) |>
  summarise(
    n_tickers = n_distinct(ticker),
    n_trades = n(),
    avg_trades_per_ticker = round(n() / n_distinct(ticker), 1)
  ) |>
  print()

# Sample some trades
cli::cli_h2("Sample Trades (5 random examples)")
trades |>
  filter(!is.na(yes_price)) |>
  select(ticker, cpi_month, strike_value, yes_price, created_time) |>
  slice_sample(n = 5) |>
  print()

# Check for tickers with no trades
no_trades <- trades |>
  group_by(ticker) |>
  summarise(has_trades = any(!is.na(yes_price))) |>
  filter(!has_trades)

if (nrow(no_trades) > 0) {
  cli::cli_alert_warning("{nrow(no_trades)} tickers have no trades on closing day")
  cli::cli_inform("This might be expected for very illiquid strikes")
} else {
  cli::cli_alert_success("All tickers have at least one trade on closing day!")
}

# Preview what terminal prices will look like
cli::cli_h2("Preview: Terminal Prices (last trade per ticker)")
terminal_preview <- trades |>
  filter(!is.na(yes_price)) |>
  mutate(created_time = ymd_hms(created_time)) |>
  group_by(ticker, cpi_type, cpi_month, strike_value) |>
  arrange(created_time) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(ticker, cpi_month, cpi_type, strike_value, terminal_price = yes_price)

terminal_preview |>
  slice_sample(n = 10) |>
  print()

cli::cli_alert_success("Ready to proceed to Phase 4 (fit forecasts)")
