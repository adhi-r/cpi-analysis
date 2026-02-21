#!/usr/bin/env Rscript

library(targets)
library(tidyverse)
library(cli)

# Source the function
source("R/fetch_trades.R")

# Get 5 test tickers from different time periods
markets <- tar_read(markets)

test_tickers <- markets |>
  group_by(cpi_type) |>
  slice_head(n = 2) |>
  ungroup() |>
  head(5)

cli::cli_h1("Testing trade fetch with 5 tickers")
cli::cli_inform("Test tickers:")
print(test_tickers |> select(ticker, cpi_type, cpi_month, strike_value))

# Fetch trades for each
cli::cli_h2("Fetching trades...")
all_trades <- list()

for (i in seq_len(nrow(test_tickers))) {
  ticker <- test_tickers$ticker[i]
  cli::cli_alert_info("Fetching {ticker}... ({i}/5)")

  trades <- fetch_trades_for_ticker(ticker)
  all_trades[[i]] <- trades

  cli::cli_alert_success("Got {nrow(trades)} trades")
}

# Combine and inspect
combined <- bind_rows(all_trades)

cli::cli_h2("Results")
cli::cli_inform("Total trades: {nrow(combined)}")
cli::cli_inform("Trades with prices: {sum(!is.na(combined$yes_price))}")

if (nrow(combined) > 0) {
  cli::cli_alert_success("Trade fetching is working!")
  cli::cli_inform("\nSample trades:")
  combined |>
    filter(!is.na(yes_price)) |>
    select(ticker, yes_price, created_time) |>
    head(10) |>
    print()
} else {
  cli::cli_alert_danger("No trades found! Something is wrong.")
}
