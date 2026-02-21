#!/usr/bin/env Rscript

# Test script to run Phase 1 and inspect raw data before proceeding
library(tidyverse)
library(httr2)
library(jsonlite)
library(cli)

# Source the fetch function
source("R/fetch_markets.R")

cli::cli_h1("Phase 1: Fetching all CPI markets from Kalshi")

# Fetch all markets
raw_markets <- fetch_all_cpi_markets()

cli::cli_h2("Summary of raw_markets")
cli::cli_inform("Total markets fetched: {nrow(raw_markets)}")
cli::cli_inform("Columns: {paste(names(raw_markets), collapse = ', ')}")

# Look at first raw JSON to understand structure
cli::cli_h2("Sample ticker formats (30 examples)")
set.seed(42)
sample_tickers <- raw_markets |>
  slice_sample(n = min(30, nrow(raw_markets))) |>
  pull(ticker)

print(sample_tickers)

# Show some full examples
cli::cli_h2("First 3 market records (full details)")
raw_markets |> head(3) |> print()

# Check status distribution
cli::cli_h2("Market status distribution")
raw_markets |> count(status, sort = TRUE) |> print()

# Check series_ticker distribution
cli::cli_h2("Series ticker distribution")
raw_markets |> count(series_ticker, sort = TRUE) |> print()

# Save for inspection
write_csv(raw_markets, "output/raw_markets_phase1.csv")
cli::cli_alert_success("Saved to output/raw_markets_phase1.csv")

# Show settled markets only
settled <- raw_markets |> filter(status == "settled")
cli::cli_h2("Settled markets")
cli::cli_inform("Settled markets: {nrow(settled)} / {nrow(raw_markets)}")

if (nrow(settled) > 0) {
  cli::cli_inform("Sample settled tickers:")
  settled |> head(10) |> pull(ticker) |> print()
}
