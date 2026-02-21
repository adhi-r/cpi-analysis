#!/usr/bin/env Rscript

# Test the updated parser with actual Phase 1 data
library(tidyverse)
library(cli)

# Source the parse function
source("R/parse_tickers.R")

# Read the raw markets we fetched earlier
raw_markets <- read_csv("output/raw_markets_phase1.csv", show_col_types = FALSE)

cli::cli_h1("Testing Updated Parser")

# Apply the parser
markets <- parse_and_clean_markets(raw_markets)

cli::cli_h2("Parsing Results")
cli::cli_inform("Raw markets: {nrow(raw_markets)}")
cli::cli_inform("Finalized markets: {nrow(markets)}")
cli::cli_inform("Parsing success rate: {round(nrow(markets)/sum(raw_markets$status == 'finalized') * 100, 1)}%")

# Check series distribution
cli::cli_h2("Series Distribution")
markets |> count(series, sort = TRUE) |> print()

# Check CPI type distribution
cli::cli_h2("CPI Type Distribution")
markets |> count(cpi_type, sort = TRUE) |> print()

# Check date range
cli::cli_h2("Date Range")
cli::cli_inform("First month: {min(markets$cpi_month)}")
cli::cli_inform("Last month: {max(markets$cpi_month)}")

# Sample parsed records
cli::cli_h2("Sample Parsed Records (5 examples)")
markets |>
  select(ticker, series, cpi_month, cpi_type, strike_value, volume) |>
  slice_sample(n = 5) |>
  print()

# Check for parsing issues
cli::cli_h2("Validation Checks")
n_no_strike <- sum(is.na(markets$strike_value))
n_no_date <- sum(is.na(markets$cpi_month))
n_no_series <- sum(is.na(markets$series))

if (n_no_strike > 0) cli::cli_alert_warning("{n_no_strike} markets missing strike_value")
if (n_no_date > 0) cli::cli_alert_warning("{n_no_date} markets missing cpi_month")
if (n_no_series > 0) cli::cli_alert_warning("{n_no_series} markets missing series")

if (n_no_strike == 0 && n_no_date == 0 && n_no_series == 0) {
  cli::cli_alert_success("All markets parsed successfully!")
}

# Show strike value distribution per type
cli::cli_h2("Strike Value Ranges by CPI Type")
markets |>
  group_by(cpi_type) |>
  summarise(
    min_strike = min(strike_value, na.rm = TRUE),
    max_strike = max(strike_value, na.rm = TRUE),
    n_markets = n()
  ) |>
  print()

# Save parsed markets
write_csv(markets, "output/parsed_markets_test.csv")
cli::cli_alert_success("Saved to output/parsed_markets_test.csv")
