library(httr2)
library(jsonlite)
library(tidyverse)

# Test with one ticker
test_ticker <- "KXCPI-26JAN-T0.2"
base_url <- "https://api.elections.kalshi.com/trade-api/v2"

# Try WITHOUT time filter first
cat("Fetching trades WITHOUT time filter...\n")
resp1 <- request(base_url) |>
  req_url_path_append("markets", "trades") |>
  req_url_query(ticker = test_ticker, limit = 10) |>
  req_perform() |>
  resp_body_json()

cat("Number of trades:", length(resp1$trades), "\n")
if (length(resp1$trades) > 0) {
  cat("First trade:\n")
  str(resp1$trades[[1]])
}

# Try WITH time filter
cat("\nFetching trades WITH time filter (min_ts)...\n")
resp2 <- request(base_url) |>
  req_url_path_append("markets", "trades") |>
  req_url_query(ticker = test_ticker, limit = 10, min_ts = "2026-02-13T00:00:00Z") |>
  req_perform() |>
  resp_body_json()

cat("Number of trades with filter:", length(resp2$trades), "\n")
