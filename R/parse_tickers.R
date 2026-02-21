#' Parse CPI market tickers and add structured columns
#'
#' Based on actual Kalshi API response structure:
#' - series_ticker field is NA, so we parse series from ticker
#' - status is "finalized" not "settled"
#' - floor_strike contains the actual strike value
#'
#' Ticker formats observed:
#'   KXCPI-25DEC-T0.3       (headline MoM, new naming)
#'   KXCPIYOY-25JAN-T2.2    (headline YoY)
#'   KXCPICOREYOY-24FEB-T3.6 (core YoY)
#'   KXCPICORE-25FEB-T0.0   (core MoM)
#'   CPI-22SEP-T0.2         (old MoM format from 2022)

parse_and_clean_markets <- function(markets_df) {

  month_map <- c(
    JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6,
    JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12
  )

  parsed <- markets_df |>
    mutate(
      # Extract series and date from ticker
      # Format: SERIES-YYMMM-T#.# or SERIES-YYMMM-TN#.# (negative)
      parsed = str_match(ticker, "^([A-Za-z]+)-?(\\d{2}[A-Z]{3})"),
      series    = parsed[, 2],
      date_code = parsed[, 3],

      # Parse strike from ticker for cases where floor_strike is NA
      # Handle both T#.# and TN#.# (negative) formats
      strike_parsed = str_match(ticker, "-T(N?)([\\d.]+)$"),
      is_negative   = strike_parsed[, 2] == "N",
      strike_from_ticker = as.numeric(strike_parsed[, 3]) * ifelse(is_negative, -1, 1),

      # Use floor_strike if available, otherwise use parsed value
      strike_value = if_else(is.na(floor_strike), strike_from_ticker, floor_strike),

      # Parse date_code to actual month
      year  = as.integer(paste0("20", str_sub(date_code, 1, 2))),
      month = month_map[str_sub(date_code, 3, 5)],
      cpi_month = make_date(year, month, 1L),

      # Classify CPI type based on series prefix
      cpi_type = case_when(
        str_detect(series, regex("coreyoy|cpicoreyoy", ignore_case = TRUE)) ~ "core_yoy",
        str_detect(series, regex("yoy", ignore_case = TRUE))                ~ "headline_yoy",
        str_detect(series, regex("core|cpicore", ignore_case = TRUE))       ~ "core_mom",
        TRUE                                                                 ~ "headline_mom"
      )
    ) |>
    select(-parsed, -strike_parsed, -is_negative, -strike_from_ticker, -year, -month) |>
    # Keep only finalized markets for the analysis
    filter(status == "finalized")

  # Validation
  n_unparsed <- sum(is.na(parsed$strike_value))
  if (n_unparsed > 0) {
    cli::cli_warn("{n_unparsed} tickers failed to parse. Inspect these:")
    failed <- parsed |> filter(is.na(strike_value)) |> pull(ticker) |> head(20)
    print(failed)
  }

  n_no_series <- sum(is.na(parsed$series))
  if (n_no_series > 0) {
    cli::cli_warn("{n_no_series} tickers failed to extract series name.")
  }

  parsed
}
