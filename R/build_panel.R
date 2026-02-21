#' Join forecasts + actuals + volume into the final panel
#'
#' Matches the correct "actual" column to each cpi_type.
#'
#' IMPORTANT: Both forecast_mean and actual_print are PRECISE values
#' (multiple decimals), not the rounded values published by BLS.
#' - forecast_mean: fitted to rounding thresholds in fit_forecasts.R
#' - actual_print: calculated from CPI index, not rounded published rate

build_panel <- function(forecasts, actuals, volume_ts) {

  # Join forecasts with volume
  panel <- forecasts |>
    left_join(volume_ts, by = c("cpi_type", "cpi_month", "event_ticker"))

  # Join with actuals — pick the right actual column per cpi_type
  # These are PRECISE rates from the index, not rounded BLS publications
  panel <- panel |>
    left_join(actuals, by = "cpi_month") |>
    mutate(
      actual_print = case_when(
        cpi_type == "headline_mom" ~ actual_mom,
        cpi_type == "headline_yoy" ~ actual_yoy,
        cpi_type == "core_mom"     ~ actual_core_mom,
        cpi_type == "core_yoy"     ~ actual_core_yoy,
        TRUE                        ~ NA_real_
      ),
      # Error between precise forecast and precise actual
      error      = forecast_mean - actual_print,
      abs_error  = abs(error),
      z_score    = (actual_print - forecast_mean) / forecast_sigma
    ) |>
    select(
      cpi_type, cpi_month, event_ticker,
      total_volume, num_strikes,
      forecast_mean, forecast_sigma, n_informative, r_squared,
      actual_print, error, abs_error, z_score
    ) |>
    arrange(cpi_type, cpi_month)

  # Note: Summary stats printed after clean forecasts joined in _targets.R

  panel
}
