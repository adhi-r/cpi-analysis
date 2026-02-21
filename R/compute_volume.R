#' Compute volume time series from market metadata
#'
#' This is the fast path: uses the volume field on the market object,
#' no trade log needed.

compute_volume_timeseries <- function(markets) {
  markets |>
    group_by(cpi_type, cpi_month, event_ticker) |>
    summarise(
      total_volume  = sum(volume, na.rm = TRUE),
      num_strikes   = n(),
      .groups = "drop"
    ) |>
    arrange(cpi_type, cpi_month)
}
