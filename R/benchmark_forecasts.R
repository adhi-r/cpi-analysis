#' Benchmark Forecast Functions
#'
#' Naive forecasting methods to compare against Kalshi market forecasts.
#' These benchmarks help establish whether prediction markets add value
#' beyond simple time-series extrapolation.

#' Generate random walk benchmark forecasts
#'
#' Random walk model: forecast = last period's actual value.
#' This is the simplest possible forecast and surprisingly hard to beat
#' for many economic time series.
#'
#' @param actuals Actuals data frame with cpi_month and actual columns
#' @return Tibble with benchmark forecasts (forecast_rw, forecast_sigma_rw)

generate_random_walk_forecasts <- function(actuals) {

  # For each CPI type, create lagged forecast
  rw_forecasts <- tibble()

  for (type in c("headline_mom", "headline_yoy", "core_mom", "core_yoy")) {

    actual_col <- case_when(
      type == "headline_mom" ~ "actual_mom",
      type == "headline_yoy" ~ "actual_yoy",
      type == "core_mom"     ~ "actual_core_mom",
      type == "core_yoy"     ~ "actual_core_yoy"
    )

    type_forecasts <- actuals |>
      select(cpi_month, !!sym(actual_col)) |>
      arrange(cpi_month) |>
      mutate(
        cpi_type = type,
        # Forecast = lag(actual, 1)
        forecast_rw = lag(!!sym(actual_col), 1),
        # Forecast uncertainty = rolling SD of past 6 months
        forecast_sigma_rw = zoo::rollapply(
          !!sym(actual_col),
          width = 6,
          FUN = sd,
          fill = NA,
          align = "right",
          partial = FALSE
        )
      ) |>
      select(cpi_type, cpi_month, forecast_rw, forecast_sigma_rw)

    rw_forecasts <- bind_rows(rw_forecasts, type_forecasts)
  }

  rw_forecasts
}


#' Generate moving average benchmark forecasts
#'
#' MA model: forecast = average of last N months.
#' Smooths out short-term volatility but may lag turning points.
#'
#' @param actuals Actuals data frame
#' @param n_months Window size for moving average (default 3)
#' @return Tibble with MA forecasts (forecast_ma, forecast_sigma_ma)

generate_ma_forecasts <- function(actuals, n_months = 3) {

  ma_forecasts <- tibble()

  for (type in c("headline_mom", "headline_yoy", "core_mom", "core_yoy")) {

    actual_col <- case_when(
      type == "headline_mom" ~ "actual_mom",
      type == "headline_yoy" ~ "actual_yoy",
      type == "core_mom"     ~ "actual_core_mom",
      type == "core_yoy"     ~ "actual_core_yoy"
    )

    type_forecasts <- actuals |>
      select(cpi_month, !!sym(actual_col)) |>
      arrange(cpi_month) |>
      mutate(
        cpi_type = type,
        # MA forecast (lagged to avoid look-ahead bias)
        forecast_ma = zoo::rollapply(
          !!sym(actual_col),
          width = n_months,
          FUN = mean,
          fill = NA,
          align = "right",
          partial = FALSE
        ),
        forecast_ma = lag(forecast_ma, 1),  # CRITICAL: lag to avoid look-ahead
        # Forecast uncertainty = rolling SD
        forecast_sigma_ma = zoo::rollapply(
          !!sym(actual_col),
          width = n_months,
          FUN = sd,
          fill = NA,
          align = "right",
          partial = FALSE
        ),
        forecast_sigma_ma = lag(forecast_sigma_ma, 1)
      ) |>
      select(cpi_type, cpi_month, forecast_ma, forecast_sigma_ma)

    ma_forecasts <- bind_rows(ma_forecasts, type_forecasts)
  }

  ma_forecasts
}
