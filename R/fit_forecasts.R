#' Compute terminal prices for each strike using last N contracts
#'
#' Instead of using just the last trade (which can be noisy), we average
#' the last N contracts to get a more robust terminal price.
#'
#' @param trades Trades data frame
#' @param n_contracts Number of contracts to average (default 1000)

compute_terminal_prices <- function(trades, n_contracts = 1000) {
  trades |>
    mutate(created_time = ymd_hms(created_time)) |>
    filter(!is.na(yes_price)) |>
    group_by(event_ticker, cpi_type, cpi_month, ticker, strike_value) |>
    arrange(desc(created_time)) |>
    # Take last n_contracts contracts and compute volume-weighted average price
    mutate(
      cumulative_count = cumsum(count),
      in_window = cumulative_count <= n_contracts
    ) |>
    filter(in_window) |>
    summarise(
      # Volume-weighted average of yes_price
      terminal_prob = sum(yes_price * count) / sum(count) / 100,
      n_trades_used = n(),
      total_contracts_used = sum(count),
      .groups = "drop"
    ) |>
    select(
      event_ticker,
      cpi_type,
      cpi_month,
      strike_value,
      terminal_prob,
      n_trades_used,
      total_contracts_used
    )
}

#' Fit a normal distribution to the implied CDF from strike prices
#'
#' IMPORTANT: Markets settle on ROUNDED values. If strike = 0.5%, the market
#' settles YES if actual >= 0.5% AFTER rounding. Since BLS rounds to 1 decimal,
#' this means actual >= 0.45% (the rounding threshold).
#'
#' Each strike's yes_price gives P(actual >= rounding_threshold).
#' We fit N(mu, sigma) to the actual unrounded values, not the strikes.
#'
#' @param strikes Numeric vector of strike values (rounded)
#' @param probs Numeric vector of P(CPI >= strike) for each strike
#' @return A tibble with one row: forecast_mean, forecast_sigma, r_squared

fit_normal_cdf <- function(strikes, probs) {

  # Calculate rounding thresholds
  # BLS rounds to 1 decimal, so threshold = strike - 0.05
  # Example: strike 0.5% → threshold 0.45% (since 0.45% rounds up to 0.5%)
  thresholds <- strikes - 0.05

  # Convert to CDF: P(CPI < threshold) = 1 - P(CPI >= threshold)
  cdf_vals <- 1 - probs

  # Filter to informative strikes (not near 0 or 1)
  informative <- probs > 0.02 & probs < 0.98
  if (sum(informative) < 2) {
    return(tibble(
      forecast_mean = NA_real_,
      forecast_sigma = NA_real_,
      r_squared = NA_real_,
      n_informative = sum(informative)
    ))
  }

  t <- thresholds[informative]  # use thresholds, not strikes
  c <- cdf_vals[informative]

  # Objective: minimize sum of squared residuals
  obj <- function(par) {
    mu <- par[1]
    sigma <- exp(par[2])  # log-transform to keep sigma > 0
    predicted <- pnorm(t, mean = mu, sd = sigma)  # fit to thresholds
    sum((predicted - c)^2)
  }

  # Initial guess based on thresholds
  init <- c(mean(t), log(0.1))
  fit <- optim(init, obj, method = "Nelder-Mead")

  mu <- fit$par[1]
  sigma <- exp(fit$par[2])

  # R-squared as fit quality metric
  predicted <- pnorm(t, mean = mu, sd = sigma)
  ss_res <- sum((c - predicted)^2)
  ss_tot <- sum((c - mean(c))^2)
  r2 <- ifelse(ss_tot > 0, 1 - ss_res / ss_tot, NA_real_)

  tibble(
    forecast_mean  = mu,
    forecast_sigma = sigma,
    r_squared      = r2,
    n_informative  = sum(informative)
  )
}

#' Apply distribution fitting to all events
#'
#' @param terminal_prices Output of compute_terminal_prices()

fit_all_forecasts <- function(terminal_prices) {
  terminal_prices |>
    group_by(event_ticker, cpi_type, cpi_month) |>
    summarise(
      fit = list(fit_normal_cdf(strike_value, terminal_prob)),
      .groups = "drop"
    ) |>
    unnest(fit)
}

#' Refit forecasts using only strikes near actual outcome
#'
#' This filters to the 2 strikes below and 2 strikes above the actual print,
#' which helps eliminate errant price action in tail strikes.
#'
#' @param terminal_prices Terminal prices data
#' @param actuals Actuals data with actual_print column
#' @return Forecasts refitted using only informative strikes

fit_clean_forecasts <- function(terminal_prices, actuals) {

  # Join terminal prices with actuals to know which strikes matter
  terminal_with_actual <- terminal_prices |>
    left_join(actuals, by = "cpi_month") |>
    mutate(
      actual_value = case_when(
        cpi_type == "headline_mom" ~ actual_mom,
        cpi_type == "headline_yoy" ~ actual_yoy,
        cpi_type == "core_mom"     ~ actual_core_mom,
        cpi_type == "core_yoy"     ~ actual_core_yoy,
        TRUE                        ~ NA_real_
      )
    ) |>
    filter(!is.na(actual_value))

  # For each event, find strikes within ±2 of actual
  clean_strikes <- terminal_with_actual |>
    group_by(event_ticker, cpi_type, cpi_month, actual_value) |>
    arrange(strike_value) |>
    mutate(
      rank_from_actual = rank(abs(strike_value - actual_value)),
      # Keep 2 below, 2 above, and any ties
      keep = rank_from_actual <= 4
    ) |>
    filter(keep) |>
    # QUALITY CHECK: Must have strikes on BOTH sides of actual
    mutate(
      has_below = any(strike_value < actual_value),
      has_above = any(strike_value > actual_value),
      valid_event = has_below & has_above
    ) |>
    filter(valid_event) |>
    ungroup()

  # Refit using only these strikes
  clean_strikes |>
    group_by(event_ticker, cpi_type, cpi_month) |>
    summarise(
      fit = list(fit_normal_cdf(strike_value, terminal_prob)),
      .groups = "drop"
    ) |>
    unnest(fit) |>
    rename_with(~paste0(., "_clean"), c(forecast_mean, forecast_sigma, r_squared, n_informative))
}
