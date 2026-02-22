#' Alternative Distribution Fitting Functions for Robustness Analysis
#'
#' This file contains functions for testing distributional assumptions
#' and strike window sensitivity in the CPI forecast extraction pipeline.

#' Fit a Student's t-distribution to the implied CDF
#'
#' Similar to fit_normal_cdf but allows heavier tails via degrees of freedom parameter.
#' Uses metRology::pt.scaled() for location-scale t-distribution.
#'
#' @param strikes Numeric vector of strike values (rounded BLS values)
#' @param probs Numeric vector of P(CPI >= strike) for each strike (terminal yes_price / 100)
#' @return A tibble with: forecast_mean, forecast_sigma, forecast_df, r_squared, n_informative

fit_t_cdf <- function(strikes, probs) {

  # Calculate rounding thresholds (same logic as normal fit)
  # Strikes are rounded values (0.5%, 0.7%, etc.)
  # Markets settle YES if actual >= threshold
  # BLS rounds MoM to 1 decimal → threshold = strike - 0.05
  thresholds <- strikes - 0.05

  # Convert yes_price to CDF: P(CPI < threshold)
  cdf_vals <- 1 - probs

  # Filter to informative strikes (avoid extreme tails with high uncertainty)
  informative <- probs > 0.02 & probs < 0.98

  # Need at least 3 points for 3-parameter fit (mu, sigma, df)
  if (sum(informative) < 3) {
    return(tibble(
      forecast_mean = NA_real_,
      forecast_sigma = NA_real_,
      forecast_df = NA_real_,
      r_squared = NA_real_,
      n_informative = sum(informative)
    ))
  }

  t <- thresholds[informative]
  c <- cdf_vals[informative]

  # Objective: minimize sum of squared residuals
  # par = c(mu, log(sigma), log(df))
  # Use log-transform to ensure positivity of sigma and df
  obj <- function(par) {
    mu <- par[1]
    sigma <- exp(par[2])  # ensure positive
    df <- exp(par[3])      # ensure positive

    # Use metRology package for location-scale t
    predicted <- metRology::pt.scaled(t, df = df, mean = mu, sd = sigma)
    sum((predicted - c)^2)
  }

  # Initial guess: start with high df (approaches normal)
  init <- c(mean(t), log(0.1), log(10))

  fit <- tryCatch(
    optim(init, obj, method = "Nelder-Mead", control = list(maxit = 1000)),
    error = function(e) list(convergence = 1)
  )

  if (fit$convergence != 0) {
    return(tibble(
      forecast_mean = NA_real_,
      forecast_sigma = NA_real_,
      forecast_df = NA_real_,
      r_squared = NA_real_,
      n_informative = sum(informative)
    ))
  }

  # Extract parameters
  mu <- fit$par[1]
  sigma <- exp(fit$par[2])
  df <- exp(fit$par[3])

  # Compute R-squared
  predicted <- metRology::pt.scaled(t, df = df, mean = mu, sd = sigma)
  ss_res <- sum((c - predicted)^2)
  ss_tot <- sum((c - mean(c))^2)
  r2 <- ifelse(ss_tot > 0, 1 - ss_res / ss_tot, NA_real_)

  tibble(
    forecast_mean = mu,
    forecast_sigma = sigma,
    forecast_df = df,
    r_squared = r2,
    n_informative = sum(informative)
  )
}


#' Fit a non-parametric CDF using linear interpolation
#'
#' No distributional assumption. Interpolates between strike probabilities
#' to find the median (and IQR) directly from the empirical CDF.
#'
#' @param strikes Numeric vector of strike values (rounded)
#' @param probs Numeric vector of P(CPI >= strike) for each strike
#' @return A tibble with: forecast_median, forecast_iqr, r_squared, n_informative

fit_nonparametric_cdf <- function(strikes, probs) {

  # Calculate rounding thresholds
  thresholds <- strikes - 0.05
  cdf_vals <- 1 - probs

  # Filter to informative strikes
  informative <- probs > 0.02 & probs < 0.98

  if (sum(informative) < 2) {
    return(tibble(
      forecast_median = NA_real_,
      forecast_iqr = NA_real_,
      r_squared = NA_real_,
      n_informative = sum(informative)
    ))
  }

  t <- thresholds[informative]
  c <- cdf_vals[informative]

  # Sort by threshold
  ord <- order(t)
  t <- t[ord]
  c <- c[ord]

  # Linear interpolation to find quantiles
  # Median: where CDF = 0.5
  forecast_median <- tryCatch(
    approx(c, t, xout = 0.5, rule = 2)$y,
    error = function(e) NA_real_
  )

  # IQR: Q3 - Q1
  q25 <- tryCatch(
    approx(c, t, xout = 0.25, rule = 2)$y,
    error = function(e) NA_real_
  )
  q75 <- tryCatch(
    approx(c, t, xout = 0.75, rule = 2)$y,
    error = function(e) NA_real_
  )
  forecast_iqr <- q75 - q25

  # R-squared: not very meaningful for non-parametric, but include for consistency
  # Compare interpolated CDF to empirical points
  predicted <- approx(t, c, xout = t, rule = 2)$y
  ss_res <- sum((c - predicted)^2, na.rm = TRUE)
  ss_tot <- sum((c - mean(c))^2)
  r2 <- ifelse(ss_tot > 0, 1 - ss_res / ss_tot, NA_real_)

  tibble(
    forecast_median = forecast_median,
    forecast_iqr = forecast_iqr,
    r_squared = r2,
    n_informative = sum(informative)
  )
}


#' Fit forecasts with varying strike window size
#'
#' Wrapper around fit_normal_cdf that filters to N strikes each side of actual.
#' Replicates the logic from fit_clean_forecasts() but allows varying N.
#'
#' @param terminal_prices Terminal prices data (output of compute_terminal_prices)
#' @param actuals Actuals data with actual_print column
#' @param n_strikes_each_side Number of strikes to keep on each side (default 2)
#' @return Forecasts fitted using specified strike window

fit_forecasts_window <- function(terminal_prices, actuals, n_strikes_each_side = 2) {

  # Join terminal prices with actuals to know where actual landed
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

  # For each event, find strikes within ±N of actual
  windowed_strikes <- terminal_with_actual |>
    group_by(event_ticker, cpi_type, cpi_month, actual_value) |>
    arrange(strike_value) |>
    mutate(
      rank_from_actual = rank(abs(strike_value - actual_value)),
      # Keep top N below + top N above (total 2N, or fewer if tied)
      keep = rank_from_actual <= (2 * n_strikes_each_side)
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

  # Fit using windowed strikes (source fit_normal_cdf from fit_forecasts.R)
  windowed_strikes |>
    group_by(event_ticker, cpi_type, cpi_month) |>
    summarise(
      fit = list(fit_normal_cdf(strike_value, terminal_prob)),
      .groups = "drop"
    ) |>
    unnest(fit) |>
    mutate(strike_window = n_strikes_each_side)
}


#' Apply t-distribution fitting to all events
#'
#' @param terminal_prices Output of compute_terminal_prices()
#' @return Tibble with t-distribution forecasts for all events

fit_all_forecasts_t <- function(terminal_prices) {
  terminal_prices |>
    group_by(event_ticker, cpi_type, cpi_month) |>
    summarise(
      fit = list(fit_t_cdf(strike_value, terminal_prob)),
      .groups = "drop"
    ) |>
    unnest(fit)
}


#' Apply non-parametric fitting to all events
#'
#' @param terminal_prices Output of compute_terminal_prices()
#' @return Tibble with non-parametric forecasts for all events

fit_all_forecasts_nonparametric <- function(terminal_prices) {
  terminal_prices |>
    group_by(event_ticker, cpi_type, cpi_month) |>
    summarise(
      fit = list(fit_nonparametric_cdf(strike_value, terminal_prob)),
      .groups = "drop"
    ) |>
    unnest(fit)
}
