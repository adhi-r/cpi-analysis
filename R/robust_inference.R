#' Robust Statistical Inference Functions
#'
#' Functions for proper hypothesis testing with heteroskedasticity and
#' autocorrelation consistent (HAC) standard errors, and bootstrap CIs.

#' Compute HAC (Newey-West) standard errors for forecast errors
#'
#' Uses sandwich package for heteroskedasticity and autocorrelation consistent SEs.
#' This accounts for time-series correlation in forecast errors and
#' heteroskedasticity (changing variance over time).
#'
#' @param panel Panel dataset with errors
#' @param max_lag Maximum lag for autocorrelation adjustment (default 3)
#' @return Tibble with HAC standard errors by cpi_type

compute_hac_se <- function(panel, max_lag = 3) {

  # For each CPI type, test if mean forecast error != 0 (test for bias)
  hac_results <- panel |>
    filter(!is.na(error_clean)) |>
    group_by(cpi_type) |>
    summarise(
      n = n(),
      mean_error = mean(error_clean, na.rm = TRUE),
      mae = mean(abs_error_clean, na.rm = TRUE),

      # Fit model: error ~ 1 (just testing mean != 0)
      hac_result = if (n() >= 5) {
        list(tryCatch({
          model <- lm(error_clean ~ 1, data = cur_data())
          # Extract HAC SE using Newey-West
          se <- sqrt(sandwich::vcovHAC(model, lag = max_lag)[1,1])
          t_stat <- mean_error / se
          p_val <- 2 * pt(abs(t_stat), df = n() - 1, lower.tail = FALSE)
          list(hac_se = se, t_stat_hac = t_stat, p_value_hac = p_val)
        }, error = function(e) {
          list(hac_se = NA_real_, t_stat_hac = NA_real_, p_value_hac = NA_real_)
        }))
      } else {
        list(list(hac_se = NA_real_, t_stat_hac = NA_real_, p_value_hac = NA_real_))
      },

      .groups = "drop"
    ) |>
    unnest_wider(hac_result)

  hac_results
}


#' Bootstrap confidence intervals for forecast accuracy metrics
#'
#' Non-parametric bootstrap (resampling with replacement) to estimate
#' sampling distribution of MAE and construct percentile confidence intervals.
#'
#' @param panel Panel dataset
#' @param n_boot Number of bootstrap replications (default 1000)
#' @param seed Random seed for reproducibility
#' @return Tibble with bootstrap CIs

bootstrap_accuracy_ci <- function(panel, n_boot = 1000, seed = 42) {

  set.seed(seed)

  # For each CPI type, bootstrap MAE
  boot_results <- panel |>
    filter(!is.na(abs_error_clean)) |>
    group_by(cpi_type) |>
    summarise(
      n = n(),
      mae_observed = mean(abs_error_clean, na.rm = TRUE),

      # Bootstrap distribution
      boot_dist = if (n() >= 5) {
        list(replicate(n_boot, {
          sample_idx <- sample(n(), replace = TRUE)
          mean(abs_error_clean[sample_idx], na.rm = TRUE)
        }))
      } else {
        list(rep(NA_real_, n_boot))
      },

      # Compute percentile CI
      ci_lower = quantile(boot_dist[[1]], 0.025, na.rm = TRUE),
      ci_upper = quantile(boot_dist[[1]], 0.975, na.rm = TRUE),
      boot_se = sd(boot_dist[[1]], na.rm = TRUE),

      .groups = "drop"
    ) |>
    select(-boot_dist)

  boot_results
}
