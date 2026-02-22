#' Specification Curve Analysis Functions
#'
#' Combines results from all robustness checks into a single dataset
#' showing how findings vary across reasonable methodological choices.
#' This addresses researcher degrees of freedom and demonstrates
#' robustness (or sensitivity) of main results.

#' Combine all robustness results into specification curve dataset
#'
#' Creates a long-format dataset with one row per specification.
#' Each row represents: (cpi_type, method, strike_window, sample_period) combination.
#'
#' @param baseline_panel Panel from baseline method (normal, 2 strikes, full sample)
#' @param t_dist_panel Panel from t-distribution method
#' @param nonparam_panel Panel from non-parametric method
#' @param window_1_panel Panel from 1-strike window
#' @param window_3_panel Panel from 3-strike window
#' @param window_4_panel Panel from 4-strike window
#' @param rw_panel Panel with random walk forecasts joined
#' @param ma_panel Panel with moving average forecasts joined
#' @param pre_2024_panel Panel filtered to pre-2024
#' @param post_2024_panel Panel filtered to post-2024
#' @return Long-format tibble for specification curve analysis

build_specification_curve <- function(
  baseline_panel,
  t_dist_panel,
  nonparam_panel,
  window_1_panel,
  window_3_panel,
  window_4_panel,
  panel_with_benchmarks,
  pre_2024_panel,
  post_2024_panel
) {

  # Helper function to summarize a panel
  summarize_panel <- function(panel, method, window, period, error_col = "abs_error_clean") {

    panel |>
      filter(!is.na(!!sym(error_col))) |>
      group_by(cpi_type) |>
      summarise(
        method = method,
        strike_window = window,
        sample_period = period,
        n_events = n(),
        mae = mean(!!sym(error_col), na.rm = TRUE),
        median_ae = median(!!sym(error_col), na.rm = TRUE),
        rmse = sqrt(mean((!!sym(gsub("abs_error", "error", error_col)))^2, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # Baseline: normal distribution, 2 strikes, full sample
  spec_baseline <- summarize_panel(baseline_panel, "normal", 2, "full")

  # T-distribution (uses abs_error not abs_error_clean)
  spec_t <- t_dist_panel |>
    filter(!is.na(abs_error)) |>
    group_by(cpi_type) |>
    summarise(
      method = "t_distribution",
      strike_window = 2,
      sample_period = "full",
      n_events = n(),
      mae = mean(abs_error, na.rm = TRUE),
      median_ae = median(abs_error, na.rm = TRUE),
      rmse = sqrt(mean(error^2, na.rm = TRUE)),
      .groups = "drop"
    )

  # Non-parametric (uses forecast_median, different error structure)
  spec_nonparam <- nonparam_panel |>
    filter(!is.na(abs_error)) |>
    group_by(cpi_type) |>
    summarise(
      method = "nonparametric",
      strike_window = 2,
      sample_period = "full",
      n_events = n(),
      mae = mean(abs_error, na.rm = TRUE),
      median_ae = median(abs_error, na.rm = TRUE),
      rmse = sqrt(mean(error^2, na.rm = TRUE)),
      .groups = "drop"
    )

  # Strike window variations (use existing error/abs_error columns)
  spec_window_1 <- window_1_panel |>
    filter(!is.na(abs_error)) |>
    group_by(cpi_type) |>
    summarise(
      method = "normal",
      strike_window = 1,
      sample_period = "full",
      n_events = n(),
      mae = mean(abs_error, na.rm = TRUE),
      median_ae = median(abs_error, na.rm = TRUE),
      rmse = sqrt(mean(error^2, na.rm = TRUE)),
      .groups = "drop"
    )

  spec_window_3 <- window_3_panel |>
    filter(!is.na(abs_error)) |>
    group_by(cpi_type) |>
    summarise(
      method = "normal",
      strike_window = 3,
      sample_period = "full",
      n_events = n(),
      mae = mean(abs_error, na.rm = TRUE),
      median_ae = median(abs_error, na.rm = TRUE),
      rmse = sqrt(mean(error^2, na.rm = TRUE)),
      .groups = "drop"
    )

  spec_window_4 <- window_4_panel |>
    filter(!is.na(abs_error)) |>
    group_by(cpi_type) |>
    summarise(
      method = "normal",
      strike_window = 4,
      sample_period = "full",
      n_events = n(),
      mae = mean(abs_error, na.rm = TRUE),
      median_ae = median(abs_error, na.rm = TRUE),
      rmse = sqrt(mean(error^2, na.rm = TRUE)),
      .groups = "drop"
    )

  # Temporal splits
  spec_pre <- summarize_panel(pre_2024_panel, "normal", 2, "pre_2024")
  spec_post <- summarize_panel(post_2024_panel, "normal", 2, "post_2024")

  # Benchmarks
  spec_rw <- panel_with_benchmarks |>
    filter(!is.na(forecast_rw), !is.na(actual_print)) |>
    mutate(
      error_rw = forecast_rw - actual_print,
      abs_error_rw = abs(error_rw)
    ) |>
    filter(!is.na(abs_error_rw)) |>
    group_by(cpi_type) |>
    summarise(
      method = "random_walk",
      strike_window = NA_real_,
      sample_period = "full",
      n_events = n(),
      mae = mean(abs_error_rw, na.rm = TRUE),
      median_ae = median(abs_error_rw, na.rm = TRUE),
      rmse = sqrt(mean(error_rw^2, na.rm = TRUE)),
      .groups = "drop"
    )

  spec_ma <- panel_with_benchmarks |>
    filter(!is.na(forecast_ma), !is.na(actual_print)) |>
    mutate(
      error_ma = forecast_ma - actual_print,
      abs_error_ma = abs(error_ma)
    ) |>
    filter(!is.na(abs_error_ma)) |>
    group_by(cpi_type) |>
    summarise(
      method = "moving_average_3",
      strike_window = NA_real_,
      sample_period = "full",
      n_events = n(),
      mae = mean(abs_error_ma, na.rm = TRUE),
      median_ae = median(abs_error_ma, na.rm = TRUE),
      rmse = sqrt(mean(error_ma^2, na.rm = TRUE)),
      .groups = "drop"
    )

  # Combine all
  bind_rows(
    spec_baseline,
    spec_t,
    spec_nonparam,
    spec_window_1,
    spec_window_3,
    spec_window_4,
    spec_pre,
    spec_post,
    spec_rw,
    spec_ma
  ) |>
    arrange(cpi_type, method, sample_period)
}


#' Plot specification curve
#'
#' Visualizes how MAE varies across all specifications.
#' Points represent different methodological choices.
#'
#' @param spec_curve Specification curve dataset
#' @return ggplot object

plot_specification_curve <- function(spec_curve) {

  # Order specifications by MAE within each CPI type
  spec_curve <- spec_curve |>
    filter(!is.na(mae)) |>
    group_by(cpi_type) |>
    mutate(
      spec_id = paste(method, ifelse(is.na(strike_window), "", strike_window), sample_period, sep = "_"),
      spec_rank = rank(mae)
    ) |>
    ungroup()

  ggplot(spec_curve, aes(x = spec_rank, y = mae, color = method, shape = sample_period)) +
    geom_point(size = 3, alpha = 0.8) +
    facet_wrap(~cpi_type, scales = "free_y", ncol = 1) +
    labs(
      title = "Specification Curve: Forecast Accuracy Across Methods",
      subtitle = "Each point = one specification (method × window × period)",
      x = "Specification (ranked by MAE)",
      y = "Mean Absolute Error (pp)",
      color = "Method",
      shape = "Sample Period"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    ) +
    guides(
      color = guide_legend(nrow = 2),
      shape = guide_legend(nrow = 1)
    )
}
