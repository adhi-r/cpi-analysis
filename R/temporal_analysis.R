#' Temporal Stability Analysis Functions
#'
#' Functions for testing whether forecast accuracy has changed over time.
#' Structural breaks could indicate changes in market liquidity, participant
#' sophistication, or macroeconomic regime.

#' Split panel into pre/post 2024 subsamples
#'
#' @param panel Full panel dataset
#' @return List with pre_2024 and post_2024 tibbles

split_temporal <- function(panel) {
  list(
    pre_2024 = panel |> filter(cpi_month < as.Date("2024-01-01")),
    post_2024 = panel |> filter(cpi_month >= as.Date("2024-01-01"))
  )
}


#' Test for structural break in forecast accuracy
#'
#' Uses simple t-test to compare mean absolute error before vs. after 2024.
#' A significant difference would suggest forecast quality has changed.
#'
#' @param panel Full panel dataset
#' @return Tibble with test statistics by cpi_type

test_structural_break <- function(panel) {

  panel |>
    mutate(post_2024 = cpi_month >= as.Date("2024-01-01")) |>
    filter(!is.na(abs_error_clean)) |>  # Use clean forecasts
    group_by(cpi_type) |>
    summarise(
      n_pre = sum(!post_2024, na.rm = TRUE),
      n_post = sum(post_2024, na.rm = TRUE),
      mae_pre = mean(abs_error_clean[!post_2024], na.rm = TRUE),
      mae_post = mean(abs_error_clean[post_2024], na.rm = TRUE),
      mae_diff = mae_post - mae_pre,

      # t-test for difference in means
      # Returns NA if insufficient data in either period
      t_result = if (n_pre >= 2 && n_post >= 2) {
        list(t.test(
          abs_error_clean[post_2024],
          abs_error_clean[!post_2024]
        ))
      } else {
        list(NULL)
      },

      t_stat = if (!is.null(t_result[[1]])) t_result[[1]]$statistic else NA_real_,
      p_value = if (!is.null(t_result[[1]])) t_result[[1]]$p.value else NA_real_,

      .groups = "drop"
    ) |>
    select(-t_result)
}
