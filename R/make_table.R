#' Create period summary table for article
#'
#' Aggregates panel data into three key periods showing volume and accuracy.
#' This replaces the volume and accuracy time series plots with a compact table.
#'
#' @param panel Full panel dataset with volume and accuracy
#' @return gt table object

make_period_summary_table <- function(panel) {

  # Define periods
  period_summary <- panel |>
    filter(!is.na(abs_error_clean)) |>
    mutate(
      period = case_when(
        cpi_month < as.Date("2023-06-01") ~ "Pre-dog days (2022–mid 2023)",
        cpi_month >= as.Date("2023-06-01") & cpi_month < as.Date("2025-01-01") ~ "Dog days (mid 2023–2024)",
        cpi_month >= as.Date("2025-01-01") ~ "Post-Robinhood (2025+)",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(period)) |>
    group_by(period) |>
    summarise(
      events = n(),
      avg_volume = mean(total_volume, na.rm = TRUE),
      median_volume = median(total_volume, na.rm = TRUE),
      min_volume = min(total_volume, na.rm = TRUE),
      max_volume = max(total_volume, na.rm = TRUE),
      avg_mae = mean(abs_error_clean, na.rm = TRUE),
      median_mae = median(abs_error_clean, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Order periods chronologically
      period = factor(period, levels = c(
        "Pre-dog days (2022–mid 2023)",
        "Dog days (mid 2023–2024)",
        "Post-Robinhood (2025+)"
      ))
    ) |>
    arrange(period) |>
    mutate(
      volume_range = paste0(
        scales::comma(min_volume, accuracy = 1),
        "–",
        scales::comma(max_volume, accuracy = 1)
      )
    ) |>
    select(period, events, avg_volume, volume_range, avg_mae)

  # Create gt table
  period_summary |>
    gt::gt() |>
    gt::tab_header(
      title = "Volume exploded 100x, accuracy barely moved",
      subtitle = "Summary statistics across three key periods"
    ) |>
    gt::cols_label(
      period = "Period",
      events = "Events",
      avg_volume = "Avg Volume",
      volume_range = "Volume Range",
      avg_mae = "Avg MAE"
    ) |>
    gt::fmt_number(
      columns = avg_volume,
      decimals = 0,
      use_seps = TRUE
    ) |>
    gt::fmt_number(
      columns = avg_mae,
      decimals = 3,
      pattern = "{x}pp"
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#f0f0f0"),
      locations = gt::cells_body(rows = 2)  # Highlight dog days row
    ) |>
    gt::tab_footnote(
      footnote = "MAE = Mean Absolute Error (percentage points). Lower is better.",
      locations = gt::cells_column_labels(columns = avg_mae)
    ) |>
    gt::opt_stylize(style = 6, color = "gray") |>
    gt::tab_options(
      table.font.size = 14,
      heading.title.font.size = 16,
      heading.subtitle.font.size = 12
    )
}


#' Save gt table as PNG
#'
#' @param gt_table A gt table object
#' @param filename Output file path
#' @return File path (for targets)

save_gt_table <- function(gt_table, filename) {
  gt::gtsave(gt_table, filename)
  filename
}
