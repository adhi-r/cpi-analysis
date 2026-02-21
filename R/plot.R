#' Volume over time — bar plot with log scale, faceted by type

make_volume_plot <- function(volume_ts) {
  volume_ts |>
    mutate(
      cpi_label = case_match(
        cpi_type,
        "headline_mom" ~ "Headline MoM",
        "headline_yoy" ~ "Headline YoY",
        "core_mom"     ~ "Core MoM",
        "core_yoy"     ~ "Core YoY"
      )
    ) |>
    ggplot(aes(x = cpi_month, y = total_volume, fill = cpi_label)) +
    geom_col() +
    facet_wrap(~cpi_label, ncol = 1, scales = "free_y") +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%y") +
    scale_y_log10(labels = scales::comma) +
    scale_fill_manual(values = c(
      "Headline MoM" = "#1b9e77",
      "Headline YoY" = "#d95f02",
      "Core MoM" = "#7570b3",
      "Core YoY" = "#e7298a"
    )) +
    labs(
      title = "Kalshi CPI Market Volume Over Time",
      subtitle = "Total contracts traded per monthly event (log scale)",
      x = NULL, y = "Contracts traded (log scale)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing.y = unit(1, "lines")
    )
}

#' Accuracy over time — faceted by type

make_accuracy_plot <- function(panel) {
  panel |>
    filter(!is.na(abs_error_clean)) |>
    mutate(
      cpi_label = case_match(
        cpi_type,
        "headline_mom" ~ "Headline MoM",
        "headline_yoy" ~ "Headline YoY",
        "core_mom"     ~ "Core MoM",
        "core_yoy"     ~ "Core YoY"
      )
    ) |>
    ggplot(aes(x = cpi_month, y = abs_error_clean, fill = cpi_label)) +
    geom_col() +
    facet_wrap(~cpi_label, ncol = 1, scales = "free_y") +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%y") +
    scale_fill_manual(values = c(
      "Headline MoM" = "#1b9e77",
      "Headline YoY" = "#d95f02",
      "Core MoM" = "#7570b3",
      "Core YoY" = "#e7298a"
    )) +
    labs(
      title = "CPI Forecast Accuracy Over Time",
      subtitle = "|Market forecast − actual print| (clean: 2 strikes each side)",
      x = NULL, y = "Absolute error (pp)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing.y = unit(1, "lines")
    )
}

#' THE MONEY CHART: volume vs accuracy scatter

make_scatter_plot <- function(panel) {
  panel |>
    filter(!is.na(abs_error_clean), total_volume > 0) |>
    ggplot(aes(x = total_volume, y = abs_error_clean)) +
    geom_point(size = 3, alpha = 0.6, color = "#2c7fb8") +
    geom_smooth(method = "lm", se = TRUE, color = "#253494", fill = "#2c7fb8", alpha = 0.2) +
    scale_x_log10(labels = scales::comma) +
    labs(
      title = "Volume vs. Forecast Accuracy",
      subtitle = "Each dot is one CPI event (all types combined)",
      x = "Total volume (log scale)", y = "Absolute error (pp)"
    ) +
    theme_minimal(base_size = 14)
}
