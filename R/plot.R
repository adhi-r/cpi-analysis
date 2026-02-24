#' Volume over time — bar plot with linear scale, faceted by type

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
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c(
      "Headline MoM" = "#1b9e77",
      "Headline YoY" = "#d95f02",
      "Core MoM" = "#7570b3",
      "Core YoY" = "#e7298a"
    )) +
    labs(
      title = "Robinhood integration caused an 816-fold volume spike",
      subtitle = "Total contracts traded per CPI event, by market type",
      x = NULL, y = "Contracts traded"
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
      title = "Forecast accuracy stayed stable despite massive volume swings",
      subtitle = "Absolute error between market forecast and actual CPI print, by market type",
      x = NULL, y = "Absolute error (pp)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing.y = unit(1, "lines")
    )
}

#' THE MONEY CHART: volume vs accuracy scatter (log-log)

make_scatter_plot <- function(panel) {
  panel |>
    filter(!is.na(abs_error_clean), total_volume > 0) |>
    ggplot(aes(x = total_volume, y = abs_error_clean)) +
    geom_point(size = 3, alpha = 0.6, color = "#2c7fb8") +
    geom_smooth(method = "lm", se = TRUE, color = "#253494", fill = "#2c7fb8", alpha = 0.2) +
    scale_x_log10(labels = scales::comma) +
    scale_y_log10(labels = function(x) paste0(x, "pp")) +
    labs(
      title = "Trading volume doesn't predict forecast accuracy",
      subtitle = "Each dot represents one CPI event across all market types (log-log scale)",
      x = "Total volume (log scale)", y = "Absolute error (log scale)"
    ) +
    theme_minimal(base_size = 14)
}


#' Learning curve: accuracy improves over time as forecasters gain experience

make_learning_curve_plot <- function(panel) {
  panel |>
    filter(!is.na(abs_error_clean), total_volume > 0) |>
    mutate(
      period = case_when(
        cpi_month < as.Date("2023-06-01") ~ "Pre-dog days",
        cpi_month >= as.Date("2023-06-01") & cpi_month < as.Date("2025-01-01") ~ "Dog days",
        cpi_month >= as.Date("2025-01-01") ~ "Post-Robinhood"
      ),
      period = factor(period, levels = c("Pre-dog days", "Dog days", "Post-Robinhood"))
    ) |>
    ggplot(aes(x = cpi_month, y = abs_error_clean)) +
    geom_point(aes(color = period, size = total_volume), alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "#253494", fill = "#2c7fb8", alpha = 0.2, linewidth = 1.2) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
    scale_y_continuous(labels = function(x) paste0(x, "pp")) +
    scale_size_continuous(
      range = c(1.5, 15),
      labels = scales::comma,
      breaks = c(1e2, 1e4, 1e6)
    ) +
    scale_color_manual(values = c(
      "Pre-dog days" = "#E74C3C",
      "Dog days" = "#F39C12",
      "Post-Robinhood" = "#27AE60"
    )) +
    labs(
      title = "Forecasters got better with practice, even as volume ebbed and flowed",
      subtitle = "Absolute CPI forecast error over time (lower is better). Dot size shows trading volume: tiny during\nthe low-volume dog days, massive post-Robinhood.",
      x = NULL,
      y = "Absolute error (pp)",
      color = "Period",
      size = "Volume",
      caption = "Source: @eightyhi analysis of Kalshi trade data. See footnote for methodology."
    ) +
    guides(
      size = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        order = 2
      ),
      color = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        order = 1,
        override.aes = list(size = 4)
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.spacing.x = unit(2, "cm"),
      panel.grid.minor = element_blank()
    )
}
