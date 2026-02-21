library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("tidyverse", "httr2", "jsonlite", "blscrapeR", "scales", "patchwork", "knitr", "rmarkdown")
)

# Source all R/ files
tar_source()

list(
  # ──────────────────────────────────────────────
  # PHASE 1: Discover all CPI markets on Kalshi
  # ──────────────────────────────────────────────

  tar_target(
    raw_markets,
    fetch_all_cpi_markets()
    # Returns a tibble: one row per strike market
    # Columns: ticker, event_ticker, series_ticker, title, subtitle,
    #          status, volume, yes_price, result, open_time, close_time,
    #          floor_strike, cap_strike, ...
  ),

  tar_target(
    markets,
    raw_markets |> parse_and_clean_markets()
    # Adds parsed columns: series, date_code, cpi_month (Date),
    #   strike_value, cpi_type (headline_mom, headline_yoy, core_yoy)
    # Filters to settled markets only
  ),

  # ──────────────────────────────────────────────
  # PHASE 2: Volume time series (fast — uses market metadata only)
  # ──────────────────────────────────────────────

  tar_target(
    volume_ts,
    markets |> compute_volume_timeseries()
    # Returns tibble: cpi_type, cpi_month, total_volume, num_strikes
  ),

  # ──────────────────────────────────────────────
  # PHASE 3: Pull trade logs (OPTIMIZED — only closing day trades)
  # ──────────────────────────────────────────────

  tar_target(
    market_metadata,
    markets |> select(ticker, close_time)
    # Ticker + close_time pairs for filtering trades to closing day only
  ),

  # Dynamic branching: one branch per ticker
  # OPTIMIZATION: Only fetches trades from the day each market closed
  # This reduces API calls and data volume by ~90%
  tar_target(
    raw_trades,
    fetch_trades_for_ticker(market_metadata$ticker, market_metadata$close_time),
    pattern = map(market_metadata),
    # Each branch returns a tibble of trades for one ticker (closing day only)
    # Columns: trade_id, ticker, yes_price, count, created_time, taker_side
    iteration = "list"
  ),

  tar_target(
    trades,
    bind_rows(raw_trades) |>
      left_join(markets |> select(ticker, event_ticker, series, cpi_month, strike_value, cpi_type),
                by = "ticker")
  ),

  # ──────────────────────────────────────────────
  # PHASE 4: Extract terminal prices and fit distributions
  # ──────────────────────────────────────────────

  tar_target(
    terminal_prices,
    trades |> compute_terminal_prices()
    # For each strike market: last traded yes_price before close
    # Returns tibble: event_ticker, cpi_type, cpi_month, strike_value, terminal_prob
  ),

  tar_target(
    forecasts,
    terminal_prices |> fit_all_forecasts()
    # For each event: fit N(mu, sigma) to the implied CDF
    # Returns tibble: event_ticker, cpi_type, cpi_month,
    #   forecast_mean, forecast_sigma, num_informative_strikes, fit_quality (R²)
  ),

  # ──────────────────────────────────────────────
  # PHASE 5: Pull actual CPI data from BLS
  # ──────────────────────────────────────────────

  tar_target(
    actuals,
    fetch_actual_cpi()
    # Returns tibble: cpi_month, actual_mom, actual_yoy, actual_core_mom, actual_core_yoy
    # All in percent, multiple decimal places
  ),

  # Clean forecasts: refit using only strikes near actual outcome
  tar_target(
    forecasts_clean,
    fit_clean_forecasts(terminal_prices, actuals)
    # Refit using only 2 strikes below and 2 above actual
    # Returns: event_ticker, cpi_type, cpi_month,
    #   forecast_mean_clean, forecast_sigma_clean, r_squared_clean, n_informative_clean
  ),

  # ──────────────────────────────────────────────
  # PHASE 6: Join into panel dataset
  # ──────────────────────────────────────────────

  tar_target(
    panel,
    build_panel(forecasts, actuals, volume_ts) |>
      # Add clean forecasts for comparison
      left_join(forecasts_clean, by = c("event_ticker", "cpi_type", "cpi_month")) |>
      mutate(
        # Compute errors for clean forecasts too
        error_clean = forecast_mean_clean - actual_print,
        abs_error_clean = abs(error_clean),
        z_score_clean = (actual_print - forecast_mean_clean) / forecast_sigma_clean
      )
    # Returns tibble: cpi_type, cpi_month, total_volume, num_strikes,
    #   forecast_mean, forecast_sigma, actual_print, error, abs_error, z_score
    #   PLUS: forecast_mean_clean, forecast_sigma_clean, error_clean, abs_error_clean, z_score_clean
  ),

  tar_target(
    panel_csv,
    {
      dir.create("data", showWarnings = FALSE, recursive = TRUE)
      write_csv(panel, "data/cpi_forecast_panel.csv")
      "data/cpi_forecast_panel.csv"
    },
    format = "file"
  ),

  # ──────────────────────────────────────────────
  # PHASE 7: Charts
  # ──────────────────────────────────────────────

  tar_target(
    plot_volume,
    make_volume_plot(volume_ts)
    # Line chart: volume over time, one line per cpi_type
  ),

  tar_target(
    plot_accuracy,
    make_accuracy_plot(panel)
    # Line chart: abs_error over time, possibly with volume on secondary axis
  ),

  tar_target(
    plot_scatter,
    make_scatter_plot(panel)
    # THE MONEY CHART: volume (x) vs abs_error (y), one dot per event-month
  ),

  tar_target(
    save_plots,
    {
      dir.create("figures", showWarnings = FALSE, recursive = TRUE)
      ggsave("figures/volume_over_time.png", plot_volume, width = 10, height = 6)
      ggsave("figures/accuracy_over_time.png", plot_accuracy, width = 10, height = 6)
      ggsave("figures/volume_vs_accuracy.png", plot_scatter, width = 8, height = 6)
      "figures/"
    },
    format = "file"
  ),

  # ──────────────────────────────────────────────
  # PHASE 8: R Markdown Reports
  # ──────────────────────────────────────────────

  tar_render(
    report,
    "report.Rmd",
    output_dir = "report_outputs"
  ),

  tar_render(
    handoff,
    "handoff.Rmd",
    output_dir = "handoff_outputs"
  ),

  # ──────────────────────────────────────────────
  # PHASE 9: Handoff Package (plots + documents)
  # ──────────────────────────────────────────────

  tar_target(
    handoff_package,
    {
      # Force dependencies on prior targets
      stopifnot(file.exists(save_plots))
      stopifnot(file.exists(panel_csv))

      # Create handoff directory
      dir.create("handoff_outputs/figures", showWarnings = FALSE, recursive = TRUE)

      # Copy key plots to handoff folder
      file.copy("figures/volume_over_time.png", "handoff_outputs/figures/volume_over_time.png", overwrite = TRUE)
      file.copy("figures/accuracy_over_time.png", "handoff_outputs/figures/accuracy_over_time.png", overwrite = TRUE)
      file.copy("figures/volume_vs_accuracy.png", "handoff_outputs/figures/volume_vs_accuracy.png", overwrite = TRUE)

      # Copy panel CSV for reference
      file.copy("data/cpi_forecast_panel.csv", "handoff_outputs/cpi_forecast_panel.csv", overwrite = TRUE)

      c("handoff_outputs/figures/volume_over_time.png",
        "handoff_outputs/figures/accuracy_over_time.png",
        "handoff_outputs/figures/volume_vs_accuracy.png",
        "handoff_outputs/cpi_forecast_panel.csv")
    },
    format = "file"
  )
)
