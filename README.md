# CPI Forecast Accuracy Analysis

Analysis of Kalshi CPI prediction market accuracy using the {targets} pipeline.

## Project Structure

```
cpi_analysis/
├── R/                      # Function definitions (sourced by targets)
│   ├── fetch_markets.R     # Kalshi API: fetch market metadata
│   ├── fetch_trades.R      # Kalshi API: fetch trade logs
│   ├── fetch_cpi.R         # BLS API: fetch actual CPI data
│   ├── parse_markets.R     # Parse ticker format, extract strike values
│   ├── compute_*.R         # Analysis functions (terminal prices, forecasts, etc.)
│   └── plot_*.R            # Visualization functions
│
├── _targets.R              # Pipeline orchestration (run with tar_make())
│
├── report.Rmd              # Full analysis report (HTML)
├── handoff.Rmd             # Handoff document for collaborators (HTML + MD)
│
├── figures/                # Generated plots (from pipeline)
├── data/                   # Generated CSV data (from pipeline)
├── report_outputs/         # Report HTML outputs
└── handoff_outputs/        # Handoff package: docs, plots, and data
    ├── handoff.html        # Full handoff document (HTML)
    ├── HANDOFF.md          # Handoff document (Markdown)
    ├── cpi_forecast_panel.csv
    └── figures/            # Key plots for handoff
        ├── volume_over_time.png
        ├── accuracy_over_time.png
        └── volume_vs_accuracy.png
```

## Quick Start

1. **Install dependencies:**
   ```r
   install.packages(c("targets", "tarchetypes", "tidyverse", "httr2",
                      "jsonlite", "blscrapeR", "scales", "patchwork",
                      "knitr", "rmarkdown"))
   ```

2. **Set Kalshi API key:**
   Create `.Renviron` file with:
   ```
   KALSHI_API_KEY=your_key_here
   ```

3. **Run the pipeline:**
   ```r
   library(targets)
   tar_make()
   ```

4. **View outputs:**
   - Full report: `report_outputs/report.html`
   - Handoff package: `handoff_outputs/` (all files needed for blog post)
   - Individual plots: `figures/`
   - Panel dataset: `data/cpi_forecast_panel.csv`

## Pipeline Phases

1. **Markets Discovery** – Fetch all CPI markets from Kalshi API
2. **Volume Analysis** – Compute trading volume time series
3. **Trade Logs** – Pull trade-by-trade data (optimized: closing day only)
4. **Forecast Extraction** – Fit probability distributions from terminal prices
5. **Actual CPI Data** – Fetch realized CPI values from BLS
6. **Panel Dataset** – Join forecasts + actuals + volume
7. **Visualizations** – Generate all plots
8. **Reports** – Render R Markdown documents

## Handoff Package

The `handoff_outputs/` folder contains everything needed to write about this analysis:
- **HANDOFF.md** – Markdown document with data inventory, key findings, quotable stats
- **handoff.html** – Same content in HTML format
- **figures/** – Three core plots (volume over time, accuracy over time, volume vs accuracy)
- **cpi_forecast_panel.csv** – Full dataset for custom analysis

This folder can be shared directly with collaborators or blog post writers.

## Notes

- All outputs are reproducible via `tar_make()`
- The pipeline is optimized to fetch only closing-day trades (reduces API calls by ~90%)
- Forecasts use two methods: "full" (all strikes) and "clean" (only strikes near actual outcome)
