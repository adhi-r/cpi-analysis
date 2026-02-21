#' Fetch actual CPI data from BLS using blscrapeR
#'
#' Uses the {blscrapeR} package to get data directly from BLS.
#' No API key required.
#'
#' BLS series codes:
#' - CUSR0000SA0: CPI-U All Items, Seasonally Adjusted (headline)
#' - CUSR0000SA0L1E: CPI-U Less Food & Energy, Seasonally Adjusted (core)
#'
#' CRITICAL: Verify that Kalshi's CPI markets use seasonally adjusted data
#' and match the BLS rounding conventions (typically 1 decimal for MoM).

fetch_actual_cpi <- function() {

  # CPI-U All Items, Seasonally Adjusted (headline)
  headline <- blscrapeR::bls_api(
    seriesid = "CUSR0000SA0",
    startyear = 2021,  # need 12 months before first market
    endyear = year(Sys.Date())
  ) |>
    mutate(
      date = as.Date(paste(year, periodName, "01"), format = "%Y %B %d"),
      cpi_level = as.numeric(value)
    ) |>
    arrange(date) |>
    mutate(
      actual_mom = (cpi_level / lag(cpi_level) - 1) * 100,
      actual_yoy = (cpi_level / lag(cpi_level, 12) - 1) * 100
    )

  # Core CPI (Less Food & Energy), Seasonally Adjusted
  core <- blscrapeR::bls_api(
    seriesid = "CUSR0000SA0L1E",
    startyear = 2021,
    endyear = year(Sys.Date())
  ) |>
    mutate(
      date = as.Date(paste(year, periodName, "01"), format = "%Y %B %d"),
      core_level = as.numeric(value)
    ) |>
    arrange(date) |>
    mutate(
      actual_core_mom = (core_level / lag(core_level) - 1) * 100,
      actual_core_yoy = (core_level / lag(core_level, 12) - 1) * 100
    )

  # Join and reshape - keep index values for precise calculations
  headline |>
    left_join(
      core |> select(date, core_level, actual_core_mom, actual_core_yoy),
      by = "date"
    ) |>
    transmute(
      cpi_month = date,
      # Keep index values for precise forecast calculations
      headline_index = cpi_level,
      core_index = core_level,
      # Precise actual rates (NOT rounded to BLS published values)
      actual_mom = actual_mom,
      actual_yoy = actual_yoy,
      actual_core_mom = actual_core_mom,
      actual_core_yoy = actual_core_yoy
    ) |>
    filter(cpi_month >= "2022-05-01")
}
