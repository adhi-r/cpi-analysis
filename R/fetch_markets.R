#' Fetch all CPI-related markets from Kalshi API
#'
#' Tries multiple known series tickers and deduplicates.
#' The Kalshi API requires no authentication for read-only market data.
#'
#' @return A tibble with one row per strike market

fetch_all_cpi_markets <- function() {

  base_url <- "https://api.elections.kalshi.com/trade-api/v2"

  # Known CPI series tickers — old and new naming conventions
  # IMPORTANT: inspect results and add any we missed
  series_candidates <- c(
    "KXCPI", "KXCPIYOY", "KXCPICOREYOY", "KXCPICORE",
    "CPI", "CPIYOY",
    "INFL"  # possible older ticker
  )

  fetch_series <- function(series_ticker) {
    cli::cli_inform("Fetching series: {series_ticker}")
    all_markets <- list()
    cursor <- NULL

    repeat {
      params <- list(series_ticker = series_ticker, limit = 1000)
      if (!is.null(cursor)) params$cursor <- cursor

      resp <- httr2::request(base_url) |>
        httr2::req_url_path_append("markets") |>
        httr2::req_url_query(!!!params) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      markets <- resp$markets
      if (length(markets) == 0) break

      all_markets <- c(all_markets, markets)

      cursor <- resp$cursor
      if (is.null(cursor) || cursor == "") break

      Sys.sleep(0.2)
    }

    cli::cli_inform("  Found {length(all_markets)} markets")
    all_markets
  }

  # Fetch all series, flatten, deduplicate
  raw <- series_candidates |>
    map(possibly(fetch_series, otherwise = list())) |>
    list_flatten()

  # Convert to tibble
  # The exact field names depend on the API response.
  # IMPORTANT: print str(raw[[1]]) to see actual field names,
  # then adjust this conversion accordingly.
  markets_df <- raw |>
    map_dfr(function(m) {
      tibble(
        ticker         = m$ticker %||% NA_character_,
        event_ticker   = m$event_ticker %||% NA_character_,
        series_ticker  = m$series_ticker %||% NA_character_,
        title          = m$title %||% NA_character_,
        subtitle       = m$subtitle %||% NA_character_,
        status         = m$status %||% NA_character_,
        volume         = m$volume %||% NA_integer_,
        yes_price      = m$yes_price %||% NA_integer_,
        result         = m$result %||% NA_character_,
        open_time      = m$open_time %||% NA_character_,
        close_time     = m$close_time %||% NA_character_,
        floor_strike   = m$floor_strike %||% NA_real_,
        cap_strike     = m$cap_strike %||% NA_real_
      )
    })

  markets_df |>
    distinct(ticker, .keep_all = TRUE)
}
