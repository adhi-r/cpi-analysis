#' Fetch all trades for a single market ticker
#'
#' Note: Kalshi API doesn't support time filtering, so we fetch all trades
#' and extract the terminal price in post-processing.
#'
#' @param ticker Character, a single strike market ticker
#' @param close_time Character, ignored (kept for compatibility)
#' @return A tibble of trades

fetch_trades_for_ticker <- function(ticker, close_time = NULL) {

  base_url <- "https://api.elections.kalshi.com/trade-api/v2"

  all_trades <- list()
  cursor <- NULL

  repeat {
    params <- list(ticker = ticker, limit = 1000)
    if (!is.null(cursor)) params$cursor <- cursor

    resp <- httr2::request(base_url) |>
      httr2::req_url_path_append("markets", "trades") |>
      httr2::req_url_query(!!!params) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200) {
      cli::cli_warn("Failed to fetch trades for {ticker}: HTTP {httr2::resp_status(resp)}")
      break
    }

    body <- httr2::resp_body_json(resp)
    trades <- body$trades
    if (length(trades) == 0) break

    all_trades <- c(all_trades, trades)

    cursor <- body$cursor
    if (is.null(cursor) || cursor == "") break

    Sys.sleep(0.2)
  }

  if (length(all_trades) == 0) {
    return(tibble(
      trade_id = character(),
      ticker = character(),
      yes_price = integer(),
      no_price = integer(),
      count = integer(),
      created_time = character(),
      taker_side = character()
    ))
  }

  all_trades |>
    map_dfr(function(t) {
      tibble(
        trade_id     = t$trade_id %||% NA_character_,
        ticker       = t$ticker %||% NA_character_,
        yes_price    = t$yes_price %||% NA_integer_,
        no_price     = t$no_price %||% NA_integer_,
        count        = t$count %||% NA_integer_,
        created_time = t$created_time %||% NA_character_,
        taker_side   = t$taker_side %||% NA_character_
      )
    })
}
