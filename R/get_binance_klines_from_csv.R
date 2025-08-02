#' Get kline/candlestick data from Binance by downloading and reading csv files
#'
#' This function downloads and reads csv files containing kline/candlestick data
#' from \url{https://data.binance.vision/} and merges the data to a data table.
#'
#' @param symbol a valid binance symbol of a trading pair
#' @param start_time,end_time an object of class `date` or `POSIXct`
#' or `POSIXt` or a character string that can be converted to these classes
#' @param interval a character string specifying the time frame of the candles.
#' Possible values are `"1s"`, `"1m"`, `"3m"`, `"5m"`, `"30m"`, `"1h"`, `"2h"`,
#' `"4h"`, `"6h"` and `"8h"`.
#' @param include_volume_data whether to include volume data or not. Default is
#' FALSE to save memory.
#'
#' @return a `data.table` with open-high-low-close values
#' @seealso
#'  * [get_binance_prices_from_csv()] for dowloading aggTrade data via csv files
#'  * [get_binance_klines()] for dowloading candlestick data directly
#' @export
#'
#' @examples
#' get_binance_klines_from_csv('BTCUSDT', start_time = '2023-01-02',
#'                              end_time = '2023-01-03', '1s')
get_binance_klines_from_csv <- function(symbol = "BTCUSDT",
          start_time = Sys.time() - lubridate::dmonths(2),
          end_time = Sys.time() - lubridate::ddays(2), interval = "1m",
          include_volume_data = F) {
  tz <- Sys.timezone()
  if (is.character(start_time)) {
    start_time <- lubridate::as_datetime(start_time, tz = tz)
  }
  if (is.character(end_time)) {
    end_time <- lubridate::as_datetime(end_time, tz = tz) +
      lubridate::ddays(1) - lubridate::dmilliseconds(1)
  }
  url <- "https://data.binance.vision/data/spot/monthly/klines/" |>
    paste0(symbol, "/", interval, "/", symbol, "-", interval, "-XX.zip")
  dates <- seq(as.Date(start_time), as.Date(end_time), by = "day")
  months <- head(unique(format(dates, "%Y-%m")), -1)
  days <- seq(as.Date(format(as.Date(end_time), "%Y-%m-01")),
              as.Date(end_time), by = "day")
  tfs <- replicate(length(c(months, days)), tempfile(fileext = ".zip"))
  urls_months <- sapply(months, \(x) sub("XX", x, url))
  urls_days <- sapply(days, \(x) sub("XX", x, sub("monthly", "daily", url)))
  urls <- unlist(c(urls_months, urls_days))
  options(timeout = 3600)
  download.file(urls, tfs, method = "libcurl", quiet = T)
  dt <- lapply(tfs, readr::read_csv,
               col_types = "ndddddndiddi",
               col_names = c("open_time", "open", "high", "low", "close",
                             "volume", "close_time", "quote_asset_volume",
                             "trades", "taker_buy_base_asset_volume",
                             "taker_buy_quote_asset_volume", "symbol"),
               col_select = if (include_volume_data) 1:11 else c(1:5, 7)) |>
    data.table::rbindlist()
  fun <- \(x) lubridate::as_datetime(x %/% 1e3,  tz = "UTC")
  dt[, `:=`(open_time = fun(open_time), close_time = fun(close_time))]
  dt[open_time >= start_time & close_time <= end_time]
}

