#' Get kline/candlestick data from Binance
#'
#' This function is a wrapper for the [binance_klines()]
#' function in the [`binancer`](https://daroczig.github.io/binancer/) package.
#' It improves [binance_klines()] by allowing to import more than 1000 candles
#' at once.
#'
#' @param symbol a valid binance symbol of a trading pair
#' @param start_time,end_time an object of class `date` or `POSIXct`
#' or `POSIXt` or a character string that can be converted to these classes
#' @param interval a character string defining the time frame of the candles.
#' @param include_volume_data whether to include volume data or not. Default is
#' FALSE to save memory.
#'
#'
#' @return a `data.table` with open-high-low-close values
#' @seealso
#'  * [get_binance_klines_from_csv()] for dowloading candlestick via csv files
#'  * [get_binance_prices_from_csv()] for dowloading aggTrade data via csv files
#' @export
#'
#' @examples
#' get_binance_klines("MATICUSDT", start_time = "2022-01-01",
#'                     end_time = "2022-12-31", interval = "2h")
get_binance_klines <- function(symbol = "BTCUSDT",
                               start_time = Sys.time() - lubridate::ddays(3),
                               end_time = Sys.time(),
                               interval = c("1m", "3m", "5m", "15m", "30m",
                                            "1h", "2h", "4h", "6h", "8h", "12h",
                                            "1d", "3d", "1w", "1M"),
                               include_volume_data = F) {
  l <- vector("list", 10000)
  i <- 1
  j <- if (include_volume_data) 1:11 else c(1:5, 7)
  while (start_time <= end_time) {
    l[[i]] <- binancer::binance_klines(symbol = symbol, interval = interval,
                                limit = 1000, start_time = start_time)[, ..j]
    start_time <- tail(l[[i]]$close_time, 1)
    i <- i + 1
  }
  data.table::rbindlist(l)[open_time <= end_time]
}
