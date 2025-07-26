#' Get kline/candlestick data of muliple coins from Binance by downloading and
#' reading csv files
#'
#' This function downloads and reads csv files containing kline/candlestick data
#' of multiple coins from \url{https://data.binance.vision/} and merges the
#' data to a data table. To save memory, only the the columns open time,
#' open price, high price and low price are returned.
#'
#' @param coins a vector of valid coin tickers
#' @param quote_currency the quote currency
#' @param start_time,end_time an object of class `date` or `POSIXct`
#' or `POSIXt` or a character string that can be converted to these classes
#' @param interval a character string specifying the time frame of the candles.
#' Possible values are `"1s"`, `"1m"`, `"3m"`, `"5m"`, `"30m"`, `"1h"`, `"2h"`,
#' `"4h"`, `"6h"` and `"8h"`.
#' @param include_volume_data whether to include volume data or not. Default is
#' FALSE to save memory.
#' @param quietly whether to suppress progess messages
#'
#' @return a `data.table` with open-high-low values
#' @seealso
#'  * [get_binance_klines_from_csv()] for dowloading candlestick data of a
#'  single trading pair via csv files
#'  * [get_binance_prices_from_csv()] for dowloading aggTrade data via csv files
#'  * [get_binance_klines()] for dowloading candlestick data directly
#' @export
#'
#' @examples
#' get_binance_klines_from_csv_multi(coins = c("MATIC", "SOL", "AVAX"),
#'                                   start_time = '2023-01-02',
#'                                   end_time = '2023-01-03',
#'                                   interval = '1s')
get_binance_klines_from_csv_multi <- function(coins = c("BTC", "ETH", "BNB"),
                                         quote_currency = "USDT",
          start_time = Sys.time() - lubridate::dmonths(2),
          end_time = Sys.time() - lubridate::ddays(2), interval = "1m",
          include_volume_data = F, quietly = F) {
  tz <- Sys.timezone()
  if (is.character(start_time)) {
    start_time <- lubridate::as_datetime(start_time, tz = tz)
  }
  if (is.character(end_time)) {
    end_time <- lubridate::as_datetime(end_time, tz = tz) +
      lubridate::ddays(1) - lubridate::dmilliseconds(1)
  }
  dates <- seq(as.Date(start_time), as.Date(end_time), by = "day")
  months <- head(unique(format(dates, "%Y-%m")), -1)
  days <- seq(as.Date(format(as.Date(end_time), "%Y-%m-01")),
              as.Date(end_time), by = "day")
  download <- function(coin, quote = quote_currency) {
    symbol <- paste0(coin, quote)
    if (!quietly) cat("downloading", symbol, '\n')
    url <- "https://data.binance.vision/data/spot/monthly/klines/" |>
      paste0(symbol, "/", interval, "/", symbol, "-", interval, "-XX.zip")
    tfs <- replicate(length(c(months, days)), tempfile(fileext = ".zip"))
    urls_months <- sapply(months, \(x) sub("XX", x, url))
    urls_days <- sapply(days, \(x) sub("XX", x, sub("monthly", "daily", url)))
    urls <- unlist(c(urls_months, urls_days))
    options(timeout = 3600)
    download.file(urls, tfs, method = "libcurl", quiet = T)
    dt <- lapply(tfs, readr::read_csv,
                 col_types = "ndddddndiddi",
                 col_names = c("open_time", "open", "high", "low",
                               "close", "volume", "close_time",
                               "quote_asset_volume", "trades",
                               "taker_buy_base_asset_volume",
                               "taker_buy_quote_asset_volume", "symbol"),
                 col_select = 1:4) |>
      data.table::rbindlist()
    lapply(tfs, file.remove)
    fun <- \(x) lubridate::as_datetime(x %/% 1000,  tz = Sys.timezone())
    dt[, `:=`(open_time = fun(open_time))]
    dt <- dt[open_time >= start_time & start_time <= end_time]
    if (interval == '1s') {
      all_times <- seq(from = start_time, to = end_time, by = "sec")
      dt <- dt[J(all_times), on = .(open_time)]
      data.table::setnafill(dt, 'nocb', cols = 2)
      dt[, `:=` (high = data.table::fifelse(is.na(high), open, high))]
      dt[, `:=` (low = data.table::fifelse(is.na(low), open, low))]
    }
    data.table::setnames(dt, old = 2:4, new = paste0(names(dt)[2:4], '_', coin))
  }
  dt <- download(coins[1])
  for (coin in coins[-1]) {
    dt <- cbind(dt, download(coin)[,-1])
  }
  dt
}

