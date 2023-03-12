#' Add the Relative Strength Index (RSI) to a data set of prices
#'
#' This function takes a data set of price data and adds a column containing the
#' RSI and a column containing a logical vector that is TRUE when the RSI is
#' below the specified cutoff and FALSE otherwise. It can be used to prepare
#' the data for using the [bot()] function with `start_asap = FALSE`.
#'
#' @param data A `data.table` containing `time` and `price` columns.
#' @param time_period A character specifying the time frame for calculating
#' the RSI. Possible values "3 minutes", "5 minutes", "15 minutes",
#' "30 minutes", "1 hour", "2 hours", "4 hours", "8 hours", "1 day". Default
#' is "1 hour".
#' @param n Number of periods for moving averages.
#' @param cutoff A cutoff value for creating a logical vector "deal_start". By
#' default, a cutoff value of 30 is used, which means that "deal_start" is TRUE
#' when the RSI is below 30 and FALSE otherwise.
#'
#' @return
#' A `data.table` with added RSI and logical vector `deal_start`.
#' @export
#'
#' @examples
#' #Download some price data
#' dat <- get_binance_prices_from_csv('PYRUSDT', start_time = '2023-02-02',
#'                                     progressbar = F)
#'
#' #Add the RSI
#' dat2 <- add_rsi(dat, time_period = "30 minutes", cutoff = 30)
#'
#' #Perform backtesting
#' backtest(data = dat2, start_asap = F)
add_rsi <- function(data, time_period = "1 hour", n = 7, cutoff = 30) {
  index <- c("3 minutes" = 180, "5 minutes" = 300, "15 minutes" = 900,
             "30 minutes" = 1800, "1 hour" = 3600, "2 hours" = 7200,
             "4 hours" = 14400, "8 hours" = 28800, "1 day" = 24 * 3600)
  interval <- index[time_period]
  data <- dtplyr::lazy_dt(data)
  data |>
    dplyr::group_by(x = as.integer(time) %/% interval) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(RSI = TTR::RSI(price, n = n)) |>
    dplyr::select(time, RSI) |>
    dplyr::left_join(x = data, by = "time") |>
    tidyr::fill(RSI, .direction = "up") |>
    dplyr::mutate(deal_start = RSI < cutoff) |>
    data.table::as.data.table()
}
