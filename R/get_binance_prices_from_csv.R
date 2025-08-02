#' Get price data from Binance via csv files
#'
#' This function downloads and reads csv files containing aggTrades data
#' from \url{https://data.binance.vision/} and merges the data to a data table.
#'
#' @param symbol a valid binance symbol of a trading pair
#' @param start_time,end_time an object of class `date` or `POSIXct`
#' or `POSIXt` or a character string that can be converted to these classes
#' @param spot whether to download spot of futures trading data
#' @param interval the interval for aggregating the data. "second" (default)
#' corresponds to 1 second, "minute" to 1 minute etc.
#' @param aggregated whether to aggregate the price data or not. If TRUE a
#' columns with the highest and lowest prices for the chosen time interval is
#' added.
#' @param processing a character string specifying the processing of the job.
#' Default is `"multisession"` which corresponds to parallel processing
#' using separate R sessions running in the background on the same machine.
#' If you don't want to use parallel processing, set this argument to
#' `"sequential"`.
#' @param ncores the number of CPU cores used in parallel processing.
#' @param progressbar whether to show a progressbar or not
#'
#' @return a `data.table` with with time and price columns
#' @seealso
#'  * [get_binance_klines_from_csv()] for dowloading candlestick data via csv files
#'  * [get_binance_klines()] for dowloading candlestick data directly
#' @export
#'
#' @examples
#' get_binance_prices_from_csv("PYRUSDT", start_time = "2023-02-02", progressbar = F)
get_binance_prices_from_csv <- function(symbol = "PYRUSDT",
              start_time = Sys.time() - lubridate::dmonths(2),
              end_time = Sys.time() - lubridate::ddays(2),
              spot = TRUE, interval = "second", aggregated = FALSE,
              processing = "multisession", ncores = parallel::detectCores(),
              progressbar = TRUE) {
  tz <- "UTC"
  if (is.character(start_time)) {
    start_time <- lubridate::as_datetime(start_time, tz = tz)
  }
  if (is.character(end_time)) {
    end_time <- lubridate::as_datetime(end_time, tz = tz) +
      lubridate::ddays(1) - lubridate::dmilliseconds(1)
  }
  x <- ifelse(spot, "spot", "futures/um")
  url <- "https://data.binance.vision/data/" |>
    paste0(x, "/monthly/aggTrades/", symbol, "/", symbol, "-aggTrades-XX.zip")
  dates <- seq(as.Date(start_time), as.Date(end_time), by = "day")
  m <- unique(format(dates, "%Y-%m"))
  months <- head(m, -1)
  days <- grep(tail(m, 1), dates, value = T)
  urls_months <- sapply(months, \(x) sub("XX", x, url))
  urls_days <- sapply(days, \(x) sub("XX", x, sub("monthly", "daily", url)))
  urls <- unlist(c(urls_months, urls_days))
  if (processing == 'sequential') {
    oplan <- future::plan(processing)
  } else {
    oplan <- future::plan(processing, workers = ncores)
  }
  on.exit(future::plan(oplan), add = TRUE)
  if (interactive() & progressbar) {
    progressr::handlers(global = TRUE)
    progressr::handlers(list(
      progressr::handler_progress(format = paste(":spin :current/:total",
         "(:message) [:bar] :percent in :elapsed ETA: :eta"), width = 80,
                                  complete = "+")))
    p <- progressr::progressor(along = urls)
  } else {
    p <- local(function(...) NULL)
    environment(p) <- new.env(parent = emptyenv())
  }
  result <- furrr::future_map_dfr(urls, ~ {
    options(timeout = 3600)
    tf <- tempfile(fileext = ".zip")
    download.file(.x, tf, quiet = T, method = "libcurl")
    csv <- unzip(tf, exdir = dirname(tf))
    file.remove(tf)
    dat <- data.table::fread(csv[1], select = c(6, 2),
                             col.names = c("time", "price"), nThread = 1)
    file.remove(csv)
    if (dat$time[1] > 1e14) {
      div <- 1e6
    } else {
      div <- 1e3
    }
    dat[, time := lubridate::as_datetime(as.numeric(time) %/% div, tz = tz)]
    if (aggregated) {
      dat[, time := lubridate::floor_date(time, unit = interval)]
      dat <- dat[, by = time, .(price = mean(price), min_price = min(price),
                                max_price = max(price))]
    } else {
      dat <- dat[!(price == dplyr::lag(price) & price == dplyr::lead(price))]
    }
    p()
    dat[time >= start_time & time <= end_time]
  })
  data.table::setorder(result, time)
  result
}
