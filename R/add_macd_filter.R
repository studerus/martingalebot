#' Add a MACD-based deal start filter to a data.table
#'
#' @description
#' This function calculates the Moving Average Convergence Divergence (MACD) on a
#' resampled time series (e.g., hourly) and then maps the signals back to the
#' original high-frequency data. It creates a logical `deal_start` column which is
#' `TRUE` when the MACD line is above its signal line, a common buy signal.
#' This prepares the data for use with `backtest(..., start_asap = FALSE)`.
#'
#' The function modifies the input `data.table` by reference for maximum memory
#' efficiency.
#'
#' @param dt A `data.table` containing 'time' and 'price' columns, sorted by time.
#' @param time_period The time frame for resampling, e.g., "1 hour", "15 minutes".
#' @param nFast The number of periods for the fast moving average.
#' @param nSlow The number of periods for the slow moving average.
#' @param nSig The number of periods for the signal line moving average.
#'
#' @return The `data.table` `dt` is returned after being modified in place,
#'   allowing for use in a `magrittr` pipeline.
#'
#'
#' @seealso \code{\link{add_rsi_filter}}, \code{\link{add_sma_filter}}, \code{\link{add_bollinger_filter}}
#'
#' @export
#'
#' @importFrom data.table .N `:=`
#' @importFrom TTR MACD
#'
#' @examples
#' \dontrun{
#' # Download data and add the MACD filter in a single pipeline.
#' dat <- get_binance_prices_from_csv(
#'   'PYRUSDT',
#'   start_time = '2023-02-01',
#'   end_time = '2023-02-28',
#'   progressbar = FALSE
#' ) |>
#'   add_macd_filter(time_period = "1 hour")
#'
#' # Perform backtesting using the new 'deal_start' column
#' backtest(data = dat, start_asap = FALSE)
#' }
add_macd_filter <- function(dt, time_period = "1 hour", nFast = 12, nSlow = 26, nSig = 9) {
  # --- Input Validation ---
  stopifnot(data.table::is.data.table(dt))
  if (!"time" %in% names(dt) || !"price" %in% names(dt)) {
    stop("Input 'dt' must contain 'time' and 'price' columns.")
  }

  # --- Configuration ---
  interval_map <- c("3 minutes" = 180, "5 minutes" = 300, "15 minutes" = 900,
                    "30 minutes" = 1800, "1 hour" = 3600, "2 hours" = 7200,
                    "4 hours" = 14400, "8 hours" = 28800, "1 day" = 24 * 3600)
  interval_sec <- interval_map[time_period]
  if (is.na(interval_sec)) {
    stop(paste("Invalid time_period. Choose from:", paste(names(interval_map), collapse = ", ")))
  }

  # --- Calculation ---
  # 1. Resample data: Get the last price for each time interval.
  dt_resampled <- dt[, .(time = time[.N], price = price[.N]), by = .(bucket = as.integer(time) %/% interval_sec)]
  data.table::setorder(dt_resampled, time)

  # 2. Calculate MACD. It returns a data.frame with 'macd' and 'signal' columns.
  if (nrow(dt_resampled) > nSlow) {
      macd_vals <- TTR::MACD(dt_resampled$price, nFast = nFast, nSlow = nSlow, nSig = nSig)
      dt_resampled[, c("macd", "signal") := as.data.table(macd_vals)]
  } else {
      dt_resampled[, c("macd", "signal") := .(NA_real_, NA_real_)]
  }

  # 3. Create the signal based on the MACD crossover.
  #    The signal is TRUE if macd > signal. NA comparison results in NA.
  dt_resampled[, deal_start_temp := macd > signal]

  # 4. Map the signal back using a robust lookup-join with forward fill.
  #    We create a small lookup table with just the time and the signal.
  lookup_table <- dt_resampled[, .(time, deal_start_temp)]
  dt[, deal_start := lookup_table[dt, on = .(time), roll = TRUE, x.deal_start_temp]]


  # --- Return ---
  # Adding [] resolves printing issues after in-place modifications.
  dt[]
}
