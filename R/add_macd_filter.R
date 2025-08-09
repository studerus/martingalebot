#' Add a flexible MACD-based signal filter to a data.table
#'
#' @description
#' This function calculates the Moving Average Convergence Divergence (MACD) on a
#' resampled time series and creates a logical signal column. It can be configured
#' to signal a bullish crossover (MACD line crosses above its signal line) or a
#' bearish crossover (MACD line crosses below its signal line).
#'
#' The function modifies the input `data.table` by reference for maximum memory
#' efficiency.
#'
#' @param dt A `data.table` containing 'time' and 'price' columns, sorted by time.
#' @param time_period The time frame for resampling, e.g., "1 hour", "15 minutes".
#' @param nFast The number of periods for the fast moving average.
#' @param nSlow The number of periods for the slow moving average.
#' @param nSig The number of periods for the signal line moving average.
#' @param column_name The name of the logical column to be created. Defaults to `"deal_start"`.
#' @param macd_is_above_signal If `TRUE` (default), the signal is `TRUE` on a bullish
#'   crossover (macd > signal). If `FALSE`, it's `TRUE` on a bearish crossover (macd < signal).
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
#' # --- Example 1: Standard Bullish Crossover for Deal Start ---
#' dat <- get_binance_prices_from_csv(
#'   'PYRUSDT',
#'   start_time = '2023-02-01',
#'   end_time = '2023-02-28',
#'   progressbar = FALSE
#' ) |>
#'   add_macd_filter(time_period = "1 hour", macd_is_above_signal = TRUE)
#'
#' # --- Example 2: Bearish Crossover for Emergency Stop ---
#' data_with_stop <- dat |>
#'   add_macd_filter(
#'     time_period = "4 hour",
#'     column_name = "emergency_stop",
#'     macd_is_above_signal = FALSE
#'   )
#' tail(data_with_stop)
#' }
add_macd_filter <- function(dt, time_period = "1 hour", nFast = 12, nSlow = 26, nSig = 9,
                          column_name = "deal_start", macd_is_above_signal = TRUE) {
  # --- Input Validation ---
  stopifnot(data.table::is.data.table(dt))
  if (!"time" %in% names(dt) || !"price" %in% names(dt)) {
    stop("Input 'dt' must contain 'time' and 'price' columns.")
  }
  stopifnot(is.character(column_name), length(column_name) == 1)
  stopifnot(is.logical(macd_is_above_signal), length(macd_is_above_signal) == 1)

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
      dt_resampled[, c("macd", "signal") := data.table::as.data.table(macd_vals)]
  } else {
      dt_resampled[, c("macd", "signal") := .(NA_real_, NA_real_)]
  }

  # 3. Create the temporary signal based on the crossover condition.
  if (macd_is_above_signal) {
    dt_resampled[, signal_temp := macd > signal]
  } else {
    dt_resampled[, signal_temp := macd < signal]
  }

  # 4. Map the signal back using a robust lookup-join with forward fill.
  lookup_table <- dt_resampled[, .(time, signal_temp)]
  dt[, (column_name) := lookup_table[dt, on = .(time), roll = TRUE, x.signal_temp]]

  # Keep NA values (insufficient history). Backtest will treat NA as FALSE.

  # --- Return ---
  # Adding [] resolves printing issues after in-place modifications.
  dt[]
}
