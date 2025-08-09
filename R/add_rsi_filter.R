#' Add an RSI-based signal filter to a data.table
#'
#' @description
#' This function calculates the Relative Strength Index (RSI) on a resampled
#' time series and creates a logical column based on a cutoff. It can be used
#' to create `deal_start` signals (e.g., RSI is low, indicating oversold) or
#' `emergency_stop` signals (e.g., a weekly RSI indicates a bear market).
#'
#' The function modifies the input `data.table` by reference for maximum memory
#' efficiency.
#'
#' @param dt A `data.table` containing 'time' and 'price' columns.
#' @param time_period The time frame for resampling, e.g., "1 hour", "1 day", "1 week".
#' @param n The number of periods for the RSI calculation.
#' @param cutoff The RSI threshold.
#' @param column_name The name of the logical column to be created. Defaults to "deal_start".
#' @param rsi_is_above If `TRUE`, the signal is `TRUE` when RSI > cutoff. If `FALSE` (default),
#'   the signal is `TRUE` when RSI < cutoff.
#'
#' @return The `data.table` `dt` is returned after being modified in place.
#'
#' @seealso \code{\link{add_sma_filter}}, \code{\link{add_bollinger_filter}}, \code{\link{add_macd_filter}}
#'
#' @export
#'
#' @importFrom data.table .N `:=`
#' @importFrom TTR RSI
#'
#' @examples
#' \dontrun{
#' # --- Example 1: Standard "Oversold" Deal Start on 4-hour chart ---
#' dat <- get_binance_prices_from_csv(
#'   'ATOMUSDT',
#'   start_time = '2022-01-01',
#'   end_time = '2022-06-30',
#'   progressbar = FALSE
#' ) |>
#'   add_rsi_filter(
#'     time_period = "4 hours", n = 14, cutoff = 30, rsi_is_above = FALSE
#'   )
#'
#' # --- Example 2: Weekly RSI for long-term Emergency Stop ---
#' # A weekly RSI breaking below 40 can be a sign of a bear market.
#' data_with_stop <- get_binance_prices_from_csv(
#'   'BTCUSDT',
#'   start_time = '2020-01-01',
#'   end_time = '2022-12-31',
#'   progressbar = FALSE
#' ) |>
#'   add_rsi_filter(
#'     time_period = "1 week",
#'     n = 14,
#'     cutoff = 40,
#'     column_name = "emergency_stop",
#'     rsi_is_above = FALSE
#'   )
#'
#' print(data_with_stop[emergency_stop == TRUE])
#' }
add_rsi_filter <- function(dt, time_period = "1 hour", n = 7, cutoff = 30,
                         column_name = "deal_start", rsi_is_above = FALSE) {
  # --- Input Validation ---
  stopifnot(data.table::is.data.table(dt))
  if (!"time" %in% names(dt) || !"price" %in% names(dt)) {
    stop("Input 'dt' must contain 'time' and 'price' columns.")
  }

  # --- Configuration ---
  interval_map <- c("3 minutes" = 180, "5 minutes" = 300, "15 minutes" = 900,
                    "30 minutes" = 1800, "1 hour" = 3600, "2 hours" = 7200,
                    "4 hours" = 14400, "8 hours" = 28800, "1 day" = 24 * 3600,
                    "1 week" = 7 * 24 * 3600)
  interval_sec <- interval_map[time_period]
  if (is.na(interval_sec)) {
    stop(paste("Invalid time_period. Choose from:", paste(names(interval_map), collapse = ", ")))
  }

  # --- Calculation ---
  dt_resampled <- dt[, .(time = time[.N], price = price[.N]), by = .(bucket = as.integer(time) %/% interval_sec)]
  data.table::setorder(dt_resampled, time)

  if (nrow(dt_resampled) > n) {
      dt_resampled[, RSI := TTR::RSI(price, n = n)]
  } else {
      dt_resampled[, RSI := NA_real_]
  }
  
  lookup_table <- dt_resampled[!is.na(RSI)]
  dt[, RSI_temp := lookup_table[dt, on = .(time), roll = TRUE, x.RSI]]

  # --- Create Signal Column ---
  if (rsi_is_above) {
    dt[, (column_name) := RSI_temp > cutoff]
  } else {
    dt[, (column_name) := RSI_temp < cutoff]
  }

  # Keep NAs for upstream logic; backtest will treat NAs as FALSE
  dt[, RSI_temp := NULL]

  # --- Return ---
  dt[]
}
