#' Add a Rate of Change (ROC) based signal filter to a data.table
#'
#' @description
#' This function calculates the Rate of Change (ROC) indicator on a resampled
#' time series. ROC measures the percentage change in price between the current
#' price and the price `n` periods ago. It is a pure momentum oscillator.
#'
#' This filter is particularly useful for creating an `emergency_stop` signal
#' based on strong, sustained downward momentum. To improve signal stability and
#' reduce false positives from single volatile candles ("wicks"), the calculation
#' can be performed on a smoothed price line (e.g., a 7-day SMA of the price).
#'
#' The function modifies the input `data.table` by reference.
#'
#' @param dt A `data.table` containing 'time' and 'price' columns, sorted by time.
#' @param time_period The time frame for resampling. Defaults to `"1 day"`.
#' @param n The number of lookback periods for the ROC calculation (e.g., 90 for 90 days).
#' @param cutoff The ROC percentage threshold (e.g., -30 for a 30% drop).
#' @param smoothing_period An integer. If > 0, an SMA of this period is applied to the
#'   price data *before* calculating the ROC. This smooths the price and makes the
#'   signal more robust. Defaults to 0 (no smoothing).
#' @param column_name The name of the logical column to be created. Defaults to `"emergency_stop"`.
#' @param roc_is_below If `TRUE` (default), the signal is `TRUE` when ROC < cutoff.
#'   If `FALSE`, the signal is `TRUE` when ROC > cutoff.
#'
#' @return The `data.table` `dt` is returned after being modified in place.
#'
#' @seealso \code{\link{add_sma_filter}}, \code{\link{add_rsi_filter}}
#'
#' @export
#'
#' @importFrom data.table .N `:=`
#' @importFrom TTR ROC SMA
#'
#' @examples
#' \dontrun{
#' # Use a long-term dataset to see the effect of the ROC emergency stop.
#' dat <- get_binance_prices_from_csv(
#'   'BTCUSDT',
#'   start_time = '2020-01-01',
#'   end_time = '2022-12-31',
#'   progressbar = FALSE
#' ) |>
#'   add_roc_filter(
#'     time_period = "1 day",
#'     n = 90,
#'     cutoff = -40,
#'     smoothing_period = 7, # <-- Use a 7-day smoothed price for ROC calculation
#'     column_name = "emergency_stop"
#'   )
#'
#' # Find when the emergency stop was triggered
#' print(dat[emergency_stop == TRUE])
#' }
add_roc_filter <- function(dt, time_period = "1 day", n = 90, cutoff = -30,
                         smoothing_period = 0, column_name = "emergency_stop",
                         roc_is_below = TRUE) {
  # --- Input Validation ---
  stopifnot(data.table::is.data.table(dt))
  if (!"time" %in% names(dt) || !"price" %in% names(dt)) {
    stop("Input 'dt' must contain 'time' and 'price' columns.")
  }
  stopifnot(is.numeric(smoothing_period), smoothing_period >= 0)
  stopifnot(is.character(column_name), length(column_name) == 1)
  stopifnot(is.logical(roc_is_below), length(roc_is_below) == 1)

  # --- Configuration ---
  interval_map <- c("3 minutes" = 180, "5 minutes" = 300, "15 minutes" = 900,
                    "30 minutes" = 1800, "1 hour" = 3600, "2 hours" = 7200,
                    "4 hours" = 14400, "8 hours" = 28800, "1 day" = 24 * 3600)
  interval_sec <- interval_map[time_period]
  if (is.na(interval_sec)) {
    stop(paste("Invalid time_period. Choose from:", paste(names(interval_map), collapse = ", ")))
  }

  # --- Calculation ---
  # 1. Resample data.
  resampled_data <- dt[, .(time = time[.N], price = price[.N]), by = .(bucket = as.integer(time) %/% interval_sec)]
  data.table::setorder(resampled_data, time)

  # 2. (Optional) Smooth the price series before calculating ROC.
  price_series <- resampled_data$price
  if (smoothing_period > 0) {
    if (nrow(resampled_data) > smoothing_period) {
      price_series <- TTR::SMA(price_series, n = smoothing_period)
    } else {
      # Not enough data to smooth, effectively skip ROC calculation
      price_series <- rep(NA_real_, nrow(resampled_data))
    }
  }

  # 3. Calculate ROC on the (potentially smoothed) price series.
  if (nrow(resampled_data) > n) {
      # TTR::ROC returns a decimal (e.g., -0.3 for -30%), so we multiply by 100.
      # We explicitly use type = "discrete" for the standard (P_t / P_{t-n} - 1) formula.
      resampled_data[, roc := TTR::ROC(price_series, n = n, type = "discrete") * 100]
  } else {
      resampled_data[, roc := NA_real_]
  }

  # 4. Map the ROC value back to the main table using a robust lookup-join.
  lookup_table <- resampled_data[!is.na(roc)]
  dt[, roc_temp := lookup_table[dt, on = .(time), roll = TRUE, x.roc]]

  # 5. Create the final signal column by reference.
  if (roc_is_below) {
    dt[, (column_name) := roc_temp < cutoff]
  } else {
    dt[, (column_name) := roc_temp > cutoff]
  }

  # Replace NAs with FALSE (occurs at the start of the series).
  dt[is.na(get(column_name)), (column_name) := FALSE]

  # 6. Clean up the temporary column.
  dt[, roc_temp := NULL]


  # --- Return ---
  dt[]
}
