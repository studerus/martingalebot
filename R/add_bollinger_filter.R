#' Add a flexible Bollinger Band signal filter to a data.table
#'
#' @description
#' This function calculates Bollinger Bands and the corresponding %B indicator on a
#' resampled time series. It then creates a logical signal column based on a
#' cutoff. The signal can be configured to trigger when %B is below the cutoff
#' (indicating "oversold") or above it (indicating "overbought").
#'
#' The function modifies the input `data.table` by reference for maximum memory
#' efficiency.
#'
#' @param dt A `data.table` containing 'time' and 'price' columns. The function
#'   assumes the data is already sorted by time.
#' @param time_period The time frame for resampling, e.g., "1 hour", "15 minutes".
#'   Defaults to `"1 hour"`.
#' @param n The number of periods for the moving average calculation within the
#'   Bollinger Bands. Defaults to 20.
#' @param sd The number of standard deviations for the upper and lower bands.
#'   Defaults to 2.
#' @param cutoff The %B threshold.
#' @param column_name The name of the logical column to be created. Defaults to `"deal_start"`.
#' @param signal_on_below If `TRUE` (default), signal is `TRUE` when %B < cutoff.
#'   If `FALSE`, signal is `TRUE` when %B > cutoff.
#'
#' @return The `data.table` `dt` is returned after being modified in place,
#'   allowing for use in a `magrittr` pipeline.
#'
#' @seealso \code{\link{add_rsi_filter}}, \code{\link{add_sma_filter}}, \code{\link{add_macd_filter}}
#'
#' @export
#'
#' @importFrom data.table .N `:=`
#' @importFrom TTR BBands
#'
#' @examples
#' \dontrun{
#' # --- Example 1: Standard "Oversold" Deal Start Filter ---
#' dat <- get_binance_prices_from_csv(
#'   'PYRUSDT',
#'   start_time = '2023-02-01',
#'   end_time = '2023-02-28',
#'   progressbar = FALSE
#' ) |>
#'   add_bollinger_filter(n = 20, cutoff = 0.05, signal_on_below = TRUE)
#'
#' # --- Example 2: "Overbought" Emergency Stop Signal ---
#' # Use a high cutoff (e.g., 0.95) to signal a stop when price is near the upper band.
#' data_with_stop <- dat |>
#'   add_bollinger_filter(
#'     n = 20, cutoff = 0.95, column_name = "emergency_stop", signal_on_below = FALSE
#'   )
#' tail(data_with_stop)
#' }
add_bollinger_filter <- function(dt, time_period = "1 hour", n = 20, sd = 2, cutoff = 0.05,
                               column_name = "deal_start", signal_on_below = TRUE) {
  # --- Input Validation ---
  stopifnot(data.table::is.data.table(dt))
  if (!"time" %in% names(dt) || !"price" %in% names(dt)) {
    stop("Input 'dt' must contain 'time' and 'price' columns.")
  }
  stopifnot(is.character(column_name), length(column_name) == 1)
  stopifnot(is.logical(signal_on_below), length(signal_on_below) == 1)

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
  resampled_data <- dt[, .(time = time[.N], price = price[.N]), by = .(bucket = as.integer(time) %/% interval_sec)]
  data.table::setorder(resampled_data, time)

  # 2. Calculate Bollinger Bands on the resampled data.
  if (nrow(resampled_data) > n) {
      # TTR::BBands returns a data.frame with dn, mavg, up, pctB
      bbands_result <- TTR::BBands(resampled_data$price, n = n, sd = sd)
      resampled_data[, pctB := bbands_result[, "pctB"]]
  } else {
      resampled_data[, pctB := NA_real_]
  }

  # 3. Map the %B value back using a robust lookup-join.
  lookup_table <- resampled_data[!is.na(pctB)]
  dt[, pctB_temp := lookup_table[dt, on = .(time), roll = TRUE, x.pctB]]

  # 4. Create the final signal column by reference.
  if (signal_on_below) {
    dt[, (column_name) := pctB_temp < cutoff]
  } else {
    dt[, (column_name) := pctB_temp > cutoff]
  }

  # Replace NAs with FALSE.
  dt[is.na(get(column_name)), (column_name) := FALSE]

  # 5. Clean up the temporary column.
  dt[, pctB_temp := NULL]

  # --- Return ---
  # Return the data.table. Adding [] is a data.table idiom that helps
  # resolve printing issues after a chain of in-place modifications.
  dt[]
}
