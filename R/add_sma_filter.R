#' Add an SMA-based signal filter to a data.table
#'
#' @description
#' This function calculates a Simple Moving Average (SMA) on a resampled time
#' series and creates a logical signal column. The column can be configured to
#' signal when the price is above the SMA (e.g., for a `deal_start` trend filter)
#' or below it (e.g., for an `emergency_stop` signal).
#'
#' The function modifies the input `data.table` by reference for maximum memory
#' efficiency.
#'
#' @param dt A `data.table` containing 'time' and 'price' columns. The function
#'   assumes the data is already sorted by time.
#' @param time_period The time frame for resampling. Defaults to `"1 day"`, which
#'   is standard for trend-following strategies.
#' @param n The number of periods for the SMA lookback (e.g., 100 days).
#' @param column_name The name of the logical column to be created. Defaults to `"deal_start"`.
#' @param price_is_above If `TRUE` (default), the signal is `TRUE` when price > SMA.
#'   If `FALSE`, the signal is `TRUE` when price < SMA.
#'
#' @return The `data.table` `dt` is returned after being modified in place,
#'   allowing for use in a `magrittr` pipeline.
#'
#' @seealso \code{\link{add_rsi_filter}}, \code{\link{add_bollinger_filter}}, \code{\link{add_macd_filter}}
#'
#' @export
#'
#' @importFrom data.table shift .N `:=`
#' @importFrom TTR SMA
#'
#' @examples
#' \dontrun{
#' # --- Example 1: Standard Trend Filter ---
#' # Download data and add the SMA filter for a 'deal_start' signal.
#' # For a short 1-month period, a shorter SMA (e.g., 10) is more illustrative.
#' dat <- get_binance_prices_from_csv(
#'   'PYRUSDT',
#'   start_time = '2023-02-01',
#'   end_time = '2023-02-28',
#'   progressbar = FALSE
#' ) |>
#'   add_sma_filter(n = 10, column_name = "deal_start", price_is_above = TRUE)
#'
#' # Perform backtesting using the new 'deal_start' column
#' backtest(data = dat, start_asap = FALSE)
#'
#' # --- Example 2: Emergency Stop Filter ---
#' # Reuse 'dat' and add a stop signal for when price drops below the 20-day SMA.
#' data_with_stop <- dat |>
#'   add_sma_filter(n = 20, column_name = "emergency_stop", price_is_above = FALSE)
#'
#' # Check the last few rows to see the new 'emergency_stop' column
#' tail(data_with_stop)
#' }
add_sma_filter <- function(dt, time_period = "1 day", n = 100,
                         column_name = "deal_start", price_is_above = TRUE) {
  # --- Input Validation ---
  stopifnot(data.table::is.data.table(dt))
  if (!"time" %in% names(dt) || !"price" %in% names(dt)) {
    stop("Input 'dt' must contain 'time' and 'price' columns.")
  }
  stopifnot(is.character(column_name), length(column_name) == 1)
  stopifnot(is.logical(price_is_above), length(price_is_above) == 1)


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

  # 2. Calculate SMA on the resampled data.
  if (nrow(resampled_data) > n) {
      resampled_data[, sma := TTR::SMA(price, n = n)]
  } else {
      resampled_data[, sma := NA_real_]
  }

  # 3. CRITICAL: Lag the SMA by one period to prevent lookahead bias.
  resampled_data[, sma_for_comparison := data.table::shift(sma, n = 1L, type = "lag")]

  # 4. Map the lagged SMA value back to the main table using a robust lookup-join.
  lookup_table <- resampled_data[!is.na(sma_for_comparison)]
  dt[, sma_temp := lookup_table[dt, on = .(time), roll = TRUE, x.sma_for_comparison]]

  # 5. Create the final signal column by reference based on the price_is_above flag.
  if (price_is_above) {
    dt[, (column_name) := price > sma_temp]
  } else {
    dt[, (column_name) := price < sma_temp]
  }

  # Replace any resulting NAs with FALSE. This happens at the start of the
  # series where the indicator is not yet defined.
  dt[is.na(get(column_name)), (column_name) := FALSE]

  # 6. Clean up the temporary column.
  dt[, sma_temp := NULL]

  # --- Return ---
  # Return the data.table. Adding [] is a data.table idiom that helps
  # resolve printing issues after a chain of in-place modifications.
  dt[]
}
