#' Add an RSI-based deal start filter to a data.table
#'
#' @description
#' This function calculates the Relative Strength Index (RSI) on a resampled
#' time series (e.g., hourly) and then maps the signals back to the original
#' high-frequency data. It creates a logical `deal_start` column which is `TRUE`
#' when the RSI is below a specified cutoff. This prepares the data for use with
#' `backtest(..., start_asap = FALSE)`.
#'
#' The function modifies the input `data.table` by reference for maximum memory
#' efficiency, meaning no large data copies are made.
#'
#' @param dt A `data.table` containing 'time' and 'price' columns. The function
#'   assumes the data is already sorted by time for performance reasons.
#' @param time_period The time frame for resampling, e.g., "1 hour", "15 minutes".
#' @param n The number of periods for the RSI calculation.
#' @param cutoff The RSI threshold below which a deal can be started.
#'
#' @return The `data.table` `dt` is returned after being modified in place,
#'   allowing for use in a `magrittr` pipeline.
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
#' # Download data and add the RSI filter in a single pipeline.
#' dat <- get_binance_prices_from_csv(
#'   'PYRUSDT',
#'   start_time = '2023-02-01',
#'   end_time = '2023-02-28',
#'   progressbar = FALSE
#' ) |>
#'   add_rsi_filter(time_period = "30 minutes", cutoff = 30)
#'
#' # Perform backtesting using the new 'deal_start' column
#' backtest(data = dat, start_asap = FALSE)
#' }
add_rsi_filter <- function(dt, time_period = "1 hour", n = 7, cutoff = 30) {
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
  #    We use integer division on the timestamp for efficient grouping.
  dt_resampled <- dt[, .(time = time[.N], price = price[.N]), by = .(bucket = as.integer(time) %/% interval_sec)]
  # Ensure resampled data is sorted for RSI calculation
  data.table::setorder(dt_resampled, time)

  # 2. Calculate RSI on the resampled data. Handle cases with insufficient data.
  if (nrow(dt_resampled) > n) {
      dt_resampled[, RSI := TTR::RSI(price, n = n)]
  } else {
      dt_resampled[, RSI := NA_real_]
  }

  # 3. Map RSI back using a robust lookup-join.
  #    This syntax avoids a bug in certain update-joins that can produce
  #    spurious NAs. It looks up the correct rolled value for each row in dt.
  lookup_table <- dt_resampled[!is.na(RSI)]
  dt[, RSI_temp := lookup_table[dt, on = .(time), roll = TRUE, x.RSI]]

  # 4. Create the final 'deal_start' column by reference.
  dt[, deal_start := RSI_temp < cutoff]

  # 5. Clean up the temporary column.
  dt[, RSI_temp := NULL]

  # --- Return ---
  # Return the data.table. Adding [] is a data.table idiom that helps
  # resolve printing issues after a chain of in-place modifications.
  dt[]
}
