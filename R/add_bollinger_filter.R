#' Add a Bollinger Band based deal start filter to a data.table
#'
#' @description
#' This function calculates Bollinger Bands and the corresponding %B indicator on a
#' resampled time series. It then creates a logical `deal_start` column, which
#' is `TRUE` when the %B value is below a specified cutoff. A low %B value
#' suggests the price is "oversold" relative to its recent volatility.
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
#' @param cutoff The %B threshold below which a deal can be started. Defaults to
#'   0.05 (i.e., when the price is at or below 5% of the band width from the
#'   lower band).
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
#' # Download data and add the Bollinger Band %B filter.
#' dat <- get_binance_prices_from_csv(
#'   'PYRUSDT',
#'   start_time = '2023-02-01',
#'   end_time = '2023-02-28',
#'   progressbar = FALSE
#' ) |>
#'   add_bollinger_filter(n = 20, cutoff = 0.05)
#'
#' # Perform backtesting using the new 'deal_start' column
#' backtest(data = dat, start_asap = FALSE)
#' }
add_bollinger_filter <- function(dt, time_period = "1 hour", n = 20, sd = 2, cutoff = 0.05) {
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

  # 4. Create the final 'deal_start' signal by reference.
  dt[, deal_start := pctB_temp < cutoff]

  # 5. Clean up the temporary column.
  dt[, pctB_temp := NULL]

  # --- Return ---
  # Return the data.table. Adding [] is a data.table idiom that helps
  # resolve printing issues after a chain of in-place modifications.
  dt[]
}
