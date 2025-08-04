#' Add a "Death Cross" signal filter to a data.table
#'
#' @description
#' This function implements a "Death Cross" indicator, a widely recognized
#' long-term market signal. A Death Cross occurs when a shorter-term moving
#' average (e.g., 50-day SMA) crosses below a longer-term moving average
#' (e.g., 200-day SMA). It is often interpreted as a signal of a potential
#' major bear market.
#'
#' The function calculates the two SMAs on daily resampled data and creates a
#' logical column, typically `emergency_stop`. The signal is `TRUE` at the
#' moment the crossover happens.
#'
#' The function modifies the input `data.table` by reference.
#'
#' @param dt A `data.table` containing 'time' and 'price' columns, sorted by time.
#' @param n_short The number of periods for the short-term SMA (e.g., 50).
#' @param n_long The number of periods for the long-term SMA (e.g., 200).
#' @param column_name The name of the logical column to be created. Defaults to
#'   `"emergency_stop"`.
#'
#' @return The `data.table` `dt` is returned after being modified in place.
#'
#' @seealso \code{\link{add_sma_filter}}, \code{\link{add_rsi_filter}}
#'
#' @export
#'
#' @importFrom data.table shift .N `:=`
#' @importFrom TTR SMA
#'
#' @examples
#' \dontrun{
#' # Use a long-term dataset to see the effect of the Death Cross.
#' # Here we'll simulate it with shorter periods for demonstration.
#' dat <- get_binance_prices_from_csv(
#'   'BTCUSDT',
#'   start_time = '2021-01-01',
#'   end_time = '2022-12-31',
#'   progressbar = FALSE
#' ) |>
#'   add_death_cross_filter(n_short = 50, n_long = 200)
#'
#' # Find the exact moment the emergency stop was triggered
#' dat[emergency_stop == TRUE]
#'
#' # The signal should be very infrequent.
#' table(dat$emergency_stop)
#' }
add_death_cross_filter <- function(dt, n_short = 50, n_long = 200, column_name = "emergency_stop") {
  # --- Input Validation ---
  stopifnot(data.table::is.data.table(dt))
  if (!"time" %in% names(dt) || !"price" %in% names(dt)) {
    stop("Input 'dt' must contain 'time' and 'price' columns.")
  }
  if (n_short >= n_long) {
    stop("'n_short' must be less than 'n_long'.")
  }

  # --- Configuration ---
  time_period <- "1 day" # Death cross is traditionally a daily signal.
  interval_sec <- 24 * 3600

  # --- Calculation ---
  # 1. Resample data to daily prices.
  resampled_data <- dt[, .(time = time[.N], price = price[.N]), by = .(bucket = as.integer(time) %/% interval_sec)]
  data.table::setorder(resampled_data, time)

  # 2. Calculate short and long SMAs.
  if (nrow(resampled_data) > n_long) {
      resampled_data[, sma_short := TTR::SMA(price, n = n_short)]
      resampled_data[, sma_long := TTR::SMA(price, n = n_long)]
  } else {
      resampled_data[, c("sma_short", "sma_long") := .(NA_real_, NA_real_)]
  }

  # 3. CRITICAL: Lag the SMAs by one day to prevent lookahead bias.
  #    We need the state from the *previous* day to decide on the *current* day's signal.
  resampled_data[, `:=`(
    sma_short_lag1 = data.table::shift(sma_short, n = 1L, type = "lag"),
    sma_long_lag1 = data.table::shift(sma_long, n = 1L, type = "lag")
  )]

  # 4. Identify the crossover event.
  #    A Death Cross occurs when:
  #    - Yesterday: short SMA was >= long SMA
  #    - Today:     short SMA is < long SMA
  resampled_data[,
    crossover_event := (sma_short_lag1 >= sma_long_lag1) & (sma_short < sma_long)
  ]
  # Replace NA with FALSE (for the first few rows where comparison is not possible)
  resampled_data[is.na(crossover_event), crossover_event := FALSE]

  # 5. Map the signal back to the original data.
  #    The signal is valid for the entire day on which the crossover is confirmed.
  lookup_table <- resampled_data[crossover_event == TRUE, .(time, signal = TRUE)]

  if (nrow(lookup_table) > 0) {
    # Create the column and initialize with FALSE
    dt[, (column_name) := FALSE]
    # For each day a crossover happened, find the corresponding rows in the original data.
    # We find the start and end of that day to apply the signal.
    for (i in 1:nrow(lookup_table)) {
      day_start <- lookup_table$time[i]
      # Find the start of the *next* day to define the interval end.
      next_day_start_row <- resampled_data[time > day_start, .I[1]]
      day_end <- if (!is.na(next_day_start_row)) resampled_data$time[next_day_start_row] else dt[.N, time] + 1

      # Apply the signal to all original timestamps within that day's bucket.
      dt[time >= day_start & time < day_end, (column_name) := TRUE]
    }
  } else {
    # If no crossover ever occurs, just create a column of FALSEs.
    dt[, (column_name) := FALSE]
  }


  # --- Return ---
  dt[]
}
