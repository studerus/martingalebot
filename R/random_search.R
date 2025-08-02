#' Perform a random search using Latin Hypercube Sampling
#'
#' This function generates a set of random parameter combinations using Latin
#' Hypercube Sampling to efficiently explore the parameter space. It then runs
#' the [backtest()] function for each combination to find profitable settings.
#' This can be more efficient than a grid search when dealing with a large number
#' of parameters.
#'
#' @param data A `data.table` containing `time` and `price` columns, and
#'   optionally a `deal_start` column.
#' @param n_samples The total number of random parameter combinations to test.
#' @param n_safety_orders_bounds A numeric vector of length 2 specifying the
#'   min and max number of safety orders (integer values).
#' @param pricescale_bounds A numeric vector of length 2 for the pricescale bounds.
#' @param volumescale_bounds A numeric vector of length 2 for the volumescale bounds.
#' @param take_profit_bounds A numeric vector of length 2 for the take profit bounds.
#' @param stepscale_bounds A numeric vector of length 2 for the stepscale bounds.
#' @param stoploss_values A numeric vector of specific stoploss percentages to
#'   sample from (e.g., `c(0, 25, 30, 40)`).
#' @param base_order_volume_bounds A numeric vector of length 2 for the base
#'   order volume bounds. Defaults to a fixed value.
#' @param first_safety_order_volume_bounds A numeric vector of length 2 for the
#'   first safety order volume bounds. Defaults to a fixed value.
#' @param start_asap A fixed logical value; `TRUE` to start deals immediately.
#' @param compound A fixed logical value; `TRUE` to compound profits.
#' @param min_covered_deviation A hard filter to discard combinations that do not
#'   meet the minimum covered deviation.
#' @param min_down_tolerance A hard filter to discard combinations that do not
#'   meet the minimum down tolerance.
#' @param max_required_capital A hard filter to discard combinations that exceed
#'   the maximum required capital.
#' @param progressbar A logical value; `TRUE` to show a progress bar.
#' @param processing A character string specifying processing, either
#'   `"sequential"` or `"multisession"`.
#' @param ncores The number of CPU cores for parallel processing.
#'
#' @return A `tibble` where each row contains the results of one parameter
#'   combination, ordered by profit.
#' @importFrom lhs randomLHS
#' @export
#' @examples
#' \dontrun{
#' # Download price data
#' dat <- get_binance_prices_from_csv(
#'   "PYRUSDT",
#'   start_time = "2025-01-01",
#'   end_time = "2025-03-01",
#'   progressbar = F
#' )
#'
#' # Perform random search
#' res <- random_search(dat, n_samples = 100, progressbar = F)
#'
#' # Plot the best result
#' res %>%
#'   slice_max(profit, n = 1) %>%
#'   exec(backtest, !!!., data = dat, plot = TRUE)
#' }
random_search <- function(data,
                          n_samples = 1000,
                          n_safety_orders_bounds = c(8, 16),
                          pricescale_bounds = c(0.6, 3.0),
                          volumescale_bounds = c(1.0, 2.0),
                          take_profit_bounds = c(1.0, 3.5),
                          stepscale_bounds = c(0.8, 1.0),
                          stoploss_values = c(0, 25, 30, 40),
                          base_order_volume_bounds = c(10, 10),
                          first_safety_order_volume_bounds = c(10, 10),
                          start_asap = TRUE,
                          compound = TRUE,
                          min_covered_deviation = 8,
                          min_down_tolerance = 8,
                          max_required_capital = 10000,
                          progressbar = TRUE,
                          processing = "sequential",
                          ncores = parallel::detectCores()) {

  # 1. Generate a Latin Hypercube Sample for numeric parameters
  # The matrix has n_samples rows and 7 columns (for 7 numeric params)
  # Values are normalized between 0 and 1.
  lhs_matrix <- lhs::randomLHS(n_samples, 7)

  # 2. Scale the LHS matrix to the actual parameter bounds
  params <- tibble::tibble(
    base_order_volume = lhs_matrix[,1] * (base_order_volume_bounds[2] - base_order_volume_bounds[1]) + base_order_volume_bounds[1],
    first_safety_order_volume = lhs_matrix[,2] * (first_safety_order_volume_bounds[2] - first_safety_order_volume_bounds[1]) + first_safety_order_volume_bounds[1],
    n_safety_orders = floor(lhs_matrix[,3] * (n_safety_orders_bounds[2] - n_safety_orders_bounds[1] + 1) + n_safety_orders_bounds[1]),
    pricescale = lhs_matrix[,4] * (pricescale_bounds[2] - pricescale_bounds[1]) + pricescale_bounds[1],
    volumescale = lhs_matrix[,5] * (volumescale_bounds[2] - volumescale_bounds[1]) + volumescale_bounds[1],
    take_profit = lhs_matrix[,6] * (take_profit_bounds[2] - take_profit_bounds[1]) + take_profit_bounds[1],
    stepscale = lhs_matrix[,7] * (stepscale_bounds[2] - stepscale_bounds[1]) + stepscale_bounds[1],
    
    # 3. Sample categorical/fixed parameters
    stoploss = sample(stoploss_values, n_samples, replace = TRUE),
    
    # Add fixed parameters
    start_asap = start_asap,
    compound = compound
  )

  # 4. Pre-filter the parameter set based on constraints
  l <- params |>
    dplyr::filter(
      dplyr::between(covered_deviation(n_safety_orders, pricescale, stepscale), min_covered_deviation, 100),
      down_tolerance(base_order_volume, first_safety_order_volume, n_safety_orders, pricescale, volumescale, take_profit, stepscale) >= min_down_tolerance,
      required_capital(base_order_volume, first_safety_order_volume, n_safety_orders, volumescale) <= max_required_capital
    )

  if (nrow(l) == 0) {
    warning("No parameter combinations met the specified constraints. Returning an empty tibble.")
    return(tibble::tibble())
  }
  
  # 5. Setup progress bar and parallel processing (re-used from grid_search)
  if (interactive() & progressbar) {
    progressr::handlers(global = TRUE)
    progressr::handlers(list(
      progressr::handler_progress(format = paste(":spin :current/:total",
         "(:message) [:bar] :percent in :elapsed ETA: :eta"), width = 80,
          complete = "+")))
    p <- progressr::progressor(steps = nrow(l))
  } else {
    p <- function(...) NULL
  }
  
  options(future.rng.onMisuse = "ignore")
  if (processing == 'sequential') {
    oplan <- future::plan(processing)
  } else {
    oplan <- future::plan(processing, workers = ncores)
  }
  on.exit(future::plan(oplan), add = TRUE)

  # 6. Execute backtests
  results <- furrr::future_pmap_dfr(l, function(...) {
    if(exists("p")) p()
    params <- list(...)
    result <- exec(backtest, data, !!!params)
    tibble::as_tibble(c(result, params))
  }, .options = furrr::furrr_options(seed = TRUE))
  
  # 7. Return sorted results
  if (nrow(results) > 0) {
    results <- results |> dplyr::arrange(dplyr::desc(profit), stoploss)
  }
  
  return(results)
}
