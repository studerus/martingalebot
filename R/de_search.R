#' Perform parameter optimization using Differential Evolution
#'
#' This function uses the Differential Evolution algorithm from the `DEoptim`
#' package to find optimal parameters for the trading strategy. It intelligently
#' searches the parameter space to maximize a given objective metric.
#'
#' @param data A `data.table` containing `time` and `price` columns.
#' @param objective_metric A character string specifying the metric to maximize.
#'   Can be any column returned by `backtest`, e.g., `"profit"` (default),
#'   or a custom metric like `"profit / max_draw_down"`.
#' @param n_safety_orders_bounds A numeric vector of length 2 for the bounds
#'   (min, max) of `n_safety_orders`.
#' @param pricescale_bounds A numeric vector of length 2 for `pricescale`.
#' @param volumescale_bounds A numeric vector of length 2 for `volumescale`.
#' @param take_profit_bounds A numeric vector of length 2 for `take_profit`.
#' @param stepscale_bounds A numeric vector of length 2 for `stepscale`.
#' @param stoploss_bounds A numeric vector of length 2 for `stoploss`.
#' @param base_order_volume_bounds A numeric vector of length 2 for `base_order_volume`.
#' @param first_safety_order_volume_bounds A numeric vector of length 2 for
#'   `first_safety_order_volume`.
#' @param DEoptim_control A list of control parameters for `DEoptim`. See
#'   [DEoptim::DEoptim.control()] for details. Key parameters are `itermax` (max iterations),
#'   `NP` (population size), `F` (differential weighting factor), and `CR` (crossover probability).
#' @param ... Additional fixed parameters passed to `backtest`, such as
#'   `start_asap = TRUE` or `compound = TRUE`.
#'
#' @return A `tibble` containing the backtest results for the best found
#'   parameter combination.
#' @importFrom DEoptim DEoptim
#' @export
#' @examples
#' \dontrun{
#' # Download price data
#' dat <- get_binance_prices_from_csv(
#'   "PYRUSDT",
#'   start_time = "2025-01-01",
#'   end_time = "2025-02-01",
#'   progressbar = F
#' )
#'
#' # Perform DE optimization
#' best_result <- de_search(
#'   dat,
#'   objective_metric = "profit",
#'   DEoptim_control = list(itermax = 10, NP = 50, trace = FALSE)
#' )
#'
#' # Print the best result
#' print(best_result)
#'
#' # Plot the best result
#' best_result %>%
#'   exec(backtest, !!!., data = dat, plot = TRUE)
#' }
de_search <- function(data,
                      objective_metric = "profit",
                      n_safety_orders_bounds = c(8, 16),
                      pricescale_bounds = c(0.6, 3.0),
                      volumescale_bounds = c(1.0, 2.0),
                      take_profit_bounds = c(1.0, 3.5),
                      stepscale_bounds = c(0.8, 1.0),
                      stoploss_bounds = c(0, 40),
                      base_order_volume_bounds = c(10, 10),
                      first_safety_order_volume_bounds = c(10, 10),
                      DEoptim_control = list(
                        NP = 10 * 8, # 10 * number of parameters
                        itermax = 20,
                        trace = TRUE
                      ),
                      ...) {

  # Define lower and upper bounds for DEoptim
  lower <- c(
    n_safety_orders = n_safety_orders_bounds[1],
    pricescale = pricescale_bounds[1],
    volumescale = volumescale_bounds[1],
    take_profit = take_profit_bounds[1],
    stepscale = stepscale_bounds[1],
    stoploss = stoploss_bounds[1],
    base_order_volume = base_order_volume_bounds[1],
    first_safety_order_volume = first_safety_order_volume_bounds[1]
  )
  
  upper <- c(
    n_safety_orders = n_safety_orders_bounds[2],
    pricescale = pricescale_bounds[2],
    volumescale = volumescale_bounds[2],
    take_profit = take_profit_bounds[2],
    stepscale = stepscale_bounds[2],
    stoploss = stoploss_bounds[2],
    base_order_volume = base_order_volume_bounds[2],
    first_safety_order_volume = first_safety_order_volume_bounds[2]
  )

  # Objective function for DEoptim to MINIMIZE
  objective_function <- function(params_vec) {
    
    # 1. Name and round integer parameters
    params <- as.list(params_vec)
    names(params) <- names(lower)
    params$n_safety_orders <- round(params$n_safety_orders)

    # 2. Run the backtest with the current parameter set
    # Combine with fixed parameters from ...
    all_params <- c(params, list(...))
    result <- purrr::exec(backtest, data, !!!all_params)
    
    # 3. Calculate the objective value to be minimized
    # We use a try block in case the metric is complex (e.g., a division)
    # and might result in NA/NaN/Inf.
    obj_value <- tryCatch({
      # Create a local environment for evaluation
      eval_env <- as.list(result)
      # Evaluate the metric expression
      val <- eval(parse(text = objective_metric), envir = eval_env)
      # If the result is invalid or negative profit, penalize heavily
      if (!is.finite(val) || result$profit < 0) -Inf else val
    }, error = function(e) -Inf) # Penalize if metric calculation fails
    
    # DEoptim minimizes, so we return the NEGATIVE of our metric
    return(-obj_value)
  }
  
  # Run the optimization
  optim_result <- DEoptim::DEoptim(
    fn = objective_function,
    lower = lower,
    upper = upper,
    control = DEoptim_control
  )
  
  # Extract the best parameter set
  best_params_vec <- optim_result$optim$bestmem
  best_params <- as.list(best_params_vec)
  names(best_params) <- names(lower)
  best_params$n_safety_orders <- round(best_params$n_safety_orders)
  
  # Run a final backtest to get the full result tibble
  final_params <- c(best_params, list(...))
  final_result <- purrr::exec(backtest, data, !!!final_params)
  
  # Combine results and parameters and return as a tibble
  return(tibble::as_tibble(c(final_result, final_params)))
}
