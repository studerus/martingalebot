#' Perform a grid search to find the most profitable parameter settings
#'
#' This function takes possible values of martingale bot parameters and runs the
#' function [backtest()] with each possible combination of these values. It can
#' be used to quickly identify the most profitable parameter combination for a
#' given time period. By default, the grid search is run in parallel using all
#' available CPU cores.
#'
#' @param data A `data.table` containing `time` and `price` columns, and
#'   optionally a `deal_start` column.
#' @param base_order_volume a numeric vector of sizes of the base order (in the
#'   quote currency)
#' @param first_safety_order_volume a numeric vector of sizes of the first
#'   safety order (in the quote currency)
#' @param n_safety_orders a numeric vector of the maximum numbers of safety
#'   orders
#' @param pricescale a numeric vector of pricescales
#' @param volumescale a numeric vector of volumescales (volume multipliers)
#' @param take_profit a numeric vector of take profit percentages
#' @param stepscale a numeric vector of stepscales (price deviation multipliers)
#' @param start_asap a logical vector indicating whether deals should start as
#'   soon as possible
#' @param stoploss a numeric vector of stoploss percentages
#' @param compound a logical vector indicating whether to compound profits
#' @param min_covered_deviation the minimum percentage price deviation from the
#'   initial order to the last safety order a given parameter combination must
#'   cover. Parameter combinations that have a covered price deviation less than
#'   this value are discarded and not tested.
#' @param min_down_tolerance the minimum price down tolerance (i.e. percentage
#'   price deviation from the initial order price to the take profit price when
#'   all safety orders are filled) a given parameter combination must have.
#'   Parameter combinations that have a price down tolerance less than this
#'   value are discarded and not tested.
#' @param max_required_capital the maximum capital a given parameter
#'   constellation can require. Parameter constellations that require more
#'   capital than this value are discarded and not tested.
#' @param n_timeperiods the number of timeperiods the function [backtest()]
#'   should be applied to. Be default, [backtest()] is applied only once per
#'   parameter constellation using the full time period of the supplied dataset.
#'   If n_timeperiods is larger than 1, each parameter constellation is tested
#'   with n_timeperiods. Each timeperiod starts `timeperiods_shift_days` later
#'   but ends at the same time. The results are aggregated over all timeperiods
#'   using the median. The idea behind this is to identify parameter
#'   constellations that are more robust and potentially better generalize to
#'   new data.
#' @param timeperiods_shift_days the number of days the timeperiods should be
#'   shifted. This only has an effect if `n_timeperiods` is larger than 1.
#' @param processing a character string specifying the processing of the job.
#'   Default is `"sequential"` which corresponds to sequential processing.
#'   If you want to use parallel processing, set this argument to
#'   `"multisession"`.
#' @param ncores the number of CPU cores used in parallel processing.
#' @param progressbar whether to show a progressbar or not
#' @return  A `data.frame` in which each row contains the results of one
#'   possible combination of parameters. The rows of the returned `data.frame`
#'   are ordered by the profit column.
#' @importFrom purrr exec
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
#' # Perform grid search
#' res <- grid_search(dat, progressbar = F)
#'
#' # Plot the best result using purrr::exec
#' # First, get the single row of best parameters
#' best_params <- res |> dplyr::slice(1)
#' # Then, use exec to call backtest, splicing the parameters with !!!
#' exec(backtest, !!!best_params, data = dat, plot = TRUE)
#' }
grid_search <- function(data,
                        base_order_volume = 10,
                        first_safety_order_volume = 10,
                        n_safety_orders = seq(8, 16, by = 2),
                        pricescale = seq(0.6, 3, by = 0.4),
                        volumescale = seq(1, 2, by = 0.2),
                        take_profit = seq(1, 3.5, by = 0.5),
                        stepscale = seq(0.8, 1, by = 0.1),
                        start_asap = T,
                        stoploss = c(0, 25, 30, 40),
                        compound = T,
                        min_covered_deviation = 8,
                        min_down_tolerance = 8,
                        max_required_capital = 10000,
                        n_timeperiods  = 1,
                        timeperiods_shift_days = 1,
                        progressbar = TRUE,
                        processing = "sequential",
                        ncores = parallel::detectCores()) {
  l <- tidyr::crossing(base_order_volume, first_safety_order_volume,
                       n_safety_orders, pricescale, volumescale, take_profit,
                       stepscale, start_asap, stoploss, compound) |>
    dplyr::filter(dplyr::between(covered_deviation(n_safety_orders, pricescale,
                                                   stepscale),
                                 min_covered_deviation, 100),
                  down_tolerance(base_order_volume, first_safety_order_volume,
                                 n_safety_orders, pricescale, volumescale,
                                 take_profit, stepscale) >= min_down_tolerance,
                  required_capital(base_order_volume, first_safety_order_volume,
                                   n_safety_orders,
                                   volumescale) <= max_required_capital)
  if (interactive() & progressbar) {
    progressr::handlers(global = TRUE)
    progressr::handlers(list(
    progressr::handler_progress(format = paste(":spin :current/:total",
       "(:message) [:bar] :percent in :elapsed ETA: :eta"), width = 80,
        complete = "+")))
    p <- progressr::progressor(steps = nrow(l) * n_timeperiods)
  } else {
    p <- local(function(...) NULL)
    environment(p) <- new.env(parent = emptyenv())
  }
  options(future.rng.onMisuse = "ignore")
  if (processing == 'sequential') {
    oplan <- future::plan(processing)
  } else {
    oplan <- future::plan(processing, workers = ncores)
  }
  on.exit(future::plan(oplan), add = TRUE)
   if (n_timeperiods == 1) {
     furrr::future_pmap_dfr(l, function(...) {
       p()
       params <- list(...)
       result <- exec(backtest, data, !!!params)
       tibble::as_tibble(c(result, params))
     }, .options = furrr::furrr_options(seed = TRUE)) |>
       dplyr::arrange(dplyr::desc(profit), stoploss)
   } else {
     purrr::map_df(seq_len(n_timeperiods), function(timeperiod) {
       min_time <- min(data$time) + lubridate::ddays(timeperiods_shift_days) *
         (timeperiod - 1)
       data2 <- dplyr::filter(data, time >= min_time)
       furrr::future_pmap_dfr(l, function(...) {
         p()
         params <- list(...)
         result <- exec(backtest, data2, !!!params)
         tibble::as_tibble(c(result, params))
       }, .options = furrr::furrr_options(seed = TRUE))
     }) |>
     dplyr::group_by(base_order_volume, first_safety_order_volume,
              n_safety_orders, pricescale, volumescale,
              take_profit, stepscale, start_asap, stoploss, compound) |>
     dplyr::summarise(dplyr::across(dplyr::everything(), median),
                      .groups = 'drop') |>
       dplyr::arrange(dplyr::desc(profit), stoploss) |>
       dplyr::relocate(dplyr::any_of(names(l)), .after = n_stoploss)
   }
}
