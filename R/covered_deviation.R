#' Calculate the covered price deviation for a specific parameter setting
#'
#' This function calculates the covered deviation (i.e. the price deviation
#' between the initial order and the last safety order) for a given number
#' of safety orders, pricescale and stepscale. This function is vectorized. So,
#' you can also apply it to vectors of number of safety orders, pricescales
#' and stepscales.
#'
#' @inheritParams backtest
#'
#' @return
#' a numeric vector of covered deviations
#' @export
#'
#' @examples
#' covered_deviation(n_safety_orders = 2:8, pricescale = 1.5, stepscale = 1.2)
covered_deviation <- function(n_safety_orders, pricescale, stepscale) {
  dev <- vector('numeric', n_safety_orders + 1)
  dev[2] = pricescale
  if (n_safety_orders > 1) {
    for (i in 3:length(dev)) {
      dev[i] = dev[i - 1] + (dev[i - 1] - dev[i - 2]) * stepscale
    }
  }
  tail(dev, 1)
}
covered_deviation <- Vectorize(covered_deviation)
