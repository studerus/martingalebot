#' Calculate the "price down tolerance" for a specific parameter setting
#'
#' This function calculates the "price down tolerance" (i.e. the percentage
#' price deviation from the initial order price to the take profit price when
#' all safety orders are filled) for a specific parameter setting. This function
#' is vectorized. So, you can also apply it to vectors of parameters.
#'
#' @inheritParams backtest
#'
#' @return
#' a numeric vector of price down tolerances
#'
#' @export
#'
#' @examples
#' down_tolerance(base_order_volume = 10, first_safety_order_volume = 10,
#'                n_safety_orders = 8, pricescale = 1.5, volumescale = 1.5,
#'                take_profit = 3, stepscale = 1.2)
down_tolerance <- function(base_order_volume, first_safety_order_volume,
                           n_safety_orders, pricescale, volumescale,
                           take_profit, stepscale) {
  dev <- volume <- vector('numeric', n_safety_orders + 1)
  volume[1] <- base_order_volume
  volume[2] <- first_safety_order_volume
  dev[2] <- pricescale
  if (n_safety_orders > 1) {
    for (i in 3:length(dev)) {
      volume[i] = volume[i - 1] * volumescale
      dev[i] = dev[i - 1] + (dev[i - 1] - dev[i - 2]) * stepscale
    }
  }
  avg_pr <- sum(dev * volume) / sum(volume)
  avg_pr - take_profit
}
down_tolerance <- Vectorize(down_tolerance)
