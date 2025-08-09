#' Calculate the "price down tolerance" for a specific parameter setting
#'
#' This function calculates the price down tolerance, i.e. the percentage
#' price deviation from the initial order price to the take profit price when
#' all safety orders are filled.
#'
#' Implementation detail: The calculation uses the weighted-average purchase
#' price per coin (WAP). If \eqn{V_i} are the order sizes in quote currency and
#' the order prices are \eqn{P_i = P_0 (1 - d_i/100)}, then the average cost per
#' coin is
#' \deqn{\bar P = \frac{\sum_i V_i}{\sum_i V_i / P_i}.}
#' The take profit price is
#' \deqn{P_{TP} = \bar P\, (1 + \mathrm{take\_profit}/100).}
#' Since \eqn{P_i} are proportional to \eqn{P_0}, the ratio simplifies to
#' \deqn{\frac{P_{TP}}{P_0} = (1 + \mathrm{take\_profit}/100)\, \frac{\sum_i V_i}{\sum_i V_i / (1 - d_i/100)}.}
#' Therefore, the down tolerance (percentage drop from start price to TP price) is
#' \deqn{100\,\left(1 - \frac{P_{TP}}{P_0}\right) = 100\,\left(1 - (1 + \mathrm{take\_profit}/100)\, \frac{\sum_i V_i}{\sum_i V_i / (1 - d_i/100)}\right).}
#'
#' This matches the intuitive definition and the visualization used in
#' `plot_martingale_config(plot_type = "allocation")`.
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
  # Build per-order deviations (%) and dollar volumes
  dev <- volume <- vector('numeric', n_safety_orders + 1)
  volume[1] <- base_order_volume
  if (n_safety_orders >= 1) {
    volume[2] <- first_safety_order_volume
    dev[2] <- pricescale
  }
  if (n_safety_orders > 1) {
    for (i in 3:length(dev)) {
      volume[i] <- volume[i - 1] * volumescale
      dev[i] <- dev[i - 1] + (dev[i - 1] - dev[i - 2]) * stepscale
    }
  }
  # Handle edge case n_safety_orders == 0
  if (n_safety_orders == 0) {
    dev[1] <- 0
  }

  # Compute ratio TP_price / starting_price without needing starting price
  sumV <- sum(volume)
  denom <- sum(volume / (1 - dev / 100))
  tp_over_start <- (1 + take_profit / 100) * (sumV / denom)

  # Down tolerance as percentage drop from starting price to TP price
  100 * (1 - tp_over_start)
}
down_tolerance <- Vectorize(down_tolerance)

