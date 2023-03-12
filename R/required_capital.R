#' Calculate the required capital for a specific parameter setting
#'
#' This function calculates the required capital for a specific parameter
#' setting. This function is vectorized. So, you can also apply it to vectors of
#' parameters.
#'
#' @inheritParams backtest
#'
#' @return
#' a numeric vector of required capitals
#' @export
#'
#' @examples
#' required_capital(n_safety_orders = 6:16, volumescale = 1.5)
required_capital <- function(base_order_volume = 10,
                             first_safety_order_volume = 10,
                             n_safety_orders, volumescale) {
  order_sizes <- c(base_order_volume, first_safety_order_volume,
            first_safety_order_volume * volumescale^(1:(n_safety_orders - 1)))
  sum(order_sizes)
}
required_capital <- Vectorize(required_capital)
