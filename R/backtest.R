#' Backtest for a single pair martingale trading strategy
#'
#' This function performs a backtest for a single pair martingale trading
#' strategy as implemented by single pair dca bots on
#' [3commas](https://3commas.io/), [Pionex](https://www.pionex.com/),
#' [TradeSanta](https://tradesanta.com/),
#' [OKX](https://www.okx.com/learn/introducing-the-spot-dollar-cost-averaging-dca-bot)
#' and others. It is mostly written in C++ to maximize speed.
#'
#' @param data A `data.table` containing `time` and `price` columns. If the
#' argument `start_asap` is set to `FALSE` and additional columns `deal_start`
#' is required.
#' @param base_order_volume The size of the base order (in the quote currency)
#' @param first_safety_order_volume The size of the first safety order (in
#' the quote currency)
#' @param n_safety_orders The maximum number of safety orders
#' @param pricescale Price deviation to open safety orders (% from initial
#'  order)
#' @param volumescale With what number should the funds used by the last safety
#'   order be multiplied?
#' @param take_profit At what percentage in profit should the bot close the
#'   deal?
#' @param stepscale With what number should the price deviation percentage used
#'   by the last safety order be multiplied?
#' @param stoploss At what percentage of drawdown should a stoploss be
#'   triggered? If set to zero (default), a stoploss will never be triggered.
#' @param start_asap Should new deals be started immediately after last deal was
#'   closed. If set to `FALSE` new deals are only started where the logical
#'   vector
#' `deal_start` in `data` is TRUE.
#' @param plot Whether to plot the results
#' @param show_trades Whether to return a `data.frame` showing the first trades
#' @param compound Whether to compound the profit or not. Default is `TRUE`.
#' @param trading_fee The trading fee percentage. By default it is set to 0.075,
#' which corresponds to the current trading fee on Binance if paid with BNB.
#' @param ... Additional arguments that have no effect.
#'
#' @return
#' By default a `data.frame` with one row is returned containing the following
#' values:
#' * `profit`: The percentage of profit
#' * `n_trades`: The number of deals (cycles) that have been closed.
#' * `max_draw_down`: The maximum percentage of draw down
#' * `required_capital`: How much capital is needed to run a bot with the
#' current settings
#' * `covered deviation`: The percentage price deviation from the initial order
#' to the last safety order.
#' * `down_tolerance`: The percentage price deviation from the initial order price
#' to the take profit price when all safety orders are filled.
#' * `max_time`: The maximum number of days the bot was in a stuck position
#' (maximum number of days of beeing fully invested).
#' * `percent_inactive`: The percentag of time the bot was in a stuck position.
#' That is, all safety orders were filled and the bot was fully invested.
#' * `n_stoploss`: The number of stoplosses that had been triggered.
#'
#' If the argument `show_trades` is `TRUE` a `data.frame` with the first few
#' trades is returned. This was mainly implemented to check the correctness
#' of the trading algorithm.
#'
#' If the argument `plot` is `TRUE` an interactive plot showing the changes in
#' capital and price of the traded cryptocurrency over time as well as

#' performed buy and sell orders and stoplosses.
#'
#' @export
#'
#' @examples
#' #Download some price data and perform backtesting
#' get_binance_prices_from_csv(
#'   'PYRUSDT',
#'    start_time = '2025-01-01',
#'    end_time = '2025-03-01',
#'    progressbar = F
#' ) |>
#' backtest()
backtest <- function(data, base_order_volume = 10, first_safety_order_volume = 10,
                n_safety_orders = 8, pricescale = 2.4, volumescale = 1.5,
                take_profit = 2.4, stepscale = 1, stoploss = 0, start_asap = T,
                plot = F, show_trades = F, compound = T,
                trading_fee = 0.075, ...) {
  output <- botCfun(base_order_volume = base_order_volume,
                    first_safety_order_volume = first_safety_order_volume,
                    n_safety_orders = n_safety_orders, pricescale = pricescale,
                    volumescale = volumescale, take_profit = take_profit,
                    pricemult = stepscale, stoploss = stoploss,
                    trading_fee = trading_fee, show_trades = show_trades,
                    plot = plot, start_asap = start_asap, compound = compound,
                    price = data$price, date = data$time,
                    deal_start = if (start_asap) T else data$deal_start)
  if (plot) {
    fee_factor <- (100 - trading_fee) / 100
    df <- output[[2]] |>
      dplyr::filter(Time != 0) |>
      dplyr::arrange(Time) |>
      dplyr::mutate(Cycle = 1 + cumsum(!is.na(dplyr::lag(Sold)))) |>
      dplyr::filter(cumsum(Capital) > 0) |>
      dplyr::group_by(Cycle) |>
      dplyr::mutate(Capital = ifelse(is.na(Sold),
                dplyr::first(Capital[Capital != 0]) - cumsum(Dollar) +
                fee_factor * cumsum(Bought) * Price, Capital)) |>
      dplyr::ungroup(Cycle) |>
      dplyr::mutate(Capital = 100 * Capital / dplyr::first(Capital))
    buys <- dplyr::filter(df, Average_Buy_Price != 0) |>
      tibble::add_column(name = 'Price')
    sells <- dplyr::filter(df, Price_Change > 0) |>
      tibble::add_column(name = 'Price')
    stoplosses <- dplyr::filter(df, Price_Change < 0 & is.na(Bought)) |>
      tibble::add_column(name = 'Price')
    gg <- df |>
      dplyr::select(Time, Price, Capital) |>
      tidyr::pivot_longer(Price:Capital) |>
      ggplot2::ggplot(ggplot2::aes(y = value, x = Time)) +
      ggplot2::geom_path() +
      ggplot2::geom_point(data = buys, ggplot2::aes(x = Time, y = Price),
                          size = 1.6, color = 'red') +
      ggplot2::geom_point(data = sells, ggplot2::aes(x = Time, y = Price),
                          size = 1.4, color = 'green') +
      ggplot2::geom_point(data = stoplosses, ggplot2::aes(x = Time, y = Price),
                          size = 1.6, color = 'blue') +
      ggplot2::facet_wrap(. ~ name, ncol = 1, scales = 'free') +
      ggplot2::labs(title = 'title', y = NULL, x = NULL) +
      tidyquant::theme_tq()

    p <- plotly::ggplotly(gg, height = if (interactive())  NULL else 700) |>
      plotly::layout(margin = list(t = 100)) |>
      plotly::layout(title = list(text = paste0(
        'Bot profit: ', round(output[[1]]$profit, 1),
        ' %; Max draw down: ', round(output[[1]]$max_draw_down, 1),
        ' % <br><sup>', paste0('Safety orders: ', n_safety_orders,
                                  ', Price scale: ', round(pricescale, 1),
                                  ', Volume scale: ', round(volumescale, 1),
                                  ', Take profit: ', round(take_profit, 1),
                                  ', Step scale: ', round(stepscale, 1),
                                  ', Stoploss: ', round(stoploss, 1),
                                  ', Start ASAP: ', start_asap), '</sup>')))
    if (!interactive()) {
      htmltools::tagList(p)
    } else {
      print(p)
      tibble::tibble(output[[1]])
    }
  } else {
    if (show_trades) {
      output[[2]]
    } else {
      tibble::tibble(output[[1]])
    }
  }
}
