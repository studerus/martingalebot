#' Plot Martingale Strategy (Timeline or Capital Allocation)
#'
#' @description
#' Visualize a martingale strategy configuration in two complementary ways:
#' - "timeline": shows how the strategy would unfold over time as safety orders
#'   are triggered, with purchase prices on the Y-axis and order sequence on the X-axis.
#' - "allocation": shows horizontal stacked bars per price level with the dollar
#'   amounts for newly invested capital, previously invested capital, accumulated
#'   loss, and uninvested funds on the X-axis and price level on the Y-axis.
#'
#' Both views display the take-profit level derived from the weighted average price
#' when all orders are filled.
#'
#' @param starting_price The initial price at which the base order would be placed.
#' @param base_order_volume The size of the base order (in quote currency).
#' @param first_safety_order_volume The size of the first safety order.
#' @param n_safety_orders The maximum number of safety orders.
#' @param pricescale Price deviation to open safety orders (% from initial order).
#' @param volumescale Volume multiplier for each subsequent safety order.
#' @param take_profit Profit target percentage when all orders are filled.
#' @param stepscale Price deviation multiplier for safety orders.
#' @param plot_type Which plot to produce. One of `"timeline"` (default) or
#'   `"allocation"`.
#'
#' @return A ggplot2 object showing the requested visualization.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_hline geom_text
#'   labs theme_minimal scale_size_continuous scale_color_gradient2
#'   scale_y_continuous scale_x_continuous element_text annotate geom_area
#'   geom_col scale_fill_manual
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' # Visualize strategy progression
#' plot_martingale_config(
#'   starting_price = 100,
#'   base_order_volume = 10,
#'   first_safety_order_volume = 10,
#'   n_safety_orders = 8,
#'   pricescale = 2.4,
#'   volumescale = 1.5,
#'   take_profit = 2.4
#' )
#' }
plot_martingale_config <- function(starting_price,
                                 base_order_volume = 10,
                                 first_safety_order_volume = 10,
                                 n_safety_orders = 8,
                                 pricescale = 2.4,
                                 volumescale = 1.5,
                                 take_profit = 2.4,
                                  stepscale = 1.0,
                                  plot_type = c("timeline", "allocation")) {
  plot_type <- match.arg(plot_type)

  # Input validation
  stopifnot(starting_price > 0)
  stopifnot(n_safety_orders >= 0)
  stopifnot(all(c(base_order_volume, first_safety_order_volume, pricescale,
                  volumescale, take_profit, stepscale) > 0))

  # Calculate order levels and volumes
  n_orders <- n_safety_orders + 1

  # Initialize vectors
  dev <- volume <- price <- vector('numeric', n_orders)
  order_type <- vector('character', n_orders)

  # Base order (index 1)
  volume[1] <- base_order_volume
  dev[1] <- 0
  price[1] <- starting_price
  order_type[1] <- "Base Order"

  # Safety orders (indices 2 to n_orders)
  if (n_safety_orders > 0) {
    volume[2] <- first_safety_order_volume
    dev[2] <- pricescale
    price[2] <- starting_price * (100 - dev[2]) / 100
    order_type[2] <- "Safety Order 1"

    if (n_safety_orders > 1) {
      for (i in 3:n_orders) {
        volume[i] <- volume[i - 1] * volumescale
        dev[i] <- dev[i - 1] + (dev[i - 1] - dev[i - 2]) * stepscale
        price[i] <- starting_price * (100 - dev[i]) / 100
        order_type[i] <- paste("Safety Order", i - 1)
      }
    }
  }

  # Calculate cumulative metrics
  cumulative_volume <- cumsum(volume)
  coins_bought <- cumsum(volume / price)  # Total coins acquired
  weighted_avg_price <- cumulative_volume / coins_bought
  portfolio_value <- coins_bought * price  # Value if sold at current trigger price
  remaining_cash <- sum(volume) - cumulative_volume

  # Calculate final take profit price
  final_weighted_avg <- weighted_avg_price[n_orders]
  take_profit_price <- final_weighted_avg * (100 + take_profit) / 100
  # Down tolerance for subtitle (see down_tolerance.R)
  dtol <- down_tolerance(
    base_order_volume = base_order_volume,
    first_safety_order_volume = first_safety_order_volume,
    n_safety_orders = n_safety_orders,
    pricescale = pricescale,
    volumescale = volumescale,
    take_profit = take_profit,
    stepscale = stepscale
  )

  # Create data frame for plotting (buys)
  plot_data <- data.frame(
    step = 1:n_orders,
    order_type = order_type,
    price = price,
    volume = volume,
    cumulative_volume = cumulative_volume,
    coins_bought_total = coins_bought,
    weighted_avg_price = weighted_avg_price,
    portfolio_value = portfolio_value,
    remaining_cash = remaining_cash,
    deviation = dev,
    stringsAsFactors = FALSE
  )
  # Add take profit point as last time step with sell volume in USD
  tp_sell_dollar <- coins_bought[n_orders] * take_profit_price
  plot_data_tp <- rbind(
    plot_data,
    data.frame(
      step = n_orders + 1,
      order_type = "Take Profit",
      price = take_profit_price,
      volume = tp_sell_dollar,
      cumulative_volume = sum(volume),
      coins_bought_total = coins_bought[n_orders],
      weighted_avg_price = weighted_avg_price[n_orders],
      portfolio_value = NA_real_,
      remaining_cash = 0,
      deviation = NA_real_,
      stringsAsFactors = FALSE
    )
  )

  # Branch: build the requested plot type
  if (identical(plot_type, "timeline")) {
    # Timeline plot
    p <- ggplot2::ggplot(plot_data_tp, ggplot2::aes(x = step, y = price)) +
      ggplot2::geom_line(color = "#666666", size = 0.8, alpha = 0.7) +
      # Buy points (blue)
      ggplot2::geom_point(
        data = subset(plot_data_tp, order_type != "Take Profit"),
        mapping = ggplot2::aes(size = volume),
        alpha = 0.85, color = "red"
      ) +
      # Price labels for buy points only
      ggplot2::geom_text(
        data = subset(plot_data_tp, order_type != "Take Profit"),
        mapping = ggplot2::aes(label = paste0("$", round(price, 2))),
        vjust = -1.2, size = 3, fontface = "bold"
      ) +
      ggplot2::geom_text(
        data = subset(plot_data_tp, order_type != "Take Profit"),
        mapping = ggplot2::aes(label = paste0("Vol: $", round(volume, 0))),
        vjust = 2.2, size = 2.5, color = "gray30"
      ) +
      # TP point highlighted in green on top
      ggplot2::geom_point(
        data = subset(plot_data_tp, order_type == "Take Profit"),
        mapping = ggplot2::aes(size = volume),
        alpha = 0.95, color = "#27AE60"
      ) +
      # Compact TP label to avoid overlap
      ggplot2::geom_text(
        data = subset(plot_data_tp, order_type == "Take Profit"),
        mapping = ggplot2::aes(label = "TP"),
        vjust = -1.0, size = 3.2, fontface = "bold", color = "#27AE60"
      ) +
      # TP volume label below the TP point
      ggplot2::geom_text(
        data = subset(plot_data_tp, order_type == "Take Profit"),
        mapping = ggplot2::aes(label = paste0("Vol: $", round(volume, 0))),
        vjust = 2.2, size = 2.5, color = "gray30"
      ) +
      ggplot2::scale_size_continuous(range = c(3, 12), guide = "none") +
       ggplot2::labs(
        title = "Martingale Strategy Timeline",
        subtitle = paste0(
          "Starting: $", starting_price,
          " | Down Tolerance: ", round(dtol, 2), "%",
          " | Required Capital: $", sprintf("%.2f", sum(volume))
        ),
         x = NULL,
        y = "Purchase Price ($)",
        caption = "Point size = Dollar amount (buys and final sell)"
      ) +
       ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
        legend.position = "none",
         panel.grid.minor = ggplot2::element_blank(),
         axis.text = ggplot2::element_text(size = 9),
         axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
         plot.margin = ggplot2::margin(t = 20, r = 20, b = 30, l = 20)
       ) +
      ggplot2::scale_x_continuous(breaks = 1:(n_orders + 1), labels = c(plot_data$order_type, "Take Profit")) +
       ggplot2::scale_y_continuous(labels = function(x) paste0("$", x),
                                   expand = ggplot2::expansion(mult = c(0.08, 0.18))) +
       ggplot2::coord_cartesian(clip = "off") +
      # TP label slightly above the TP point for visibility
      ggplot2::annotate(
        "text", x = n_orders + 1,
        y = take_profit_price + (max(price) - min(price)) * 0.06,
        label = paste0("TP $", round(take_profit_price, 2)),
        color = "black", fontface = "bold", size = 3.2
      )
  } else {
    # Allocation plot (horizontal stacked bars by price level)
    total_capital <- sum(volume)
    # Build per-level components
    plot_data_list <- vector("list", length = n_orders)
    for (i in seq_len(n_orders)) {
      newly_invested <- volume[i]
      previously_invested_amount <- if (i == 1) 0 else sum(volume[1:(i - 1)])
      coins_bought_before <- if (i == 1) 0 else sum(volume[1:(i - 1)] / price[1:(i - 1)])
      previously_invested_value <- coins_bought_before * price[i]
      accumulated_loss_i <- max(0, previously_invested_amount - previously_invested_value)
      total_invested_i <- previously_invested_amount + newly_invested
      uninvested_funds_i <- max(0, total_capital - total_invested_i)
      plot_data_list[[i]] <- data.frame(
        price_level = price[i],
        order_label = if (i == 1) "Base Order" else paste0("SO ", i - 1),
        newly_invested = newly_invested,
        previously_invested = previously_invested_value,
        accumulated_loss = accumulated_loss_i,
        uninvested_funds = uninvested_funds_i,
        stringsAsFactors = FALSE
      )
    }
    alloc_df <- do.call(rbind, plot_data_list)
    # Per-level TP and potential profit if price returns to per-level TP
    alloc_df$coins_bought <- vapply(seq_len(n_orders), function(i) sum(volume[1:i] / price[1:i]), numeric(1))
    alloc_df$total_invested <- vapply(seq_len(n_orders), function(i) sum(volume[1:i]), numeric(1))
    alloc_df$wap <- with(alloc_df, ifelse(coins_bought > 0, total_invested / coins_bought, NA_real_))
    alloc_df$tp_price <- alloc_df$wap * (1 + take_profit / 100)
    alloc_df$potential_profit <- with(alloc_df, pmax(0, coins_bought * tp_price - total_invested))
    # Determine a sensible bar height in y-units (so the bars are visible on numeric y-axis)
    bar_height <- if (n_orders > 1) {
      diff_vals <- diff(sort(unique(price)))
      if (length(diff_vals) == 0) max(price) * 0.05 else max(min(diff_vals) * 0.8, 1e-6)
    } else {
      max(price) * 0.1
    }
    alloc_long <- tidyr::pivot_longer(
      alloc_df,
      cols = c("newly_invested", "previously_invested", "accumulated_loss", "uninvested_funds"),
      names_to = "capital_type",
      values_to = "amount"
    )
    alloc_long$capital_type <- factor(
      alloc_long$capital_type,
      levels = c("uninvested_funds", "accumulated_loss", "previously_invested", "newly_invested"),
      labels = c("Uninvested Funds", "Accumulated Loss", "Previously Invested", "Newly Invested")
    )
    p <- ggplot2::ggplot(alloc_long, ggplot2::aes(x = amount, y = price_level, fill = capital_type)) +
      ggplot2::geom_col(position = "stack", alpha = 0.85, orientation = "y", width = bar_height) +
      ggplot2::scale_fill_manual(
        values = c(
          "Newly Invested" = "#2ECC71",
          "Previously Invested" = "#3498DB",
          "Accumulated Loss" = "#E74C3C",
          "Uninvested Funds" = "#95A5A6"
        ),
        name = "Capital Type"
      ) +
      ggplot2::labs(
        title = "Martingale Strategy: Capital Allocation by Price Level",
        subtitle = paste0(
          "Required Capital: $", sprintf("%.2f", total_capital),
          " | Down Tolerance: ", round(dtol, 2), "%",
          " | Max Orders: ", n_orders
        ),
        x = "Capital Allocation (USD)",
        y = "Price Level (USD)"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
        legend.position = "right",
        panel.grid.minor = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 9)
      ) +
      ggplot2::scale_x_continuous(labels = function(x) paste0("$", x),
                                  limits = c(0, total_capital * 1.02),
                                  expand = ggplot2::expansion(mult = c(0.01, 0.03))) +
      ggplot2::scale_y_continuous(breaks = price, labels = function(x) paste0("$", round(x, 2)))
    # Add labels for order and price at the ends
    p <- p +

      # Per-level TP and profit label on the right
      ggplot2::geom_text(
        data = alloc_df,
        ggplot2::aes(x = total_capital * 0.98, y = price_level,
                     label = sprintf("TP $%.2f  (+$%.2f)", tp_price, potential_profit)),
        inherit.aes = FALSE, hjust = 1, vjust = 0.5, size = 3, color = "#1E8449"
      )
  }

  # Add take profit annotation (position depends on plot type)
   if (identical(plot_type, "timeline")) {
     # No separate TP annotation; TP is shown as the final green point
   } else {
    # no global TP annotation for allocation plot; per-level labels are shown
  }

  # Removed bottom-right gray metrics annotation as requested

  return(p)
}
