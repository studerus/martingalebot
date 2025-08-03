library(testthat)
library(data.table)

test_that("Trailing take profit activates and closes deal correctly", {

  # 1. Create an artificial price scenario
  # Start at 100, rise to 102 (TP), peak at 105, then fall to trigger trailing stop
  price_vec <- c(100, 98, 101, 102, 103, 104, 105, 104.5, 103.95, 103)
  time_vec <- seq.POSIXt(from = as.POSIXct("2023-01-01 00:00:00", tz = "UTC"), by = "min", length.out = length(price_vec))
  
  test_data <- data.table(
    time = time_vec,
    price = price_vec
  )

  # 2. Define parameters for the test
  # - take_profit = 2% (triggers at 102)
  # - trailing_take_profit = TRUE
  # - trailing_rate = 1% (105 * (1 - 0.01) = 103.95)
  params <- list(
    data = test_data,
    base_order_volume = 100, # Use 100 for easy percentage calculations
    n_safety_orders = 0, # Simplify test with no safety orders
    take_profit = 2.0,
    trailing_take_profit = TRUE,
    trailing_rate = 0.01, # 1% trailing
    show_trades = TRUE # We need to inspect the trades
  )

  # 3. Run the backtest
  result_trades_raw <- backtest(
    data = params$data,
    base_order_volume = params$base_order_volume,
    n_safety_orders = params$n_safety_orders,
    take_profit = params$take_profit,
    trailing_take_profit = params$trailing_take_profit,
    trailing_rate = params$trailing_rate,
    show_trades = params$show_trades
  )
  
  # Filter out empty buffer rows
  actual_trades <- result_trades_raw[result_trades_raw$Time > as.POSIXct("1971-01-01"), ]

  # 4. Verify the results
  # We expect one buy, one sell, and another buy because start_asap=T
  expect_equal(nrow(actual_trades), 3)

  buy_trade <- actual_trades[1, ]
  sell_trade <- actual_trades[2, ]
  
  # Check the buy order
  expect_equal(buy_trade$Price, 100)
  expect_equal(buy_trade$Trade_Type, "Buy")
  
  # Check the sell order - this is the critical part
  # The deal should close at the trailing stop price, not the initial TP
  expected_sell_price <- 105 * (1 - 0.01)
  expect_equal(sell_trade$Price, expected_sell_price)
  expect_equal(sell_trade$Trade_Type, "Take Profit")

  # Check the final profit
  result_summary <- backtest(
    data = params$data,
    base_order_volume = params$base_order_volume,
    n_safety_orders = params$n_safety_orders,
    take_profit = params$take_profit,
    trailing_take_profit = params$trailing_take_profit,
    trailing_rate = params$trailing_rate,
    show_trades = FALSE # Get the summary now
  )

  # Profit should be based on the ~4% gain, not the 2% initial TP
  # (Ignoring fees for simplicity in this test)
  expect_gt(result_summary$profit, 3.5)
  expect_lt(result_summary$profit, 4.5)
})
