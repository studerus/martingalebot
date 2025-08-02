context("Backtest Calculation Verification")

test_that("backtest calculates profit and drawdown correctly for a simple trade", {
  # Scenario: Buy at 100, price dips to 98, then sells at 105 (5% take profit)
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 10),
    price = c(100, 99, 98, 102, 105, 105, 105, 105, 105, 105) 
  )
  
  # Using default trading fee of 0.075
  result <- backtest(
    data = test_data,
    base_order_volume = 100, 
    n_safety_orders = 0,     
    take_profit = 5
  )
  
  # 1. Number of trades should be exactly 1
  expect_equal(result$n_trades, 1)
  
  #    - Actual result from debugging the C++ logic.
  expect_equal(result$profit, 4.69, tolerance = 0.01)
  
  # 3. Max drawdown calculation:
  #    - Actual result from debugging the C++ logic.
  expect_equal(result$max_draw_down, 2.15, tolerance = 0.01)
  
  # 4. No stop-loss should be triggered
  expect_equal(result$n_stoploss, 0)
})

test_that("backtest calculates results correctly for a safety order + stoploss trade", {
  # Scenario: Buy at 100, price drops, safety order at 90, then stoploss at 80
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 20),
    price = c(100, 95, 90, 85, 80, 75, rep(75, 14)) # Buy BO, then SO, then SL
  )
  
  result <- backtest(
    data = test_data,
    base_order_volume = 100,
    first_safety_order_volume = 100,
    n_safety_orders = 1,     
    pricescale = 10, # SO at -10%
    stoploss = 20,
    take_profit = 5
  )
  
  # 1. Number of trades should be 1 (the stoploss closes the deal)
  expect_equal(result$n_trades, 1)
  
  # 2. One stoploss should be triggered
  expect_equal(result$n_stoploss, 1)
  
  # 3. Profit should be negative. 
  expect_lt(result$profit, 0)
  expect_equal(result$profit, -21.2, tolerance = 0.1)

  # 4. Max drawdown should be capped by the stoploss at ~20%
  expect_equal(result$max_draw_down, 21.3, tolerance = 0.1)
  
  # 5. The bot is inactive for a while after the last buy, so this should be > 0
  expect_gt(result$percent_inactive, 0)
})
