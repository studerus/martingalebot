library(testthat)
library(data.table)

test_that("Emergency stop closes an active deal and prevents new ones", {

  # 1. Create an artificial price and signal scenario
  price_vec <- c(100, 98, 99, 101, 100, 99, 98)
  time_vec <- seq.POSIXt(from = as.POSIXct("2023-01-01 00:00:00", tz = "UTC"), by = "min", length.out = length(price_vec))
  
  # Emergency stop triggers at the 5th time step (price = 100)
  emergency_vec <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)

  test_data <- data.table(
    time = time_vec,
    price = price_vec,
    emergency_stop = emergency_vec
  )

  # 2. Run the backtest with emergency stop enabled
  result_summary <- backtest(
    data = test_data,
    n_safety_orders = 2,
    take_profit = 10.0, # High TP, will not be reached
    stoploss = 20.0,    # High SL, will not be reached
    use_emergency_stop = TRUE,
    show_trades = FALSE
  )
  
  result_trades_raw <- backtest(
    data = test_data,
    n_safety_orders = 2,
    take_profit = 10.0,
    stoploss = 20.0,
    use_emergency_stop = TRUE,
    show_trades = TRUE
  )

  actual_trades <- result_trades_raw[result_trades_raw$Time > as.POSIXct("1971-01-01"), ]

  # 3. Verify the results
  # Summary checks
  expect_equal(result_summary$n_emergency_stops, 1)
  expect_equal(result_summary$n_stoploss, 0)
  expect_equal(result_summary$n_trades, 1) # The emergency stop closes one deal

  # Trade log checks
  # Expect 1 initial buy, 1 emergency sell, and 1 new buy because start_asap is on. Total = 3.
  expect_equal(nrow(actual_trades), 3) 
  
  initial_buy <- actual_trades[1, ]
  emergency_sell <- actual_trades[2, ]
  new_buy <- actual_trades[3, ]
  
  # Check the initial buy
  expect_equal(initial_buy$Trade_Type, "Buy")
  expect_equal(initial_buy$Price, 100)
  
  # Check that the sell was an emergency stop at the correct price and time
  expect_equal(emergency_sell$Trade_Type, "Emergency Stop")
  expect_equal(emergency_sell$Price, 100) # Price at the time of the emergency signal
  
  # Compare the numeric (epoch) time to avoid timezone attribute issues
  expect_equal(as.numeric(emergency_sell$Time), as.numeric(as.POSIXct("2023-01-01 00:04:00", tz = "UTC")))

  # Check that a new deal was started correctly after the emergency period
  expect_equal(new_buy$Trade_Type, "Buy")
  expect_equal(new_buy$Price, 98) # Price of the last time step
})
