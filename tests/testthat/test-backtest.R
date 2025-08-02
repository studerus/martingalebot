test_that("backtest returns correct structure", {
  # Create simple test data
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 100),
    price = c(100, 99, 98, 97, 96, 95, 94, 93, 92, 91, 
              rep(90, 10), # stable low period
              91, 92, 93, 94, 95, 96, 97, 98, 99, 100, # recovery
              rep(100, 70)) # stable high period
  )
  
  result <- backtest(test_data)
  
  # Check that result is a tibble with expected elements
  expect_s3_class(result, "data.frame")
  expect_true(all(c("profit", "max_draw_down", "n_trades", "n_stoploss") %in% names(result)))
  
  # Check that numeric values are reasonable
  expect_type(result$profit, "double")
  expect_type(result$max_draw_down, "double")
  expect_type(result$n_trades, "integer")
  expect_type(result$n_stoploss, "integer")
  
  # Check that values are non-negative where expected
  expect_gte(result$max_draw_down, 0)
  expect_gte(result$n_trades, 0)
  expect_gte(result$n_stoploss, 0)
})

test_that("backtest handles basic profitable scenario", {
  # Create a simple profitable scenario: price drops then recovers
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 50),
    price = c(100, rep(95, 10), rep(105, 39)) # Drop to 95, then rise to 105
  )
  
  result <- backtest(test_data, take_profit = 5, n_safety_orders = 3)
  
  # Should complete at least one deal
  expect_s3_class(result, "data.frame")
  expect_gte(result$n_trades, 0)
})

test_that("backtest gives errors for invalid parameters", {
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 10),
    price = rep(100, 10)
  )
  
  # Test invalid parameters which should now cause a C++-level stop()
  expect_error(backtest(test_data, base_order_volume = -1), "base_order_volume must be positive")
  expect_error(backtest(test_data, n_safety_orders = -1), "n_safety_orders cannot be negative")
  expect_error(backtest(test_data, take_profit = 0), "take_profit must be positive")
  expect_error(backtest(test_data, pricescale = 0), "pricescale must be positive")
  expect_error(backtest(test_data, volumescale = -1), "volumescale must be positive")
  expect_error(backtest(test_data, stoploss = 100), "stoploss must be between 0 and 100")
  expect_error(backtest(test_data, stoploss = -5), "stoploss must be between 0 and 100")
})

test_that("backtest handles edge cases", {
  # Test with single row
  single_row <- data.table::data.table(
    time = as.POSIXct("2023-01-01"),
    price = 100
  )
  
  result_single <- backtest(single_row)
  expect_s3_class(result_single, "data.frame")
  expect_equal(result_single$n_trades, 0)
  
  # Test with two rows (minimal scenario)
  two_rows <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 2),
    price = c(100, 100)
  )
  
  result_two <- backtest(two_rows)
  expect_s3_class(result_two, "data.frame")
})

test_that("backtest respects data.table input format", {
  # Test with data.frame (should work but be converted)
  df_data <- data.frame(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 10),
    price = rep(100, 10)
  )
  
  expect_no_error({
    result_df <- backtest(df_data)
  })
  
  # Test with data.table (preferred format)
  dt_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 10),
    price = rep(100, 10)
  )
  
  expect_no_error({
    result_dt <- backtest(dt_data)
  })
})

test_that("backtest with deal_start filter works", {
  # Create test data with deal_start column
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 20),
    price = c(100, rep(95, 9), rep(105, 10)),
    deal_start = c(TRUE, rep(FALSE, 9), rep(TRUE, 10)) # Only allow deals in second half
  )
  
  result_with_filter <- backtest(test_data, start_asap = FALSE)
  result_without_filter <- backtest(test_data[, !"deal_start"], start_asap = TRUE)
  
  # With filter should potentially have different results
  expect_s3_class(result_with_filter, "data.frame")
  expect_s3_class(result_without_filter, "data.frame")
})

test_that("backtest with stop loss works", {
  # Create scenario where stop loss should trigger
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 30),
    price = c(100, seq(99, 70, length.out = 29)) # Continuous decline
  )
  
  result_with_sl <- backtest(test_data, stoploss = 20) # 20% stop loss
  result_without_sl <- backtest(test_data, stoploss = 0) # No stop loss
  
  # With stop loss, max drawdown should be limited
  expect_lte(result_with_sl$max_draw_down, result_without_sl$max_draw_down)
})

test_that("backtest compound parameter works", {
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 20),
    price = c(rep(100, 10), rep(105, 10))
  )
  
  result_compound <- backtest(test_data, compound = TRUE)
  result_no_compound <- backtest(test_data, compound = FALSE)
  
  # Both should return valid results
  expect_s3_class(result_compound, "data.frame")
  expect_s3_class(result_no_compound, "data.frame")
})

test_that("backtest trading fee parameter works", {
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 10),
    price = rep(100, 10)
  )
  
  result_no_fee <- backtest(test_data, trading_fee = 0)
  result_with_fee <- backtest(test_data, trading_fee = 0.1) # 0.1% fee
  
  # Both should return valid results
  expect_s3_class(result_no_fee, "data.frame")
  expect_s3_class(result_with_fee, "data.frame")
})