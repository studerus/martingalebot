test_that("add_sma_filter works correctly", {
  # Create test data with predictable price pattern
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 72), # 3 days
    price = c(rep(100, 24), rep(110, 24), rep(90, 24)) # Day 1: 100, Day 2: 110, Day 3: 90
  )
  
  result <- add_sma_filter(test_data, time_period = "1 day", n = 2)
  
  # Check that deal_start column was added
  expect_true("deal_start" %in% names(result))
  expect_type(result$deal_start, "logical")
  
  # Check that result is still a data.table
  expect_s3_class(result, "data.table")
  
  # Check that the original data structure is preserved
  expect_equal(nrow(result), nrow(test_data))
  expect_true(all(c("time", "price") %in% names(result)))
})

test_that("add_sma_filter handles insufficient data", {
  # Create data with only 1 day (insufficient for SMA calculation)
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 12),
    price = rep(100, 12)
  )
  
  result <- add_sma_filter(test_data, time_period = "1 day", n = 5)
  
  # Should add deal_start column with all NA values
  expect_true("deal_start" %in% names(result))
  expect_true(all(is.na(result$deal_start)))
})

test_that("add_rsi_filter works correctly", {
  # Create test data with RSI pattern (declining then stable)
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 48),
    price = c(seq(100, 80, length.out = 24), rep(80, 24)) # Decline then stable
  )
  
  result <- add_rsi_filter(test_data, time_period = "1 hour", n = 7, cutoff = 30)
  
  # Check structure
  expect_true("deal_start" %in% names(result))
  expect_type(result$deal_start, "logical")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), nrow(test_data))
})

test_that("add_rsi_filter handles edge cases", {
  # Test with minimal data
  minimal_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 5),
    price = rep(100, 5)
  )
  
  result <- add_rsi_filter(minimal_data, n = 10) # n larger than data
  
  # Should handle gracefully
  expect_true("deal_start" %in% names(result))
  expect_equal(nrow(result), nrow(minimal_data))
})

test_that("add_bollinger_filter works correctly", {
  # Create test data with volatility pattern
  set.seed(123)
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 72),
    price = 100 + cumsum(rnorm(72, 0, 1)) # Random walk starting at 100
  )
  
  result <- add_bollinger_filter(test_data, time_period = "1 hour", n = 20, sd = 2, cutoff = 0.05)
  
  # Check structure
  expect_true("deal_start" %in% names(result))
  expect_type(result$deal_start, "logical")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), nrow(test_data))
})

test_that("add_macd_filter works correctly", {
  # Create test data with trending pattern
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 100),
    price = 100 + seq(0, 20, length.out = 100) + sin(seq(0, 4*pi, length.out = 100)) * 2
  )
  
  result <- add_macd_filter(test_data, time_period = "1 hour", nFast = 12, nSlow = 26, nSig = 9)
  
  # Check structure
  expect_true("deal_start" %in% names(result))
  expect_type(result$deal_start, "logical")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), nrow(test_data))
})

test_that("filter functions preserve data.table reference semantics", {
  # Test that functions return printable results
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 48),
    price = rep(100, 48)
  )
  
  # All filter functions should return the data.table for printing
  result_sma <- add_sma_filter(data.table::copy(test_data))
  result_rsi <- add_rsi_filter(data.table::copy(test_data))
  result_bollinger <- add_bollinger_filter(data.table::copy(test_data))
  result_macd <- add_macd_filter(data.table::copy(test_data))
  
  # All should be data.tables with deal_start column
  expect_s3_class(result_sma, "data.table")
  expect_s3_class(result_rsi, "data.table")
  expect_s3_class(result_bollinger, "data.table")
  expect_s3_class(result_macd, "data.table")
  
  expect_true("deal_start" %in% names(result_sma))
  expect_true("deal_start" %in% names(result_rsi))
  expect_true("deal_start" %in% names(result_bollinger))
  expect_true("deal_start" %in% names(result_macd))
})

test_that("filter functions handle insufficient data correctly", {
  # Create data with insufficient history for calculations
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 5),
    price = rep(100, 5)
  )
  
  # Filter functions should handle insufficient data gracefully
  expect_no_error({
    add_sma_filter(data.table::copy(test_data), n = 100)
    add_rsi_filter(data.table::copy(test_data), n = 100)
    add_bollinger_filter(data.table::copy(test_data), n = 100)
    add_macd_filter(data.table::copy(test_data), nSlow = 100)
  })
})

test_that("filter functions work with pipe operator", {
  # Test pipe-friendly first argument
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 48),
    price = rep(100, 48)
  )
  
  # Should work with magrittr pipe
  expect_no_error({
    result <- test_data |>
      add_sma_filter() |>
      add_rsi_filter()
  })
  
  expect_true("deal_start" %in% names(result))
  expect_s3_class(result, "data.table")
})