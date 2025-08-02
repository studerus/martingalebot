test_that("grid_search works correctly", {
  # Create simple test data
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 100),
    price = c(rep(100, 20), rep(95, 20), rep(105, 30), rep(95, 15), rep(105, 15))
  )
  
  # Simple grid search with 2x2 grid, no time periods (just defaults)
  result <- grid_search(
    test_data,
    base_order_volume = c(10, 20),
    take_profit = c(2, 3),
    n_timeperiods = 1,
    progressbar = FALSE
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_gte(nrow(result), 4) # At least 4 combinations (might be more due to defaults)
  expect_true("profit" %in% names(result))
  expect_true("base_order_volume" %in% names(result))
  expect_true("take_profit" %in% names(result))
  
  # Check that all results are numeric
  expect_type(result$profit, "double")
  expect_type(result$max_draw_down, "double")
})

test_that("grid_search with multiple time periods works", {
  # Create longer test data for time slicing
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 2000),
    price = rep(c(100, 95, 105), length.out = 2000)
  )
  
  result <- grid_search(
    test_data,
    base_order_volume = c(10, 20),
    take_profit = c(2, 3),
    n_timeperiods = 3,
    progressbar = FALSE
  )
  
  # Should have results for multiple time periods
  expect_gte(nrow(result), 4)
  # Check that we have all essential columns
  expect_true("profit" %in% names(result))
})

test_that("create_timeslices works correctly", {
  # Create test data spanning multiple months
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 3000),
    price = rep(100, 3000)
  )
  
  slices <- create_timeslices(test_data, train_months = 2, test_months = 1, shift_months = 1)
  
  # Check structure - create_timeslices returns a tibble with time ranges
  expect_s3_class(slices, "data.frame")
  expect_gt(nrow(slices), 0)
  
  # Each row should have time range columns
  expect_true(all(c("period", "start_train", "end_train", "start_test", "end_test") %in% names(slices)))
  
  # Check that times are POSIXct
  expect_s3_class(slices$start_train, "POSIXct")
  expect_s3_class(slices$end_train, "POSIXct")
})

test_that("required_capital works correctly", {
  result1 <- required_capital(
    base_order_volume = 100,
    first_safety_order_volume = 100,
    n_safety_orders = 5,
    volumescale = 2.0
  )
  
  expect_type(result1, "double")
  expect_gt(result1, 100) # Should be more than just base order
  
  # Test with different parameters
  result2 <- required_capital(
    base_order_volume = 100,
    first_safety_order_volume = 100,
    n_safety_orders = 10,
    volumescale = 2.0
  )
  
  expect_gt(result2, result1) # More safety orders = more capital
})

test_that("covered_deviation works correctly", {
  result1 <- covered_deviation(
    n_safety_orders = 5,
    pricescale = 2.0,
    stepscale = 1.0
  )
  
  expect_type(result1, "double")
  expect_gt(result1, 0)
  
  # Test with different parameters
  result2 <- covered_deviation(
    n_safety_orders = 10,
    pricescale = 2.0,
    stepscale = 1.0
  )
  
  expect_gt(result2, result1) # More safety orders = more deviation covered
})

test_that("down_tolerance works correctly", {
  result1 <- down_tolerance(
    base_order_volume = 100,
    first_safety_order_volume = 100,
    n_safety_orders = 5,
    pricescale = 2.0,
    volumescale = 2.0,
    take_profit = 3.0,
    stepscale = 1.0
  )
  
  expect_type(result1, "double")
  expect_gt(result1, 0)
  
  # Test edge case with no safety orders
  result2 <- down_tolerance(
    base_order_volume = 100,
    first_safety_order_volume = 100,
    n_safety_orders = 0,
    pricescale = 2.0,
    volumescale = 2.0,
    take_profit = 3.0,
    stepscale = 1.0
  )
  
  expect_lt(result2, 0) # No safety orders means negative down tolerance
})

test_that("utility functions work with pipe operator", {
  # Test that grid_search works with pipe
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 100),
    price = rep(100, 100)
  )
  
  expect_no_error({
    result <- test_data |>
      grid_search(base_order_volume = c(10, 20), take_profit = c(2, 3))
  })
  
  expect_s3_class(result, "data.frame")
  
  # Test create_timeslices with pipe
  expect_no_error({
    slices <- test_data |>
      create_timeslices(train_months = 1, test_months = 1)
  })
  
  expect_type(slices, "list")
})