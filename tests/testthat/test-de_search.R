context("Differential Evolution Search Functionality")

test_that("de_search returns correct structure and respects bounds", {
  # Create simple, short test data
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 50),
    price = 100 + cumsum(rnorm(50))
  )
  
  # Define tight bounds for testing
  n_safety_orders_bounds <- c(5, 10)
  pricescale_bounds <- c(1.0, 2.0)
  stoploss_bounds <- c(20, 30)
  
  # A very short optimization run for testing purposes
  # We expect a warning about NP size, which is fine for a quick test
  expect_warning(
    result <- de_search(
      data = test_data,
      n_safety_orders_bounds = n_safety_orders_bounds,
      pricescale_bounds = pricescale_bounds,
      stoploss_bounds = stoploss_bounds,
      DEoptim_control = list(itermax = 2, NP = 10, trace = FALSE),
      start_asap = TRUE # Pass fixed param
    ),
    "it is best to set 'NP'"
  )
  
  # 1. Check return type and structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  
  expected_names <- c("profit", "max_draw_down", "n_safety_orders", "pricescale", "stoploss")
  expect_true(all(expected_names %in% names(result)))
  
  # 2. Check that the best found parameters are within the specified bounds
  expect_gte(result$n_safety_orders, n_safety_orders_bounds[1])
  expect_lte(result$n_safety_orders, n_safety_orders_bounds[2])
  
  expect_gte(result$pricescale, pricescale_bounds[1])
  expect_lte(result$pricescale, pricescale_bounds[2])
  
  expect_gte(result$stoploss, stoploss_bounds[1])
  expect_lte(result$stoploss, stoploss_bounds[2])
})

test_that("de_search works with a complex objective_metric", {
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 50),
    price = 100 + cumsum(rnorm(50))
  )
  
  # Test with a custom risk-adjusted metric
  # A minimal run to ensure the function executes without error
  expect_warning(
    expect_no_error(
      de_search(
        data = test_data,
        objective_metric = "profit / sqrt(max_draw_down + 1)", # Add 1 to avoid division by zero
        DEoptim_control = list(itermax = 1, NP = 10, trace = FALSE)
      )
    ),
    "it is best to set 'NP'"
  )
})
