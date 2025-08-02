context("Random Search Functionality")

test_that("random_search returns correct structure and respects parameters", {
  # Create simple, short test data to make the test run fast
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 50),
    price = 100 + cumsum(rnorm(50))
  )
  
  # Define parameters for the search
  n_samples <- 10
  n_safety_orders_bounds <- c(5, 10)
  pricescale_bounds <- c(1.0, 2.0)
  stoploss_values <- c(0, 20, 30)
  base_order_volume_bounds <- c(15, 15) # Fixed value
  
  # Run the random search
  result <- random_search(
    data = test_data,
    n_samples = n_samples,
    n_safety_orders_bounds = n_safety_orders_bounds,
    pricescale_bounds = pricescale_bounds,
    stoploss_values = stoploss_values,
    base_order_volume_bounds = base_order_volume_bounds,
    progressbar = FALSE,
    processing = "sequential"
  )
  
  # 1. Check return type and basic structure
  expect_s3_class(result, "tbl_df")
  
  # It's possible filters remove all samples, so only test columns if rows exist
  if (nrow(result) > 0) {
    expected_names <- c("profit", "max_draw_down", "n_trades", "n_stoploss", 
                        "n_safety_orders", "pricescale", "volumescale", 
                        "take_profit", "stepscale", "stoploss", "base_order_volume",
                        "first_safety_order_volume", "start_asap", "compound")
    expect_true(all(expected_names %in% names(result)))
    
    # 2. Check that parameter bounds are respected
    expect_true(all(result$n_safety_orders >= n_safety_orders_bounds[1] & result$n_safety_orders <= n_safety_orders_bounds[2]))
    expect_true(all(result$pricescale >= pricescale_bounds[1] & result$pricescale <= pricescale_bounds[2]))
    expect_true(all(result$base_order_volume == base_order_volume_bounds[1]))
    
    # 3. Check that categorical values are from the provided set
    expect_true(all(result$stoploss %in% stoploss_values))
  }
  
  # 4. Check if the number of results is plausible
  # The number of rows should be less than or equal to n_samples due to filtering
  expect_lte(nrow(result), n_samples)
})

test_that("random_search runs without error with minimal samples", {
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 20),
    price = rep(100, 20)
  )
  
  # A minimal run to ensure the function executes completely
  expect_no_error(
    random_search(
      data = test_data, 
      n_samples = 2, 
      progressbar = FALSE,
      min_covered_deviation = 0, # Relax filters to ensure results
      min_down_tolerance = 0
    )
  )
})

test_that("random_search returns an empty tibble when no parameters match", {
  test_data <- data.table::data.table(
    time = seq(as.POSIXct("2023-01-01"), by = "1 hour", length.out = 20),
    price = rep(100, 20)
  )
  
  # Use impossible constraints to force zero valid combinations
  # We expect a warning that no combinations were found
  expect_warning(
    result <- random_search(
      data = test_data, 
      n_samples = 10, 
      progressbar = FALSE,
      min_covered_deviation = 9999 # Impossible to meet
    ),
    "No parameter combinations met the specified constraints"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
