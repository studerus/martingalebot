#' Create time slices for cross validation
#'
#' This function takes a `data.frame` of price data and creates start and end
#' times for test and training data sets, which can later be used for cross
#' validation. A training time period is always immediately followed by testing
#' time period.
#'
#' @param train_months The number of months training data sets cover
#' @param test_months  The number of months test data sets cover
#' @param shift_months The number of months pairs of test and training data sets
#'   are shifted to each other. The smaller this number the more pairs of test
#'   and training data sets can be created.
#' @param data
#'
#' @return
#' A `tibble` of time slices. One row corresponds to one iteration of the outer
#' loop in the cross validation.
#' @export
#'
#' @examples
#' dat <- get_binance_prices_from_csv("PYRUSDT", start_time = "2022-01-01",
#'                                    progressbar = F)
#' create_timeslices(train_months = 4, test_months = 4, shift_months = 1,
#'                   data = dat)
create_timeslices <- function(train_months = 4, test_months = 4,
                              shift_months = 2, data) {
  tbl <- tibble::tibble(period = 1,
                start_train = min(data$time),
                end_train = start_train + lubridate::dmonths(train_months),
                start_test = end_train,
                end_test = end_train + lubridate::dmonths(test_months))
  i <- 2
  end <-  max(data$time)
  while ((max(tbl$end_test) + lubridate::dmonths(shift_months)) < end) {
    tbl[i, 1] <- i
    for (j in 2:5)
      tbl[i, j] <- tbl[i - 1, j] + lubridate::dmonths(shift_months)
    i = i + 1
  }
  tbl
}
