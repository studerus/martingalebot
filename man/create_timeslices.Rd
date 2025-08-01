% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_timeslices.R
\name{create_timeslices}
\alias{create_timeslices}
\title{Create time slices for cross validation}
\usage{
create_timeslices(data, train_months = 4, test_months = 4, shift_months = 2)
}
\arguments{
\item{data}{A \code{data.table} containing at least a \code{time} column.}

\item{train_months}{The number of months training data sets cover}

\item{test_months}{The number of months test data sets cover}

\item{shift_months}{The number of months pairs of test and training data sets
are shifted to each other. The smaller this number the more pairs of test
and training data sets can be created.}
}
\value{
A \code{tibble} of time slices. One row corresponds to one iteration of the outer
loop in the cross validation.
}
\description{
This function takes a \code{data.frame} of price data and creates start and end
times for test and training data sets, which can later be used for cross
validation. A training time period is always immediately followed by testing
time period.
}
\examples{
get_binance_prices_from_csv(
  "PYRUSDT",
   start_time = "2022-01-01",
   progressbar = F
 ) |>
 create_timeslices(train_months = 4, test_months = 4, shift_months = 1)
}
