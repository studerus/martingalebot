% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_sma_filter.R
\name{add_sma_filter}
\alias{add_sma_filter}
\title{Add an SMA-based deal start filter to a data.table}
\usage{
add_sma_filter(dt, time_period = "1 day", n = 100)
}
\arguments{
\item{dt}{A \code{data.table} containing 'time' and 'price' columns. The function
assumes the data is already sorted by time.}

\item{time_period}{The time frame for resampling. Defaults to \code{"1 day"}, which
is standard for trend-following strategies.}

\item{n}{The number of periods for the SMA lookback (e.g., 100 days).}
}
\value{
The \code{data.table} \code{dt} is returned after being modified in place,
allowing for use in a \code{magrittr} pipeline.
}
\description{
This function calculates a Simple Moving Average (SMA) on a resampled time
series and creates a logical \code{deal_start} column. This column is \code{TRUE}
whenever the current price is above the SMA of the previous period, indicating
an uptrend. It is designed to be used as a trend filter.

The function modifies the input \code{data.table} by reference for maximum memory
efficiency.
}
\examples{
\dontrun{
# Download data and add the SMA filter in a single pipeline.
# For a short 1-month period, a shorter SMA (e.g., 10) is more illustrative.
dat <- get_binance_prices_from_csv(
  'PYRUSDT',
  start_time = '2023-02-01',
  end_time = '2023-02-28',
  progressbar = FALSE
) |>
  add_sma_filter(n = 10)

# Perform backtesting using the new 'deal_start' column
backtest(data = dat, start_asap = FALSE)
}
}
\seealso{
\code{\link{add_rsi_filter}}, \code{\link{add_bollinger_filter}}, \code{\link{add_macd_filter}}
}
