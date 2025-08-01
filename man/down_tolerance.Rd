% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/down_tolerance.R
\name{down_tolerance}
\alias{down_tolerance}
\title{Calculate the "price down tolerance" for a specific parameter setting}
\usage{
down_tolerance(
  base_order_volume,
  first_safety_order_volume,
  n_safety_orders,
  pricescale,
  volumescale,
  take_profit,
  stepscale
)
}
\arguments{
\item{base_order_volume}{The size of the base order (in the quote currency)}

\item{first_safety_order_volume}{The size of the first safety order (in
the quote currency)}

\item{n_safety_orders}{The maximum number of safety orders}

\item{pricescale}{Price deviation to open safety orders (\% from initial
order)}

\item{volumescale}{With what number should the funds used by the last safety
order be multiplied?}

\item{take_profit}{At what percentage in profit should the bot close the
deal?}

\item{stepscale}{With what number should the price deviation percentage used
by the last safety order be multiplied?}
}
\value{
a numeric vector of price down tolerances
}
\description{
This function calculates the "price down tolerance" (i.e. the percentage
price deviation from the initial order price to the take profit price when
all safety orders are filled) for a specific parameter setting. This function
is vectorized. So, you can also apply it to vectors of parameters.
}
\examples{
down_tolerance(base_order_volume = 10, first_safety_order_volume = 10,
               n_safety_orders = 8, pricescale = 1.5, volumescale = 1.5,
               take_profit = 3, stepscale = 1.2)
}
