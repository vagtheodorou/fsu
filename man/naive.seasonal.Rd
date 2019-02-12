\name{naive.seasonal}
\alias{naive.seasonal}
\title{
Forecasts using the seasonal naive method
}
\description{
Returns forecasts for the seasonal naive method's forecasts applied to x.
}
\usage{
naive.seasonal(x, fh)
}
\arguments{
  \item{x}{
a numeric vector or time series
}
  \item{fh}{
number of periods for forecasting
}
}
\value{
point forecasts as a time series

}
\seealso{
\code{\link[fsu]{theta.classic}}, \code{\link[fsu]{four.theta}}.
}
\examples{
ts<-rnorm(100, mean=50, sd=10)
naive.seasonal(ts,10)
}
