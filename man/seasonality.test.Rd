\name{seasonality.test}
\alias{seasonality.test}
\title{
Seasonality test of time series
}
\description{
Determines whether or not a time series is seasonal
}
\usage{
seasonality.test(x,ppy)
}
\arguments{
  \item{x}{
a numeric vector or time series
}
\item{ppy}{
...
}
}
\value{
logical vector
}
\seealso{
\code{\link[fsu]{four.theta}}, \code{\link[fsu]{theta.classic}}.
}
\examples{
ts<-rnorm(100, mean=50, sd=10)
seasonality.test(ts,1)
}
