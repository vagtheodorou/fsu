\name{smape.cal}
\alias{smape.cal}
\title{
Error measure
}
\description{
Rerutns the Symmetric Mean Absolute Percentage Error
}
\usage{
smape.cal(outsample, forecasts)
}
\arguments{
  \item{outsample}{
a numeric vector or time series of actual values
}
 \item{forecasts}{
a numeric vector or time series of forcasts
}
}
\value{
smape error for each forcast point
}
\seealso{
\code{\link[fsu]{mase.cal}}, \code{\link[fsu]{theta.classic}}.
}
\examples{
smape.cal(c(10,10),c(11,12))
}
