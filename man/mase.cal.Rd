\name{mase.cal}
\alias{mase.cal}
\title{
Error measure
}
\description{
Rerutns the Mean Absolute Scaled Error
}
\usage{
mase.cal(insample, outsample, forecasts)
}
\arguments{
  \item{insample}{
a numeric vector or time series of actual values used to produce forecasts
}
 \item{outsample}{
a numeric vector or time series of actual values
}
 \item{forecasts}{
a numeric vector or time series of forcasts
}
}
\value{
mase error for each forcast point
}
\references{
Hyndman R.J., Koehler A.B. (2006). \emph{Another look at measures of forecast accuracy}. International Journal of Forecasting, 22, pp.679-688.
}
\seealso{
\code{\link[fsu]{smape.cal}}, \code{\link[fsu]{theta.classic}}.
}
\examples{
mase.cal(c(1:10),c(10,10),c(11,12))
}
