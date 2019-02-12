\name{theta.classic}
\alias{theta.classic}
\title{
Forecasts using the Theta method
}
\description{
Returns forecasts and other information for the Theta method's forecasts applied to x.
}
\usage{
theta.classic(x, fh)
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
  \item{mean}{
point forecasts as a time series
}
  \item{fitted}{
fitted values
}
  \item{mean0}{
point forecasts of the zero Theta line
}
  \item{fitted0}{
fitted values of the zero Theta line
}
  \item{mean2}{
point forecasts of the other Theta line
}
  \item{fitted2}{
fitted values of the other Theta line
}
}
\references{
Assimakopoulos, V. and Nikolopoulos, K. (2000) \emph{The theta model: a decomposition
approach to forecasting}. International Journal of Forecasting 16, 521–530.
}
\seealso{
\code{\link[fsu]{four.theta}}, \code{\link[fsu]{theta.BoxCox}}.
}
\examples{
ts<-rnorm(100, mean=50, sd=10)
theta.classic(ts,10)$mean
}
