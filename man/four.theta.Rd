\name{four.theta}
\alias{four.theta}
\title{
Forecasts using the 4Theta method
}
\description{
Returns forecasts and other information for the 4Theta method's forecasts applied to x.
}
\usage{
four.theta(x, fh)
}
\arguments{
   \item{x}{
a numeric vector or time series
}
  \item{fh}{
number of periods for forecasting
}
}
\details{
Based on Assimakopoulos, V. and Nikolopoulos, K. (2000), Spiliotis, E. examined modifications on the decomposition framework of Theta to boost its performance. This includes considering non-linear patterns of trend, adjusting trend intensity and introducing a multiplicative expression of the method. The extensions proposed transform Theta into a generalized forecasting algorithm for automatic extrapolation with enhanced flexibility and improved properties compared to its classical form.
}
\value{
\item{mean}{
point forecasts as a time series
}
  \item{fitted}{
fitted values
}
 \item{description}{
 ...
 }
}
\seealso{
\code{\link[fsu]{theta.classic}}, \code{\link[fsu]{theta.BoxCox}}.
}
\examples{
ts<-rnorm(100, mean=50, sd=10)
four.theta(ts,10)$mean
}
