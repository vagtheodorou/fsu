\name{theta.BoxCox}
\alias{theta.BoxCox}
\title{
Forecasts using the BoxCox Theta method
}
\description{
Returns forecasts for a transformation of the Theta method's forecasts applied to x.
}
\usage{
theta.BoxCox(x, fh)
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
Based on Assimakopoulos, V. and Nikolopoulos, K. (2000), Legaki, N.Z. and Koutsouri, A. developed a framework to boost Theta's performance. This includes deseasonalizing the data, applying Box-Cox Transformation, generating forecast based on the Theta method, applying Reverse Box-Cox Transformation and Re-seasonalizing the data.
}
\value{
 \item{mean}{
point forecasts as a time series
}
}
\seealso{
\code{\link[fsu]{theta.classic}}, \code{\link[fsu]{four.theta}}.
}
\examples{
ts<-rnorm(100, mean=50, sd=10)
theta.BoxCox(ts,10)$mean
}
