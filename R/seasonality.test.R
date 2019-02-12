seasonality.test <- function(x, ppy){
  #Used for determining whether the time series is seasonal
  #Check inputs
  if (any(x<0 | ppy<0)){stop("Initial inputs cannot be negative")}
  tcrit <- 1.645
  if (length(x)<3*ppy){
    test_seasonal <- FALSE
  }else{
    xacf <- acf(x, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit/sqrt(length(x)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )

    if (is.na(test_seasonal)==TRUE){ test_seasonal <- FALSE }
  }

  return(test_seasonal)
}
