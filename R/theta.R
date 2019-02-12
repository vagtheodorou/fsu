theta.classic <- function(x, fh){
  #Used to estimate Theta classic
  #Check inputs
  if (any(x<0 | fh<0)){stop("Initial inputs cannot be negative")}
  #Set parameters
  wses <- wlrl<-0.5 ; theta <- 2
  #Estimate theta line (0)
  observations <- length(x)
  xt <- c(1:observations)
  xf <- c((observations+1):(observations+fh))
  train <- data.frame(x=x, xt=xt)
  test <- data.frame(xt = xf)

  estimate <- lm(x ~ poly(xt, 1, raw=TRUE))
  thetaline0In <- as.numeric(predict(estimate))
  thetaline0Out <- as.numeric(predict(estimate,test))

  #Estimate theta line (2)
  thetalineT <- theta*x+(1-theta)*thetaline0In
  sesmodel <- ses(thetalineT, h=fh)
  thetaline2In <- sesmodel$fitted
  thetaline2Out <- sesmodel$mean

  #Theta forecasts
  forecastsIn <- (thetaline2In*wses)+(thetaline0In*wlrl)
  forecastsOut <- (thetaline2Out*wses)+(thetaline0Out*wlrl)

  #Zero forecasts become positive
  for (i in 1:length(forecastsOut)){
    if (forecastsOut[i]<0){ forecastsOut[i]<-0 }
  }

  output=list(fitted = forecastsIn, mean = forecastsOut,
              fitted0 = thetaline0In, mean0 = thetaline0Out,
              fitted2 = thetaline2In, mean2 = thetaline2Out)

  return(output)
}
