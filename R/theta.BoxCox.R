theta.BoxCox<-function(x, fh){
  #Check inputs
  if (any(x<0 | fh<0)){stop("Initial inputs cannot be negative")}
  ppy <- frequency(x) ; ST <- F
  if (ppy>1){
    ST <- seasonality.test(x,ppy)
  }
  if (ST==T){
    Dec <- decompose(x, type="multiplicative")
    des_input <- x/Dec$seasonal
    SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
  }else{
    des_input <- x ; SIout <- rep(1, fh)
  }

  # Do box cox for the best l
  lambda<-BoxCox.lambda(des_input, method="loglik", lower=0, upper=1)
  data.bxcx <- BoxCox(des_input, lambda)

  # Forecast
  data.forecast <- thetaf(data.bxcx, h=fh)

  # Inv boxcox the same as x actually
  data.forecast.inv<-InvBoxCox(data.forecast$mean, lambda)

  #multiply with the respective indices
  if (ST==T){
    data.forecast.ses<-data.forecast.inv*SIout
  }else{
    data.forecast.ses <- data.forecast.inv
  }

  output <- list(mean=data.forecast.ses)
  return(output)
}
