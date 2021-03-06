four.theta<- function(x, fh){
  #Used to automatically select the best Theta model
  #Check inputs
  if (any(x<0 | fh<0)){stop("Initial inputs cannot be negative")}
  #Scale
  base <- mean(x) ; x <- x/base

  molist <- c("M","A") ; trlist <- c("Lrl","Exp")

  theta.fit <- function(x, fh, theta, curve, model, seasonality , plot=FALSE){
    #Used to fit a Theta model

    #Check if the inputs are valid
    if (theta<0){ theta <- 2  }
    if (fh<1){ fh <- 1  }
    #Estimate theta line weights
    outtest <- naive(x, h=fh)$mean
    if (theta==0){
      wses <- 0
    }else{
      wses <- (1/theta)
    }
    wlrl <- (1-wses)
    #Estimate seasonaly adjusted time series
    ppy <- frequency(x)
    if (seasonality=="N"){
      des_input <- x ; SIout <- rep(1, fh) ; SIin <- rep(1, length(x))
    }else if (seasonality=="A"){
      Dec <- decompose(x, type="additive")
      des_input <- x-Dec$seasonal
      SIin <- Dec$seasonal
      SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
    }else{
      Dec <- decompose(x, type="multiplicative")
      des_input <- x/Dec$seasonal
      SIin <- Dec$seasonal
      SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
    }

    #If negative values, force to linear model
    if (min(des_input)<=0){ curve <- "Lrl" ; model <- "A"  }
    #Estimate theta line zero
    observations <- length(des_input)
    xs <- c(1:observations)
    xf = xff <- c((observations+1):(observations+fh))
    dat=data.frame(des_input=des_input, xs=xs)
    newdf <- data.frame(xs = xff)

    if (curve=="Exp"){
      estimate <- lm(log(des_input)~xs)
      thetaline0In <- exp(predict(estimate))+x-x
      thetaline0Out <- exp(predict(estimate, newdf))+outtest-outtest
    }else{
      estimate <- lm(des_input ~ poly(xs, 1, raw=TRUE))
      thetaline0In <- predict(estimate)+des_input-des_input
      thetaline0Out <- predict(estimate, newdf)+outtest-outtest
    }

    #Estimete Theta line (theta)
    if (model=="A"){
      thetalineT <- theta*des_input+(1-theta)*thetaline0In
    }else if ((model=="M")&(all(thetaline0In>0)==T)&(all(thetaline0Out>0)==T)){
      thetalineT <- (des_input^theta)*(thetaline0In^(1-theta))
    }else{
      model<-"A"
      thetalineT <- theta*des_input+(1-theta)*thetaline0In
    }

    #forecasting TL2
    sesmodel <- ses(thetalineT, h=fh)
    thetaline2In <- sesmodel$fitted
    thetaline2Out <- sesmodel$mean

    #Theta forecasts
    if (model=="A"){
      forecastsIn <- as.numeric(thetaline2In*wses)+as.numeric(thetaline0In*wlrl)+des_input-des_input
      forecastsOut <- as.numeric(thetaline2Out*wses)+as.numeric(thetaline0Out*wlrl)+outtest-outtest
    }else if ((model=="M")&
              (all(thetaline2In>0)==T)&(all(thetaline2Out>0)==T)&
              (all(thetaline0In>0)==T)&(all(thetaline0Out>0)==T)){
      forecastsIn <- ((as.numeric(thetaline2In)^(1/theta))*(as.numeric(thetaline0In)^(1-(1/theta))))+des_input-des_input
      forecastsOut <- ((as.numeric(thetaline2Out)^(1/theta))*(as.numeric(thetaline0Out)^(1-(1/theta))))+outtest-outtest
    }else{
      model<-"A"
      thetalineT <- theta*des_input+(1-theta)*thetaline0In
      sesmodel <- ses(thetalineT,h=fh)
      thetaline2In <- sesmodel$fitted
      thetaline2Out <- sesmodel$mean
      forecastsIn <- as.numeric(thetaline2In*wses)+as.numeric(thetaline0In*wlrl)+des_input-des_input
      forecastsOut <- as.numeric(thetaline2Out*wses)+as.numeric(thetaline0Out*wlrl)+outtest-outtest
    }

    #Seasonal adjustments
    if (seasonality=="A"){
      forecastsIn <- forecastsIn+SIin
      forecastsOut <- forecastsOut+SIout
    }else{
      forecastsIn <- forecastsIn*SIin
      forecastsOut <- forecastsOut*SIout
    }

    #Zero forecasts become positive
    for (i in 1:length(forecastsOut)){
      if (forecastsOut[i]<0){ forecastsOut[i] <- 0 }
    }

    if (plot==TRUE){
      united <- cbind(x,forecastsOut)
      for (ik in 1:(observations+fh)){ united[ik,1] = sum(united[ik,2],united[ik,1], na.rm = TRUE) }
      plot(united[,1],col="black",type="l",main=paste("Model:",model,",Curve:",curve,",Theta:",theta),xlab="Time",ylab="Values",
           ylim=c(min(united[,1])*0.85,max(united[,1])*1.15))
      lines(forecastsIn, col="green") ; lines(forecastsOut, col="green")
      lines(thetaline2In, col="blue") ; lines(thetaline2Out, col="blue")
      lines(thetaline0In, col="red") ; lines(thetaline0Out, col="red")
    }

    output=list(fitted=forecastsIn,mean=forecastsOut,
                fitted0=thetaline0In,mean0=thetaline0Out,
                fitted2=thetaline2In,mean2=thetaline2Out,
                model=paste(seasonality,model,curve,c(round(theta,2))))

    return(output)
  }

  #Check seasonality & Create list of models
  ppy <- frequency(x) ; ST <- F
  if (ppy>1){ ST <- seasonality.test(x, ppy) }
  if (ST==T){

    selist <- c("M","A")
    listnames <- c()
    for (i in 1:length(selist)){
      for (ii in 1:length(molist)){
        for (iii in 1:length(trlist)){
          listnames <- c(listnames,paste(selist[i], molist[ii], trlist[iii]))
        }
      }
    }

  }else{

    listnames <- c()
    for (ii in 1:length(molist)){
      for (iii in 1:length(trlist)){
        listnames <- c(listnames, paste("N", molist[ii], trlist[iii]))
      }
    }

  }

  modellist <- NULL
  for (i in 1:length(listnames)){
    modellist[length(modellist)+1] <- list(c(substr(listnames,1,1)[i], substr(listnames,3,3)[i],
                                             substr(listnames,5,7)[i]))
  }

  #Start validation
  errorsin <- c() ; models <- NULL

  #With this function determine opt theta per case
  optfun <- function(y, x, fh, curve, model, seasonality){
    mean(abs(theta.fit(x=x, fh, theta=y, curve, model, seasonality , plot=FALSE)$fitted-x))
  }

  for (j in 1:length(listnames)){
    optTheta <- optimize(optfun, c(1:3),
                         x=x, fh=fh, curve=modellist[[j]][3], model=modellist[[j]][2],
                         seasonality=modellist[[j]][1])$minimum

    fortheta <- theta.fit(x=x, fh=fh, theta=optTheta, curve=modellist[[j]][3], model=modellist[[j]][2],
                          seasonality=modellist[[j]][1], plot=F)
    models[length(models)+1] <- list(fortheta)
    errorsin <- c(errorsin, mean(abs(x-fortheta$fitted)))
  }

  #Select model and export
  selected.model <- models[[which.min(errorsin)]]
  description <- selected.model$model
  output <- list(fitted=selected.model$fitted*base,mean=selected.model$mean*base,
                 description=description)
  #Returns the fitted and forecasted values, as well as the model used (Type of seasonality, Type of Model, Type of Trend, Theta coef.)

  return(output)

}
