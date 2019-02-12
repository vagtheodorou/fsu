naive.seasonal <- function(x, fh){
  #Used to estimate Seasonal Naive
  frcy <- frequency(x)
  frcst <- naive(x, h=fh)$mean
  if (frcy>1){
    frcst <- head(rep(as.numeric(tail(x,frcy)), fh), fh) + frcst - frcst
  }
  return(frcst)
}
