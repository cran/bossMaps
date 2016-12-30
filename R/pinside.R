#' Calculate the probability inside an expert range given a logistic decay
#'
#' @param dists frequency table of unique distance values (output from running \code{\link[raster]{freq}} on the \code{rdist} object).
#' @param parms A named vector of parameter values from \code{\link{logistic}} that describe the desired curve.
#' These include all parameters for \code{\link{logistic}} (\code{rate,skew,shift}).
#' Optionally, you can add a parameter \code{buffer} which adds a buffer around the range as 'inside.'  
#' @return Scalar of the estimated probability inside the range given the specified logistic decay (with optional buffer).  
#' @description  Uses the \code{\link{logistic}} function to estimate the probability inside the range
#' @export

pinside=function(dists,parms){
  pred=logistic(dists[,"value"],parms=parms) 
  spred=pred*dists[,"count"]  #multiply transform by number of pixels in each class
  npred=spred/sum(spred) #normalize
  ## If fitdist buffer was not included in parms, set it to zero
  if(is.na(parms["buffer"])) parms["buffer"]=0
  ## set in/out boundary using the "buffer" parameter
  inside=dists[,"value"]<=parms[["buffer"]]  # identify pixels inside range
  pin= sum(npred[inside],na.rm=T)   # calculate total % inside range  
  return(pin)
}
