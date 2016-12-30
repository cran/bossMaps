#' Apply a custom logisitic transform to a vector
#'
#' @param x \code{vector} or \code{raster*} object to apply logistic transform.
#' @param lower The lower asymptote
#' @param upper The upper asymptote
#' @param rate The rate of decay.  
#' A rate of 0 indicates no decay, while large rates (>10) become like step-functions.
#' @param skew Affects near which asymptote maximum rate (\code{skew} must be >0).  
#' If skew=1, the curve becomes a standard symetrical logistic centered at \code{m}.    
#' As skew approaches zero, it becomes more asymetrical.    
#' @param shift The time of maximum decay. Slides the curve left-right. 
#' @param parms an optional list with each of the following named parameters
#' (e.g. \code{list(lower=0.01,upper=1,rate=0.04,skew=0.2,shift=0)}).
#' Use of \code{parms} rather than naming each parameter separately is for
#' convenience with the \code{\link{optim}} function.
#' @description  Performs a generalized logistic transform (aka Richard's curve) as follows:
#'  \deqn{Y(t) = upper - { upper-lower \over (1 + e^{-rate(x - shift)}) ^ {1 / skew} }}{y=upper-(upper-lower)/((1+exp(-rate*(x-shift)))^(1/skew))}
#' @examples curve(logistic(x,rate=0.05,skew=0.5),-150,300)
#' @references Richards, F. J. (1959). 'A Flexible Growth Function for Empirical Use'. 
#' Journal of Experimental Botany 10 (2): 290-300.
#' @export

logistic = function(x, lower = 0, upper = 1, rate = 0.04, skew = 0.2, 
  shift = 0, parms = NULL) {
  if (is.null(parms)) 
    parms = list(lower = lower, upper = upper, rate = rate, 
      skew = skew, shift = shift)
  ## check values 
  if (!is.numeric(unlist(parms))) 
    stop("All parameters must be numeric")
  if (parms[["skew"]] <= 0) 
    stop("skew must be greater than 0")
  ## fit curve
  y = parms[["upper"]] - ((parms[["upper"]] - parms[["lower"]])/((1 + 
    exp(-parms[["rate"]] * (x - parms[["shift"]])))^(1/parms[["skew"]])))
  return(y)
}