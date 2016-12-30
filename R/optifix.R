#' optifix. Optimise with some fixed parameters 
#'
#' @description  Like \code{\link[stats]{optim}}, but with option to fix some parameters.
#' @param parms Parameters to potentially optimize in \code{fn}
#' @param fixed A vector of TRUE/FALSE values indicating which parameters in \code{parms} to hold constant (not optimize). If TRUE, the corresponding parameter in fn() is fixed. Otherwise it's variable and optimised over.
#' @param fn Function to optimize (same as in \code{\link[stats]{optim}})
#' @param gr Gradient function (same as in \code{\link[stats]{optim}})
#' @param method Method to use for optimization (same as in \code{\link[stats]{optim}})
#' @param lower Lower limits (same as in \code{\link[stats]{optim}})
#' @param upper Upper limits (same as in \code{\link[stats]{optim}})
#' @param control Control list (same as in \code{\link[stats]{optim}})
#' @param hessian Return Hessian object (same as in \code{\link[stats]{optim}})
#' @param ... Further arguments to be passed to fn and gr.
#' @return Similar to \code{\link[stats]{optim}} but adds a vector of all the parameters and a vector copy of the 'fixed' argument. 
#' @author Written by Barry Rowlingson October 2011 
#' @references \url{http://www.maths.lancs.ac.uk/~rowlings/R/Optifix/optifix.R}
#' @details Originally written by Barry Rowlingson"
    
optifix <- function(parms, fixed, fn, gr = NULL, ...,
                    method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), 
                    lower = -Inf, upper = Inf, control = list(), hessian = FALSE){ 
  force(fn)
  force(fixed) 
  .npar=length(parms) 
  .fixValues = parms[fixed]
  names(.fixValues)=names(parms)[fixed]
  .parStart = parms[!fixed]
  names(.parStart)=names(parms)[!fixed]
  
  .fn <- function(par,pnames=names(parms),...){
    .par = rep(NA,sum(!fixed))
    .par[!fixed] = par
    .par[fixed] = .fixValues
    names(.par)=pnames
    fn(.par,...) }

  if(!is.null(gr)){
    .gr <- function(par,pnames=names(parms),...)
      { .gpar = rep(NA,sum(!fixed))
        .gpar[!fixed] = par
        .gpar[fixed] = .fixValues
        names(.par)=pnames
        gr(.gpar,...)[!fixed] } }
  else{ .gr <- NULL } 

  .opt = stats::optim(.parStart,.fn,.gr,...,method=method,
               lower=lower,upper=upper,control=control,hessian=hessian) 
  
  .opt$fullpars = rep(NA,sum(!fixed)) 
  .opt$fullpars[fixed]=.fixValues 
  .opt$fullpars[!fixed]=.opt$par 
  names(.opt$fullpars)=names(parms)
  .opt$fixed = fixed 
  return(.opt) }
