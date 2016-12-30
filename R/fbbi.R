#' Factor Bias Back In (FBBI)
#'
#' @param prior  raster* object of spatial priors
#' @param maxent  raster* object of maxent output
#' @param ... additional functions to be passed to \code{\link[raster]{writeRaster}}
#' @description  Calculate minxent layer using maxent output  (from run with bias layer) and prior (i.e., the bias layer)
#' @export
#' @import raster

fbbi <- function(prior, maxent, ...) {
  ## Multiply the prior and maxent output asciis
  output <- normalize(maxent * prior, ...)
  ## Return the layer
  return(output)
}
