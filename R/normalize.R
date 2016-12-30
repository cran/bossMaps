#' Normalize a raster
#'
#' @param x  raster* object
#' @param ... additional functions to be passed to \code{\link[raster]{writeRaster}}
#' @description  Divide raster by the sum of all cells.
#' @export
#' @import raster

normalize <- function( x, ...){
  if(class(x)!="RasterLayer") stop("x must be a raster object")
  msum=raster::cellStats(x,sum, na.rm = TRUE )
  raster::calc(x,function(x)  x/msum ,...)
}
