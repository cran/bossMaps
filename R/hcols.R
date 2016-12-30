#' Aesthetically pleasing color ramp for plotting habitat suitability
#'
#' @param x number of colors to return
#' @param bias a positive number. Higher values give more widely spaced colors at the high end (as in \link{colorRampPalette}).
#' @description Aesthetically pleasing color ramp for plotting habitat suitability
#' @return Returns a character vector of colors (see \code{\link{rgb}}) interpolating 
#' the given sequence (similar to \code{\link{heat.colors}} or \code{\link{terrain.colors}}.
#' @examples 
#' library(ggplot2)
#' data=cbind.data.frame(expand.grid(x=1:10,y=1:10),z=rnorm(100))
#' ggplot(data,aes(x=x,y=y,fill=z))+geom_raster()+
#' scale_fill_gradientn(colours=hcols(20,bias=2))
#' @export

hcols=function(x,bias=1) {
  grDevices::colorRampPalette(c('grey90','steelblue4','steelblue2','steelblue1','gold','red1','red4'),bias=bias)(x) 
}