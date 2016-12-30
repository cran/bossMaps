#' Create an axis transform using the Inverse hyperbolic sine transformation
#'
#' @description  Create an axis transform using the Inverse hyperbolic sine transformation to allow log-like axis using data with negative values
#' @return Adds a new transformation for use with ggplot
#' @references \url{http://wresch.github.io/2013/03/08/asinh-scales-in-ggplot2.html}
#' @references \url{http://robjhyndman.com/hyndsight/transformations/}
#' @references \url{http://stackoverflow.com/questions/14504869/histogram-with-negative-logarithmic-scale-in-r}
#' @export
#' @examples   
#'  library(ggplot2)
#'  ggplot(data.frame(x=seq(-1000,1000,len=200)), aes(x=x,y=x))+
#'  geom_line(size=1)+
#'  scale_x_continuous(trans = 'asinh',breaks=c(-1000,-100,10,-1,0,1,10,100,1000))


asinh_trans <- function(){
  scales::trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}
