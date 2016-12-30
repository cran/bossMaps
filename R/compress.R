#' Compress maxent output by converting from ASCII file to compressed geotif
#'
#' @param file \code{character} path to file to be compressed
#' @param fun Function to use to compress the data.  Default is \code{rtrans()}, which is \code{round(log(x)*100}.
#' @param ... additional functions to be passed to \code{writeRaster()}
#' @param x value passed to \code{rtrans} or \code{untrans} for conversion.
#' @description  Maxent writes out ASCII files that are uncompressed. This function transforms the data, converts it to a compressed
#' geotif, and deletes the original.  The default transform \code{rtrans} is to take the \eqn{round(log(x)*100)}.
#' Note that this conversion may be lossy depending on the transform applied.
#' @return Returns the converted file as a raster() object
#' @export
#' @import raster

compress = function(file, fun = rtrans) {
  temprast = raster(file)
  calc(
    temprast,
    fun,
    filename = sub(".grd", ".tif", file),
    options = c("COMPRESS=LZW",
                "PREDICTOR=2"),
    datatype = "INT2S",
    overwrite = TRUE
  )
  file.remove(file)
  file.remove(sub('.grd','.gri',file))
  writeLines(
    paste0(
      "compress() applies a potentially lossy compression format that needs to be untransformed before use"
    )
  )
  return(raster(sub(".grd", ".tif", filename)))
}

#' @describeIn compress transform data to faciliate storing as an integer as \eqn{round(log(x)*100)}.
rtrans = function(x) {
  round(log(x) * 100)
}

#' @describeIn compress uncompress data transformed with \code{rtrans}
unrtrans = function(x) {
  exp(x / 100)
}
