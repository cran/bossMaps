#' Really remove raster files
#'
#' @param x a \code{raster*} object to be removed
#' @param verbose logical indicating whether to print verbose messages
#' @description The raster package has the sometimes problematic behavior of leaving temporary files after the R object has been removed.  
#' This function first checks if a temporary file exists on disk and removes it.
#' @author Adam M. Wilson
#' @author Keith Ma
#' @references https://gist.github.com/adammwilson/10180852
#' @export
#' @import raster

rmRaster <- function(x, verbose = FALSE) {
  
  stopifnot(grepl("Raster",class(x)))
  if(!fromDisk(x)) return(NULL)
  
  ## get raster temporary directory
  
  sink(tempfile())  # use sink to supress printing of rasterOptions() summary
  tdir=rasterOptions()[["tmpdir"]]
  sink(NULL)
  
  if(class(x)=="RasterLayer") files=basename(x@file@name)
  if(class(x)=="RasterStack") files=do.call(c,lapply(methods::slot(x,"layers"),function(x) x@file@name))

  files=files[file.exists(files)]
  if(length(files)==0) return(NULL)

  lapply(files,function(f) {
    if(fromDisk(x) & file.exists(f))  
      file.remove(f, sub('grd', 'gri', f))
    if(verbose){
      print(paste('Deleted: ', f))
      print(paste('Deleted: ', sub('grd', 'gri', f)))
    }
  })

  parent.var.name <- deparse(substitute(x))
  rm(list = parent.var.name, envir = sys.frame(-1))
  
  #if(verbose) print(paste('Removed: ', parent.var.name))
}