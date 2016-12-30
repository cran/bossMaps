#' Use Calcuate distance to range edge
#'
#' @param range SpatialPolygons* object with species range
#' @param points Optional SpatialPoints* object with species occurrences to be
#' included in defining the domain, but not in the distance calculations.
#' @param domain Empty raster extent or path to geotif representing the potential 
#' modelling domain with resolution, projection, etc.
#' @param domainkm  Define the distance (in km) from expert range to include in the modeling domain.  The range polygon will be buffered by this value and used to define a rectangular region around the range.  See \code{mask} to additionally mask pixels with distances farther than a specified value.  
#' @param fact \code{numeric} aggregation factor used to speed up
#' distance calculation (larger numbers faster but less accurate).
#' @param mask \code{logical} indicating whether to mask the domain to the range buffered by \code{domainkm}.  
#' If \code{FALSE} (the default), the returned object will have complete data across a rectangular domain.  
#' If \code{TRUE}, the rectangular domain will be masked by the buffered range.  
#' @param verbose \code{logical} indicating whether to print status messages.
#' @param ... additional functions to be passed to \code{\link[raster]{writeRaster}}
#' @description  Calculates distance to range boundary and returns a raster object.  
#' Range is projected to World Azimuthal Equidistant (centered on range centroid) before buffering.
#' Starting a cluster with \code{beginCluster()} can improve processing time if multiple cores are available.
#' @export
#' @import rgeos sp raster
 
rangeDist = function(range, points, domain, domainkm = 1000, 
  fact = 2, mask = FALSE, verbose = TRUE, ...) {
  ## Test inputs Confirm range is spatialPolygons
  if (!class(range) %in% c("SpatialPolygonsDataFrame",
                           "SpatialPolygons", 
                           "RasterLayer")) 
    stop("range must be a spatialPolygons* object or raster object")
  
  if (grepl("SpatialPolygons", class(range))) {
    ## create union of all range polygons
    urange = methods::as(gUnionCascaded(range), "SpatialPolygonsDataFrame")
    
    ## Project to Plate Carree (equidistant cylindrical) centered
    ## on range centroid before buffering
    bc = coordinates(gCentroid(urange))  #centroid
    uproj = projection(urange)  #store projection
    bproj = paste0("+proj=eqc +lon_0=", bc[2]) #define new projection for buffering
    
    # reproject range
    urange2 = spTransform(urange, bproj)
    
    ## If points are given, add them to the range before
    ## buffering.
    if (!missing(points)) {
      points2 = spTransform(points, bproj)
      # include points with tiny buffer (to make them polygons)
      urange_outer = gUnion(urange2, gBuffer(points2, width = 0.001))
    }
    else 
      urange_outer = urange2  #don't include points
    
    ## buffer range to domainkm
    brange1 = gBuffer(urange_outer, width = domainkm * 1000)
    ## transform back to original projection
    brange = spTransform(brange1, uproj)
    ## crop domain to ROI
    cdomain = crop(domain, brange)
    ## rasterize range to domain
    if (verbose) 
      writeLines("Rasterizing range to ROI")
    rrange = rasterize(methods::as(urange, "SpatialPolygons"), cdomain, 
      field = -1, background = 1)
    ## delete any temporary files
    lapply(list(cdomain), function(x) rmRaster(x))

    # get range edges
    rbound = boundaries(rrange, classes = TRUE, asNA=T)
    
      }
  
  
  
  if (class(range) == "RasterLayer") {
    # Confirm range raster has only 0s and 1s  
    range_unique=unique(range)
    if(!all.equal(range_unique,c(0,1)))
      stop(
        paste("Range raster must include only 0s and 1s (and NAs for areas outside the domain),",
              "currently it contains the following values (may be truncated): ",
              paste(range_unique,collapse=",")))
    
        ## reclassify to -1 and 1 for boundary() to work correctly below
        rrange=reclassify(range,rcl=matrix(c(-.1,.1,1,.9,1.1,-1),nrow=2,byrow=T))
        rrange_forboundary=reclassify(range,rcl=matrix(c(-.1,.1,NA,.9,1.1,1),nrow=2,byrow=T))
        
        # get range edges from raster
        rbound = boundaries(rrange_forboundary, classes = TRUE, asNA=T)
        
  }

    
  # Aggregate to speed up distance calculation
  if (fact > 1) {
    rrange2 = aggregate(rrange, fact, max)
    rbound3 = aggregate(rbound, fact, max)
  } else {
    rrange2 = rrange
    rbound3 = rbound
  }
  # Calculate distance
  if (verbose) 
    writeLines("Calculating distances")
  rdist1 = distance(rbound3)
  rdist2 = rrange2 * rdist1/1000
  ## resample back to full resolution and optionally apply mask.
  if (verbose) 
    writeLines("Resampling back to original resolution")
  
  ## delete any temporary files
  lapply(list(rrange, 
              rbound, 
              rrange2, 
              rbound3, 
              rdist1), 
    function(x) rmRaster(x))
  
  ## If no masking, return it
  
  if (!mask) 
    if (fact > 1) {
      return(resample(rdist2, rrange, ...))
    } else {
      return(rdist2)
    }
  
  if (mask) {
    if (fact > 1) {
      rdist3 = resample(rdist2, rrange)
    } else {
      rdist3 = rdist2
    }
    if (verbose) 
      writeLines("Masking to buffered range")
    rdist4 = mask(rdist3, brange, ...)
    rmRaster(rdist3)
    return(rdist4)
  }
}
