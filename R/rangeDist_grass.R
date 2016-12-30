#' Use GRASS to calcuate distance to range edge
#'
#' @param spdata \code{list} object with range and points
#' @param domain Empty raster extent or path to geotif representing the potential modelling domain with resolution, projection, etc.
#' @param domainkm  Distance threshold (in km) from expert range to set species \code{domain}.
#' @param mask \code{logical} indicating whether to mask the distance to values <= \code{domainkm}.
#' @param verbose \code{logical} indicating whether to print status messages.
#' @param clean \code{logical} indicating whether to delete all temporary GRASS files (the default) or keep them.  By default these files are in the current \code{tempdir()} and will be deleted when quitting R.
#' @param file filename for output file exported from GRASS.
#' @param gisBase Path to GRASS Binaries as required by \code{\link[rgrass7]{initGRASS}}
#' @description  Calculates distance to range boundary and returns a raster object.  Alternatively, one could use \code{raster::distance()} but it is very slow with large rasters.  This function uses GRASS (and requires grass to be available), but is much faster.
#' @export
#' @import raster

rangeDist_grass = function(spdata,
                           domain,
                           domainkm = 1000,
                           mask = T,
                           verbose = T,
                           clean = F,
                           gisBase = NULL,
                           file) {
  ## Test inputs
  ## Confirm decayfun is legitimate
  if (!"range" %in% names(spdata))
    stop("spdata must include species range")
  
  ## buffer range to domainkm
  range = spdata[["range"]]
  ## create union of all range polygons
  urange = methods::as(gUnionCascaded(range), "SpatialPolygonsDataFrame")
  brange = gBuffer(urange, width = domainkm / 110)
  ##   crop domain to ROI
  cdomain = crop(domain, brange)
  ## rasterize range to domain
  if (verbose)
    writeLines("Rasterizing range to ROI")
  #rrangeBound=rasterize(as(urange,"SpatialLines"),cdomain)
  rrange = rasterize(methods::as(urange, "SpatialPolygons"), cdomain)
  ## write rrange to disk for grass processing
  rfile = paste0(tempfile(), ".tif")
  writeRaster(
    rrange,
    rfile,
    format = "GTiff",
    overwrite = T,
    NAvalue = -99
  )
  ## start grass with geotif as source
  domain_sp = methods::as(rrange, "SpatialGrid")
  
  ## start grass instance using domain
  if(is.null(gisBase)) stop("Please supply gisBase path to GRASS Binaries")
  rgrass7::initGRASS(
    mapset = "PERMANENT",
    SG = domain_sp,
    gisBase = gisBase,
    override = T
  )#,
  rgrass7::execGRASS(
    "g.proj",
    flags = c("c", "quiet"),
    proj4 = projection(rrange),
    ignore.stderr = T
  )
  rgrass7::execGRASS(
    "r.in.gdal",
    flags = c("quiet", "overwrite", "e"),
    input = rfile,
    output = "range",
    ignore.stderr = T
  )
  rgrass7::execGRASS(
    "g.region",
    flags = c("a"),
    raster = "range",
    align = "range"
  )
  #file.remove(rfile)
  
  if (F) {
    #some testing junk not to be run
    rgrass7::execGRASS("g.region", flags = c("p"))
    res = raster(rgrass7::readRAST("range", ignore.stderr = T))
  }
  
  ## set mask to limit processing to within the mask
  if (mask) {
    ## push to grass
    rgrass7::writeVECT(urange,
              vname = "expert",
              v.in.ogr_flags = c("overwrite", "quiet"))
    ## create buffered version to crop domain
    domainbuffer = domainkm / 110
    rgrass7::execGRASS(
      "v.buffer",
      flags = c("quiet", "overwrite"),
      input = "expert",
      output = "bexpert",
      distance = domainbuffer,
      ignore.stderr = T
    )
    rgrass7::execGRASS(
      "r.mask",
      vector = "bexpert",
      flags = "overwrite",
      ignore.stderr = T
    )
  }
  ## calculate distances
  if (verbose)
    writeLines("Calculating distance-to-range")
  rgrass7::execGRASS(
    "r.grow.distance",
    flags = c("m", "overwrite"),
    input = "range",
    distance = "distance1",
    metric = "geodesic"
  )
  if (verbose)
    writeLines("Converting to integer")
  rgrass7::execGRASS(
    "r.mapcalc",
    flags = "overwrite",
    expression = paste0("dist=int(round(distance1/1000))")
  )
  
  ## write it to disk
  if (verbose)
    writeLines("Saving output")
  rgrass7::execGRASS(
    "r.out.gdal",
    flags = c("verbose", "overwrite"),
    input = "dist",
    output = file,
    createopt = c("COMPRESS=DEFLATE", "ZLEVEL=9")
  )
  res = raster(file)
  #  res=raster(readRAST("dist",ignore.stderr = T))
  
  ## clean up minor extent issues (GRASS seems to round a bit on resolution, etc.)
  #extent(res)=alignExtent(res,rrange)
  #res(res)=res(domain)
  
  ## remove temporary GRASS database
  if (clean) {
    grassdir = file.path(rgrass7::gmeta()$GISDBASE, rgrass7::gmeta()$LOCATION_NAME)
    file.remove(list.files(grassdir, recursive = T, full.names = T))
  }
  ## return the distance raster
  return(res)
}