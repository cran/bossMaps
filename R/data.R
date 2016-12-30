#' Point distributional data for the Solitary Tinamou (Tinamus solitarius)
#'
#' Point ocurrance data from GBIF and eBird extracted from the Map Of Life (\url{mol.org}).
#'
#' @author Adam M. Wilson
#' @references \url{MOL.org}
#' @format A SpatialPointsDataFrame object with 171 point locations and the following variables: 
#' \code{cartodb_id}, \code{type},
#' \code{provider}, \code{seasonality},
#' \code{presence}, \code{uncertainty},
#' and \code{date} collected.
"Tinamus_solitarius_points"

#' Expert range data for the Solitary Tinamou (Tinamus solitarius)
#'
#' Expert range data extracted from the Map Of Life (\url{mol.org}).
#'
#' @author Adam M. Wilson
#' @references \url{MOL.org}
#' @format A SpatialPolygonDataFrame object with 2 versions of the expert range map
"Tinamus_solitarius_range"

#' Expert range environmental data for the Solitary Tinamou (Tinamus solitarius)
#'
#' Environmental data (from WorldClim) for the Solitary Tinamou's range (\url{mol.org}).
#' Bio1 is Mean Annual Temperature and Bio12 is Mean Annual Precipitation.
#'
#' @author Adam M. Wilson
#' @format A rasterStack object
"Tinamus_solitarius_env"


#' Point distributional data for the Beamys hindei
#'
#' Point ocurrance data from GBIF and eBird extracted from the Map Of Life (\url{mol.org}).
#'
#' @author Adam M. Wilson
#' @references \url{MOL.org}
#' @format A SpatialPointsDataFrame object with 171 point locations and the following variables: 
#' \code{cartodb_id}, \code{type},
#' \code{provider}, \code{seasonality},
#' \code{presence}, \code{uncertainty}, 
#' and \code{date} collected.
"Beamys_hindei_points"

#' Expert range data for Beamys_hindei
#'
#' Expert range data extracted from the Map Of Life (\url{mol.org}).
#'
#' @author Adam M. Wilson
#' @references \url{MOL.org}
#' @format A SpatialPolygonDataFrame object with 2 versions of the expert range map
"Beamys_hindei_range"

#' Expert range data for Leucadendron lanigerum
#'
#' Expert range data for Leucadendron lanigerum, a South African Shrub.
#'
#' @author Adam M. Wilson
#' @references \url{MOL.org}
#' @format A Raster layer object with 1s for the expert range, 0s for the rest of the domain, and NAs outside the domain.
"Leucadendron_lanigerum_range"
