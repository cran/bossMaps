% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rangeDist.R
\name{rangeDist}
\alias{rangeDist}
\title{Use Calcuate distance to range edge}
\usage{
rangeDist(range, points, domain, domainkm = 1000, fact = 2, mask = FALSE,
  verbose = TRUE, ...)
}
\arguments{
\item{range}{SpatialPolygons* object with species range}

\item{points}{Optional SpatialPoints* object with species occurrences to be
included in defining the domain, but not in the distance calculations.}

\item{domain}{Empty raster extent or path to geotif representing the potential 
modelling domain with resolution, projection, etc.}

\item{domainkm}{Define the distance (in km) from expert range to include in the modeling domain.  The range polygon will be buffered by this value and used to define a rectangular region around the range.  See \code{mask} to additionally mask pixels with distances farther than a specified value.}

\item{fact}{\code{numeric} aggregation factor used to speed up
distance calculation (larger numbers faster but less accurate).}

\item{mask}{\code{logical} indicating whether to mask the domain to the range buffered by \code{domainkm}.
If \code{FALSE} (the default), the returned object will have complete data across a rectangular domain.  
If \code{TRUE}, the rectangular domain will be masked by the buffered range.}

\item{verbose}{\code{logical} indicating whether to print status messages.}

\item{...}{additional functions to be passed to \code{\link[raster]{writeRaster}}}
}
\description{
Calculates distance to range boundary and returns a raster object.  
Range is projected to World Azimuthal Equidistant (centered on range centroid) before buffering.
Starting a cluster with \code{beginCluster()} can improve processing time if multiple cores are available.
}

