\dontrun{
  library(raster)
  library(ggplot2)
  
  data("Tinamus_solitarius_points")
  data("Tinamus_solitarius_range")
  
  ## Define global modeling grid
  domain = raster(
    xmn = -180,
    xmx = 180,
    ymn = -90,
    ymx = 90,
    crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs",
    resolution = 10 / 360,
    vals = NULL
  )
  
  ## turn on raster progress bar
  rasterOptions(progress = "")
  
  ## calculate distance-to-range: this is slow but only has to
  ## be done once per species.  Can speed it up by increasing
  ## 'fact' (at the expense of reduced accuracy).
  range = Tinamus_solitarius_range
  points=Tinamus_solitarius_points
  
  rdist = rangeDist(range=range,
                    domain=domain,
                    domainkm = 100,
                    mask = FALSE,
                    fact = 10)
  
  ## Mask out undesired areas (ocean, etc.)  Typically you would
  ## do this using your environmental data, but here we'll just
  ## use a coastline polygon from the maps package
#    land = map(
#    interior = F,
#    fill = T,
#    xlim = bbox(rdist)[1, ],
#    ylim = bbox(rdist)[2, ]
#  )
#  land = map2SpatialPolygons(land, IDs = land$names)
#  rdist = mask(rdist, land)
  
  ## calculate frequency table of distances
  dists = freq(rdist)
  
  ### plot to visualize potential decay parameters
  vars = expand.grid(
    rate = c(0, 0.03, 0.05, 0.1, 10),
    skew = c(0.2,
             0.4),
    shift = 0,
    stringsAsFactors = FALSE
  )
  x = seq(-150, 300, len = 1000)
  
  ## Calculate all the curves
  erd = do.call(rbind, lapply(1:nrow(vars), function(i) {
    y = logistic(x, parms = unlist(c(
      lower = 0, upper = 1, vars[i, ]
    )))
    return(cbind.data.frame(
      group = i,
      c(vars[i, ]),
      x = x,
      y = y
    ))
  }))
  
  ## plot it
  ggplot(erd,
         aes(
           x = x,
           y = y,
           linetype = as.factor(skew),
           colour = as.factor(rate),
           group = group
         )) + 
    geom_vline(aes(xintercept=0),
               colour = "red") + geom_line() +
    xlab("Prior value (not normalized)") +
    xlab("Distance to range edge (km)")
  
  
  ## calculate the expert range prior
  expert = rangeOffset(
    rdist,
    dists = dists,
    parms = c(
      prob = 0.9,
      rate = 0.05,
      skew = 0.4,
      shift = 0
    ),
    normalize = TRUE,
    verbose = TRUE
  )
  
  ## View the metadata
  metadata(expert)$parms
  
  ## plot it
  plot(expert)
}
