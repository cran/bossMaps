#' Evaluate whether potential curve parameters are feasible given the range and domain geometry.
#'
#' @param rdist \code{raster*} object of distances to the range edge (output from \code{\link{rangeDist}}
#' @param dists frequency table of unique distance values (output from running \code{\link[raster]{freq}} on the \code{rdist} object).
#' If NULL, it will be created within the function but can also be supplied here to speed up processing multiple logistic parameters
#' @param prob vector of "probability inside" values to consider
#' @param rate vector of rate values to consider
#' @param skew vector of skew values to consider
#' @param shift vector of shift values to consider
#' @param verbose logical indicating whether to print verbose messages
#' @param plot logical indicating whether to draw a plot of desired probability vs. decay rate.
#' @return dataframe of fitted parameters for each parameter combination provided with prob, rate, skew, and shift.
#' @description Note that parallelization is supported to expedite evaluation; see examples
#' @import raster ggplot2 foreach doParallel
#' @export
#' @examples
#' data(Beamys_hindei_range)
#' # Generate domain
#'  domain=raster::raster(xmn=-180, xmx=180, ymn=-90, ymx=90,
#'  crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs",
#'  resolution=.1, vals=NULL)
#'  # registerDoParallel(2) #optionally use to speed up calculation
#'  # Calculate distance to range
#'  rdist=rangeDist(range=Beamys_hindei_range,domain=domain,
#'                  domainkm=1000,mask=FALSE,fact=2,verbose=FALSE)
#'  dpar=checkRates(rdist,plot=FALSE)
#'  head(dpar)

checkRates = function(rdist,
                      dists = NULL,
                      prob = seq(0.1, 1, len = 10),
                      rate = exp(seq(log(0.01), log(10), len = 10)),
                      skew = c(0.2),
                      shift = 0,
                      verbose = T,
                      plot = T) {
  ### expand grid of values
  fvars = expand.grid(
    prob = prob,
    rate = rate,
    skew = skew,
    shift = 0,
    stringsAsFactors = F
  )
  
  ## create the rdist frequency table if not provided
  if (is.null(dists)) {
    writeLines(
      paste(
        "Calculating the range distance frequency table,",
        "you can also provide this with the dists parameter."
      )
    )
    dists = freq(rdist, digits = 0, useNA = "no")
  }
  
  if (verbose)
    writeLines(
      paste(
        "Fitting the expert range decay,",
        "this can take a while depending on how many values you selected..."
      )
    )
  
  # temporarily define vars for R CMD CHECK, which doesn't like defining them inside loops.
  i=pdif=fitbuffer=..level..=vector()
  
  ## loop over vars and compute the rangeOffset
  dfvars = foreach::foreach(
    i = 1:nrow(fvars),
    .combine = rbind.data.frame,
    .errorhandling = 'remove'
  ) %dopar% {
    rangeOffset(
      rdist,
      dists = dists,
      parms = c(
        prob = fvars$prob[i],
        rate = fvars$rate[i],
        skew = fvars$skew[i],
        shift = fvars$shift[i]
      ),
      returnRaster = F,
      verbose = F
    )
  }
  
  dfvars$fitbuffer = round(dfvars$fitbuffer)
  dfvars$pdif = (dfvars$pinside - dfvars$prob) * 100
  dfvars$range = dfvars$upper - dfvars$lower
  
  if (plot) {
    p1 = ggplot2::ggplot(as.data.frame(dfvars),
              ggplot2::aes(
                  x = rate,
                  y = prob,
                  fill = pdif,
                  z = fitbuffer
                )) +
      facet_grid(skew ~ shift) +
      geom_raster() + scale_x_log10() +
      geom_point(data = dplyr::filter(dfvars, abs(pdif) < 0.01), size = .2) +
      stat_contour(aes(linetype = as.factor(..level..)),
                   breaks = c(10, 50, 100, 150)) +  #fit distance in km
      scale_linetype_manual(values = c(1, 5, 4, 3), name = "Required\nBuffer (km)") +
      scale_fill_gradient2(
        low = "red",
        mid = "grey90",
        high = "blue",
        midpoint = -5,
        name = expression(paste(Delta, "Pin")),
        na.value = "transparent",
        guide = guide_colorbar(barwidth = 1, direction = "vertical"),
        space = "Lab"
      ) +
      xlab("Decay Rate (log axis)") + ylab("Desired Probability Inside the Range") +
      ## add arrows
      annotate(
        "text",
        x = .04,
        y = 0.25,
        label = "Slower\nDecay",
        angle = 0,
        size = 5,
        colour = 'black',
        fontface = "bold"
      ) +
      annotate(
        "text",
        x = 5,
        y = 0.25,
        label = "Faster\nDecay",
        angle = 0,
        size = 5,
        colour = 'black',
        fontface = "bold"
      ) +
      geom_segment(
        aes(
          x = 0.1,
          y = 0.25,
          xend = 2,
          yend = 0.25
        ),
        colour = 'black',
        size = .5,
        arrow = arrow(length = unit(0.5, "cm"), ends = "last")
      ) +
      ggtitle(expression(atop(
        "Feasible parameter combinations",
        atop(
          italic(
            "Dots indicate combinations that achieve the desired probability inside"
          ),
          italic(
            "Colors indicate the difference between desired and achieved probability inside"
          )
        )
      ))) +
      theme_bw()
    print(p1)
  }
  
  return(dfvars)
}