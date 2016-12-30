#' Generate a spatial map of an expert map (with decay) to be used as an offset
#'
#' @param rdist \code{raster*} object of distances to the range edge (output from \code{\link{rangeDist}}
#' @param parms A named vector of parameter values from \code{\link{logistic}} 
#' that describe the desired curve.
#' These include all parameters for \code{\link{logistic}} (\code{rate,skew,shift})
#' and two additional parameters named \code{prob}
#' (indicating the desired probability inside the range, e.g. 0.95)
#' and \code{buffer} (a desired buffer, in meters) outside the expert range
#' that should be considered 'inside' the range.  See description for details.
#' @param dists frequency table of unique distance values 
#' (output from running \code{\link[raster]{freq}} on the \code{rdist} object).
#' If NULL, it will be created within the function but can also be supplied here
#' to speed up processing multiple logistic parameters
#' @param verbose logical indicating whether to print verbose messages
#' @param returnRaster logiical indicating whether to calculate the full spatial prior and return the raster.
#' If \code{FALSE}, the result will simply be the fitted parameters.
#' @param doNormalize logical indicating whether to normalize the raster before returning it.
#' If \code{FALSE}, the result will range from \code{lower} to \code{upper}.
#' @param doWriteRaster \code{logical} indicating whether to write the output to disk
# @param doCompress If \code{TRUE} use bossMaps::compress to shrink file size with bossMaps::rtrans
#' @param doWriteMetadata \code{logical} indicating whether to write summary metadata to disk as a \code{.csv} file
#' @param ... additional functions to be passed to \code{\link[raster]{writeRaster}}
#' @description  Uses the \code{\link{logistic}} function to transform the \code{rdist} object to a normalized expert range
#' with a desired proportion of the total probability inside the range.
#' You can set \code{fitdist} to include a buffer around the range as 'inside' for the purposes of
#' the prior expectation of what % of individuals will occur in the range.
#' Typically the user supplies \code{prob}, \code{rate}, and \code{skew} as \code{fixparms} while \code{upper} and \code{lower} are estimated.
#' @example R/examples/CalculateExpertPrior.R
#' @export
#' @import raster
rangeOffset = function(rdist,
                       parms,
                       dists = NULL,
                       returnRaster = TRUE,
                       doNormalize = TRUE,
                       verbose = TRUE,
                       doWriteRaster = FALSE,
                       doWriteMetadata = TRUE,
                       ...) {
  if (is.null(names(parms)))
    stop(paste0("parms must be a named vector"))
  if (is.null(dists)) {
    if (verbose)
      writeLines(
        paste0(
          "Creating the distance frequency table.",
          "You can provide this as an argument to speed up processing"
        )
      )
    dists = raster::freq(rdist, digits = 0, useNA = "no")
  }
  dists = stats::na.omit(dists)
  if (verbose)
    writeLines(paste0("Running Optimization"))
  rp = sum(dists[, "count"][dists[, "value"] <= 0]) /
    sum(dists[,
              "count"])
  upper1 = as.numeric(parms["prob"] / rp)
  lower1 = as.numeric((1 - parms["prob"]) / (1 - rp))
  parms = c(parms, upper = upper1)
  fixparms = unlist(c(parms, buffer = 0))
  out.lower1 = stats::nlm(
    f = minimize.fn,
    p = c(lower = lower1),
    pname = "lower",
    fixparms = fixparms,
    dists = dists,
    gradtol = 1e-10,
    iterlim = 200
  )
  fixparms = unlist(c(parms, lower = out.lower1$estimate))
  out.out = stats::optimize(
    f = minimize.fn,
    interval = c(0, max(dists[,
                              "value"], na.rm = T)),
    pname = "buffer",
    fixparms = fixparms,
    dists = dists
  )
  pout = c(fixparms, fitbuffer = round(out.out$minimum))
  pout["pinside"] = pinside(dists, pout)
  pout["pinside_fitbuffer"] = pinside(dists = dists,
                                      parms = c(pout,
                                                buffer = as.numeric(pout["fitbuffer"])))
  if ((as.numeric(parms["prob"]) - as.numeric(pout["pinside"])) <
      -0.01) {
    warning(
      paste0(
        "You asked for ",
        100 * pout["prob"],
        "% inside the range, but the parameters",
        " in combination with your expert range and domain resulted in ",
        round(100 * pout["pinside"], 2),
        "%. If the fitted % inside the range is less than the requested value,",
        " you're telling the algorithm that the expert",
        " map is very accurate (a relatively high % inside) but also that there's a slow decay in",
        " relative occurrence rate outside the range, which isn't consistent with the",
        " expert map being very accurate. Try adjusting the the prior probability inside",
        " the expert map (make it lower), decay rate (make it faster), or skew (make it higher)",
        " if you are unhappy about this...",
        " However, moving the boundary outwards by ",
        pout["fitbuffer"],
        "km can produce ",
        " a probability 'inside' of ",
        round(100 * pout["pinside_fitbuffer"], 2),
        "%.",
        "If the fitted % inside the range is greater than the requested value, try a lower value of rate.",
        "Due to the range geometry, a slow decay rate is inconsistent with a % inside the expert map."
      )
    )
  }
  if (!returnRaster)
    return(as.data.frame(t(pout)))
  if (returnRaster) {
    if (verbose)
      writeLines(paste0("Calculating the prior over the domain"))
    prior = calc(rdist, function(x)
      logistic(x, parms = pout))
    if (verbose)
      writeLines(paste0("Normalizing Prior"))
    if (doNormalize) {
      nprior = normalize(prior)
    }
    if (!doNormalize)
      nprior = prior
    pmeta = c(
      pout,
      prange = rp,
      ncells = ncell(nprior),
      nNAcells = raster::cellStats(is.na(nprior), sum),
      normalize = doNormalize
    )
    m <- list(parms = pmeta, pnames = names(pmeta))
    metadata(nprior) <- m
    lapply(list(prior), function(x)
      rmRaster(x))
    if (verbose)
      writeLines(paste0(
        "Saving normalized prior with the following parameters:\n",
        paste(names(pout), "=", format(unlist(pout),
                                       digits = 2), collapse = "\n")
      ))
    if (doWriteRaster) {
      result = writeRaster(nprior, ...)
      #result = writeRaster(nprior, filename=filename,overwrite=T)# ...)
      metadata(result) = m
      if (doWriteMetadata) {
        utils::write.csv(m, file = paste0(result@file@name, ".csv"))
      }
      #if (doCompress) { result = compress(filename, rtrans) }
      rmRaster(nprior)
      return(result)
    }
    if (!doWriteRaster)
      return(nprior)
  }
}

### Minimization function function that returns the log of the
### squared difference between the desired probability inside
### the range and the actual given the logistic curve.
minimize.fn <- function(par,
                        pname = "lower",
                        fixparms,
                        dists,
                        verbose = F) {
  names(par) = pname
  ## combine parameters into a single vector
  tparms = unlist(c(par, fixparms[!grepl(pname, names(fixparms))]))
  if (tparms["lower"] <= 0)
    return(1e+100)  # prevent lower > upper
  if (tparms["lower"] > tparms["upper"])
    return(1e+100)  # prevent lower > upper
  if (is.na(tparms["buffer"]))
    tparms["buffer"] = 0
  if (tparms["buffer"] < 0)
    return(1e+100)  # prevent buffer <= zero
  
  # get probability inside range with these parameters
  pin = pinside(dists, tparms)
  
  return(log((100 * (tparms[["prob"]] - pin)) ^ 2))  # log of the squared difference to the desired probability
}
