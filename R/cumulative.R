#' Make maxent's cumulative output from raw output and (optionally) apply a threshold
#'
#' @param x  raster* object of continuous (raw) maxent output
#' @param threshold  \code{scalar} value to threshold the continuous map into binary predictions
#' @description  The function first converts the raw maxent output to cumulative (by sorting and calculating the cumulative sum).
#' Then the (optional) threshold is applied to return a binary map of the estimated range.
#' If \code{threshold} is \code{NULL}, the cumulative output is returned.
#' @export

cumulative = function(x, threshold = NULL) {
  z = values(x)
  nas = is.na(z)
  sorted = sort(z,
                index.return = T,
                na.last = NA,
                method = "quick")
  cum = cumsum(sorted$x)
  z2 = rep(NA, length(z))
  z2[!nas][sorted$ix] = cum * 100
  ## if threshold is not null, then apply it to make a binary Map.
  if (!is.null(threshold)) {
    z2 = ifelse(z2 > 100 * (1 - threshold), 1, 0)
    thresholdval = min(z[z2 == 1], na.rm = T)
  }
  ## replace values of original raster with cumulative (and possibly thresholded values)
  values(x) = z2
  if (!is.null(threshold))
    metadata(x) = list(threshold = thresholdval)
  return(x)
}