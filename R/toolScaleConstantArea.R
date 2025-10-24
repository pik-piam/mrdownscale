#' toolScaleConstantArea
#'
#' Scale x to make the sum of all land types is constant over time.
#'
#' @param x a magpie object
#' @param ... not used, will throw an error if supplied
#' @param noteThreshold if the maximum difference between the scaled and the
#' original data is greater than this, a note is triggered
#' @param warnThreshold if the maximum difference between the scaled and the
#' original data is greater than this, a warning is triggered
#' @return x scaled
#'
#' @author Pascal Sauer
#' @export
toolScaleConstantArea <- function(x, ..., noteThreshold = 10^-10, warnThreshold = 10^-5) {
  stopifnot(nyears(x) >= 2, ...length() == 0, noteThreshold <= warnThreshold)

  areaSum <- dimSums(x, 3)
  meanArea <- dimSums(areaSum, 2) / nyears(areaSum)
  scalingFactor <- meanArea / areaSum
  scalingFactor[is.nan(scalingFactor)] <- 1
  scaledX <- x * scalingFactor

  maxDiff <- max(abs(scaledX - x))

  if (maxDiff > warnThreshold) {
    toolStatusMessage("warn", paste0("Total area not constant over time (max diff: ",
                                     signif(maxDiff, 2), ")"), level = 1)
  }

  if (maxDiff > noteThreshold) {
    toolStatusMessage("note", paste0("area was scaled to make it constant over time ",
                                     "(min factor: ", round(min(scalingFactor), 2),
                                     ", max factor: ", round(max(scalingFactor), 2), ")"), level = 1)
  }

  return(scaledX)
}
