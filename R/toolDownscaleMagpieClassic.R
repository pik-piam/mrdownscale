#' toolDownscaleMagpieClassic
#'
#' classic MAgPIE downscaling method using \code{luscale::interpolate2} as
#' downscaling function.
#'
#' @param x magclass containing land to be downscaled
#' @param xTarget magclass target land use dataset for initialization year
#' @param mapping mapping between \code{x} and \code{xTarget}
#' @return downscaled land use dataset
#' @author Jan Philipp Dietrich, Pascal Sauer
toolDownscaleMagpieClassic <- function(x, xTarget, mapping) {
  mapping$cluster <- mapping$lowRes
  mapping <- mapping[, c("cell", "cluster")]

  mapping <- mapping[mapping$cluster %in% getItems(x, 1), ] # TODO do we want this in general?
  stopifnot(setequal(mapping$cluster, getItems(x, 1)),
            setequal(mapping$cell, getItems(xTarget, 1)))

  "!# @monitor luscale::interpolate2"

  # interpolate2 assumes constant total over time, but only warns if unfulfilled, convert that to error
  tryCatch({
    out <- luscale::interpolate2(x[, -1, ], xTarget, mapping)
  }, warning = stop)
  getSets(out)[1:2] <- c("x", "y")

  return(out)
}
