#' calcLandTargetLowRes
#'
#' Aggregate target land data to the spatial resolution of the input data in
#' preparation for harmonization.
#'
#' @param input name of an input dataset, see \code{\link{calcLandInput}}
#' available input datasets
#' @param target name of a target dataset, see \code{\link{calcLandTarget}}
#' available target datasets
#' @return low resolution target land data
#' @author Pascal Sauer
calcLandTargetLowRes <- function(input, target) {
  xInput <- calcOutput("LandInputRecategorized", input = input,
                       target = target, aggregate = FALSE)
  xTargetIn <- calcOutput("LandTarget", target = target, aggregate = FALSE)

  # bring target data to spatial resolution of input data
  ref    <- as.SpatVector(xInput[, 1, 1])[, c(".region", ".id")]
  xTarget <- terra::extract(xTargetIn, ref, "sum", na.rm = TRUE, bind = TRUE)
  xTarget <- as.magpie(xTarget)

  stopifnot(setequal(getItems(xInput, 3), getItems(xTarget, 3)))
  out <- xTarget[, , getItems(xInput, 3)] # harmonize order of dim 3

  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(xTarget, 3)),
                 "Land categories did not change")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5, "Total area is constant over time")
  toolPrimExpansionCheck(out)

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Land target data at the same low resolution as the input dataset for harmonization",
              clean_magpie = FALSE))
}
