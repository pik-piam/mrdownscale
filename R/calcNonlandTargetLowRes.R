#' calcNonlandTargetLowRes
#'
#' Aggregate target nonland data to the spatial resolution of the input data in
#' preparation for harmonization.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @param target name of a target dataset, currently only "luh2mod"
#' @return low resolution target nonland data
#' @author Pascal Sauer
calcNonlandTargetLowRes <- function(input, target) {
  xInput <- calcOutput("NonlandInputRecategorized", input = input, target = target, aggregate = FALSE)

  # get target data in spatial resolution of input data
  xTarget <- calcOutput("NonlandTarget", target = target, aggregate = FALSE)
  ref <- as.SpatVector(xInput[, 1, 1])[, c(".region", ".id")]
  out <- terra::extract(xTarget, ref, sum, na.rm = TRUE, bind = TRUE)
  out <- as.magpie(out)

  stopifnot(setequal(getItems(xInput, 3), getItems(out, 3)))
  out <- out[, , getItems(xInput, 3)] # harmonize order of dim 3

  roundFuelWood <- c("roundwood_harvest_weight_type", "fuelwood_harvest_weight_type")
  toolExpectLessDiff(dimSums(out[, , grep("_bioh$", getItems(out, 3), value = TRUE)], 3),
                     dimSums(out[, , roundFuelWood], 3),
                     10^5, "Harvest weight types are consistent")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")

  lastYear <- terra::time(xTarget)[length(terra::time(xTarget))]
  mTarget <- as.magpie(xTarget[[terra::time(xTarget) == lastYear]])
  toolExpectLessDiff(dimSums(mTarget, 1), dimSums(out[, lastYear, ], 1), 10^-5,
                     "total sum is not changed by aggregation")

  return(list(x = out,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
              min = 0,
              description = "Land target data at the same low resolution as the input dataset for harmonization"))
}
