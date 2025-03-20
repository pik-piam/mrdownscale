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
  xTarget <- terra::extract(xTarget, ref, sum, na.rm = TRUE, bind = TRUE)
  xTarget <- as.magpie(xTarget)

  stopifnot(setequal(getItems(xInput, 3), getItems(xTarget, 3)))
  out <- xTarget[, , getItems(xInput, 3)] # harmonize order of dim 3

  woody <- c("primf", "secyf", "secmf", "primn", "secnf")
  roundFuelWood <- c("roundwood_harvest_weight_type", "fuelwood_harvest_weight_type")
  toolExpectLessDiff(dimSums(out[, , paste0(woody, "_bioh")], 3),
                     dimSums(out[, , roundFuelWood], 3),
                     10^5, "Harvest weight types are consistent")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")

  return(list(x = out,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
              min = 0,
              description = "Land target data at the same low resolution as the input dataset for harmonization"))
}
