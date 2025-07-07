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
  ref <- as.SpatVector(xInput[, 1, 1])[, c(".region", ".id")]

  # get target data in spatial resolution of input data
  xTarget <- calcOutput("NonlandTarget", target = target, aggregate = FALSE)
  out <- terra::extract(xTarget, ref, sum, na.rm = TRUE, bind = TRUE)
  out <- as.magpie(out)
  getItems(out, 3, raw = TRUE) <- sub("^(.+?)_(.+)$", "\\2.\\1", getItems(out, 3))
  names(dimnames(out))[3] <- "category.data"

  lastYear <- terra::time(xTarget)[length(terra::time(xTarget))]
  mTarget <- as.magpie(xTarget[[terra::time(xTarget) == lastYear]])
  getItems(mTarget, 3, raw = TRUE) <- sub("^(.+?)_(.+)$", "\\2.\\1", getItems(mTarget, 3))
  names(dimnames(mTarget))[3] <- "category.data"
  toolExpectLessDiff(dimSums(mTarget, 1), dimSums(out[, lastYear, ], 1), 10^-5,
                     "total sum is not changed by aggregation")

  stopifnot(setequal(getItems(xInput, 3), getItems(out, 3)))
  out <- out[, , getItems(xInput, 3)] # harmonize order

  # toolExpectTrue(max(out[, , "fertilizer"]) <= 1200,
  #                paste0("Fertilizer application is <= 1200 kg ha-1 yr-1 (max: ",
  #                       signif(max(out[, , "fertilizer"]), 3), ")"))

  toolExpectLessDiff(dimSums(out[, , "bioh"], 3),
                     dimSums(out[, , "harvest_weight_type"], 3),
                     10^5, "Harvest weight types are consistent")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")

  return(list(x = out,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: Tg yr-1",
              min = 0,
              description = "Land target data at the same low resolution as the input dataset for harmonization"))
}
