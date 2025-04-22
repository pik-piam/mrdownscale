#' convertLUH3
#'
#' Convert LUH3 cell area shares to absolute areas by multiplying with cell area, convert to Mha.
#'
#' @param x SpatRaster with LUH3 cell area shares
#' @param subtype Only "states" is converted
convertLUH3 <- function(x, subtype) {
  cellAreaKm2 <- terra::rast("multiple-static_input4MIPs_landState_CMIP_UofMD-landState-3-1_gn.nc", "carea")
  stopifnot(terra::units(cellAreaKm2) == "km2")
  # convert from km2 to Mha
  cellAreaMha <- cellAreaKm2 / 10000

  if (subtype == "states") {
    stopifnot(max(terra::minmax(x, compute = TRUE)) <= 1.0001,
              all(terra::units(x) == "1"))
    x <- x * cellAreaMha
    unit <- "Mha"
    terra::units(x) <- unit

    names(x) <- sub("-01-01\\.\\.", "..", names(x))
    terra::time(x, tstep = "years") <- as.integer(sub("-01-01$", "", terra::time(x)))
  } else if (subtype == "management") {
    unit <- "1, except fertl: kg ha-1 yr-1"

    names(x) <- sub("-01-01\\.\\.", "..", names(x))
    terra::time(x, tstep = "years") <- as.integer(sub("-01-01$", "", terra::time(x)))
  } else if (subtype == "transitions") {
    unit <- "*_bioh: kg C yr-1, *_harv: 1"
  } else {
    stop("expected subtype = states/management/transitions, but got: ", subtype)
  }

  return(list(x = x,
              class = "SpatRaster",
              unit = unit))
}
