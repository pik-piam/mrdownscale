#' convertLUH3
#'
#' Convert LUH3 cell area shares to absolute areas by multiplying with cell area, convert to Mha.
#'
#' @param x SpatRaster with LUH3 cell area shares
#' @param subtype Only "states" is converted
convertLUH3 <- function(x, subtype = "states") {
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
  } else {
    stop("subtype must be states, for other subtypes pass convert = FALSE")
  }

  return(list(x = x,
              class = "SpatRaster",
              unit = unit))
}
