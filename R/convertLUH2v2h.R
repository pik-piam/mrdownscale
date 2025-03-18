#' convertLUH2v2h
#'
#' Convert LUH2 cell area shares to absolute areas by multiplying with cell area, convert to Mha.
#'
#' @param x SpatRaster with LUH2 cell area shares
#' @param subtype Only "states" is converted
convertLUH2v2h <- function(x, subtype) {
  cellAreaKm2 <- terra::rast("staticData_quarterdeg.nc", "carea")
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
