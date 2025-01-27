#' readLUH3
#'
#' Read LUH3 data. For the states subtype, the secma and secmb categories are removed.
#' For the management subtype, only the categories cpbf1, cpbf2, rndwd, fulwd, fertl and irrig are read.
#' For the transitions subtype, only the wood harvest categories bioh and harv are read. To
#' match magpie semantics years are shifted by 1 when reading transitions.
#' The LUH3 nc files have day-based time, which is converted to years.
#'
#' @param subtype one of states, management, transitions, cellArea
readLUH3 <- function(subtype) {
  years <- 1995:2015

  if (subtype == "cellArea") {
    cellArea <- terra::rast("multiple-fixed_input4MIPs_landState_CMIP_UofMD-landState-3-0_gn.nc", "carea")
    return(list(x = cellArea, class = "SpatRaster", cache = FALSE, unit = "km2"))
  }

  # switch from tstep days to years and subset to years
  toYearsAndSubset <- function(x, years, offset = 0) {
    stopifnot(years >= 1970) # handling years < 1970 is tricky with terra
    terra::time(x, tstep = "years") <- terra::time(x, format = "years") + offset
    x <- x[[terra::time(x) %in% years]]
    return(x)
  }

  if (subtype == "states") {
    x <- terra::rast("multiple-states_input4MIPs_landState_CMIP_UofMD-landState-3-0_gn_0850-2024.nc")
    x <- toYearsAndSubset(x, years)
    # remove secma & secmb
    x <- x[[grep("secm[ab]", names(x), invert = TRUE)]]
    unit <- "1"
  } else if (subtype == "management") {
    x <- terra::rast("multiple-management_input4MIPs_landState_CMIP_UofMD-landState-3-0_gn_0850-2024.nc")
    x <- toYearsAndSubset(x, years)

    # combf is a share of wood harvest like rndwd and fulwd, but we can ignore it as long as it is 0 everywhere
    stopifnot(identical(max(terra::values(max(x["combf"])), na.rm = TRUE), 0))

    # there are variables for 2nd gen biofuel c3ann, c4ann, c3nfx, but we can ignore it as long as it is 0 everywhere
    stopifnot(identical(max(terra::values(max(x["cpbf2_(c3ann|c4ann|c3nfx)"])), na.rm = TRUE), 0))

    x <- x["cpbf1|cpbf2_c3per|cpbf2_c4per|rndwd|fulwd|fertl|irrig"]
    unit <- "1, except fertl: kg ha-1 yr-1"
  } else if (subtype == "transitions") {
    x <- terra::rast("multiple-transitions_input4MIPs_landState_CMIP_UofMD-landState-3-0_gn_0850-2023.nc")

    # LUH uses from-semantics for transitions (value for 1994 describes what happens from 1994 to 1995)
    # by adding 1 to time we get to-semantics (value for 1994 describes what happens from 1993 to 1994)
    x <- toYearsAndSubset(x, years, offset = +1)

    x <- x["bioh|harv"]
    unit <- "*_bioh: kg C yr-1, *_harv: 1"
  } else {
    stop("subtype must be states, management, transitions or cellArea")
  }

  names(x) <- paste0("y", terra::time(x), "..", sub("_[0-9]+$", "", names(x)))

  return(list(x = x,
              class = "SpatRaster",
              cache = FALSE,
              unit = unit))
}
