#' calcNonlandTarget
#'
#' Prepare the high resolution nonland target dataset for
#' harmonization and downscaling, checking data for consistency before returning.
#'
#' @param target name of the target dataset, currently only "luh2" and "luh2mod" are supported
#' @return nonland target data
#' @author Pascal Sauer
calcNonlandTarget <- function(target = "luh2mod") {
  if (target %in% c("luh2", "luh2mod")) {
    management <- readSource("LUH2v2h", subtype = "management", convert = FALSE)

    # need absolute values for downscaling, fertl_* is in kg ha-1 yr-1, convert to kg yr-1
    cellArea <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    # convert from km2 to ha
    cellAreaHa <- cellArea * 100
    fertilizer <- management["fertl"] * cellAreaHa
    terra::units(fertilizer) <- "kg yr-1"
    names(fertilizer) <- paste0(sub("fertl_", "", names(fertilizer)), "_fertilizer")

    transitions <- readSource("LUH2v2h", subtype = "transitions", convert = FALSE)
    states <- readSource("LUH2v2h", subtype = "states")

    # convert from shares to Mha yr-1
    # assuming secmf and secyf are both given relative to total secdf area
    woodHarvestArea <- c(transitions["primf_harv"] * states["primf"],
                         transitions["primn_harv"] * states["primn"],
                         transitions["secmf_harv"] * states["secdf"],
                         transitions["secyf_harv"] * states["secdf"],
                         transitions["secnf_harv"] * states["secdn"])
    names(woodHarvestArea) <- paste0(sub("_harv", "", names(woodHarvestArea)), "_wood_harvest_area")
    terra::units(woodHarvestArea) <- "Mha yr-1"

    woodHarvestWeight <- transitions["bioh"]
    # replace negative weight of wood harvest with 0
    woodHarvestWeight <- terra::classify(woodHarvestWeight, cbind(-Inf, 0, 0))
    terra::units(woodHarvestWeight) <- "kg C yr-1"

    years <- unique(terra::time(woodHarvestWeight))
    woodHarvestWeightType <- do.call(c, lapply(years, function(year) {
      total <- sum(woodHarvestWeight[[terra::time(woodHarvestWeight) == year]])
      roundwood <- total * management[[paste0("y", year, "..rndwd")]]
      names(roundwood) <- paste0("y", year, "..roundwood_harvest_weight_type")
      fuelwood <- total * management[[paste0("y", year, "..fulwd")]]
      names(fuelwood) <- paste0("y", year, "..fuelwood_harvest_weight_type")
      return(c(roundwood, fuelwood))
    }))
    terra::units(woodHarvestWeightType) <- "kg C yr-1"

    out <- c(woodHarvestArea, woodHarvestWeight, woodHarvestWeightType, fertilizer)
    terra::time(out, tstep = "years") <- as.integer(sub("^y([0-9]+).+", "\\1", names(out)))

    return(list(x = out,
                class = "SpatRaster",
                unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
                description = "Nonland target data for data harmonization"))
  } else {
    stop("Unsupported output type \"", target, "\"")
  }
}
