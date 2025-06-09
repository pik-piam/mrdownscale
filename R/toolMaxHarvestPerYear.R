#' toolMaxHarvestPerYear
#'
#' Calculate the maximum possible harvest area per year
#' (dividing by timestep length for primary land, because it is converted to
#' secondary land after harvest) based on the given land data.
#'
#' @param land magpie object with at least the following categories:
#' c("primf", "secdf", "pltns", "primn", "secdn")
#' @param split if TRUE: split secdf to secyf and secdmf,
#' rename secdf to secnf, and append "_wood_harvest_area" to names
#' @return magpie object with the maximum possible yearly wood harvest area
#'
#' @author Pascal Sauer
toolMaxHarvestPerYear <- function(land, split = TRUE) {
  timestepLength <- new.magpie(years = getYears(land)[-1],
                               fill = diff(getYears(land, as.integer = TRUE)))
  stopifnot(timestepLength > 0)

  land <- land[, , c("primf", "secdf", "pltns", "primn", "secdn")]
  prim <- c("primf", "primn")
  if (split) {
    getItems(land, 3) <- sub("secdf", "secyf", getItems(land, 3))
    getItems(land, 3) <- sub("secdn", "secnf", getItems(land, 3))
    getItems(land, 3) <- paste0(getItems(land, 3), "_wood_harvest_area")
    land <- add_columns(land, "secmf_wood_harvest_area")
    land[, , "secmf_wood_harvest_area"] <- land[, , "secyf_wood_harvest_area"]
    prim <- c("primf_wood_harvest_area", "primn_wood_harvest_area")
  }
  maxHarvest <- setYears(land[, -nyears(land), ], getYears(land)[-1])

  # cannot harvest full primf each year as it is converted to secdf after harvest
  maxHarvest[, , prim] <- maxHarvest[, , prim] / timestepLength
# TODO always return variable names with _wood_harvest_area
# TODO new assumption: can harvest secondary land each year (5 times for 5 year timestep), where to adapt code?
  return(maxHarvest)
}
