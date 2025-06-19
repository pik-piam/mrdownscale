#' toolMaxHarvestPerYear
#'
#' Calculate the maximum possible harvest area per year based on the
#' given land data.
#'
#' @param land magpie object with at least the following categories:
#' c("primf", "secdf", "pltns", "primn", "secdn")
#' @param split if TRUE: split secdf to secyf and secdmf,
#' rename secdf to secnf, and append "_wood_harvest_area" to names
#' @param timestepAdjust if TRUE: divide values for primary land by timestep
#' length. This makes sense, because once primary land has been harvested,
#' it is converted to secondary land and thus cannot be harvested again.
#' Might introduce unintended spikes when timestep length changes.
#' @return magpie object with the maximum possible yearly wood harvest area
#'
#' @author Pascal Sauer
toolMaxHarvestPerYear <- function(land, split = TRUE, timestepAdjust = TRUE) {
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

  if (timestepAdjust) {
    timestepLength <- new.magpie(years = getYears(land)[-1],
                                 fill = diff(getYears(land, as.integer = TRUE)))
    stopifnot(timestepLength > 0)
    # cannot harvest full primf each year as it is converted to secdf after harvest
    maxHarvest[, , prim] <- maxHarvest[, , prim] / timestepLength
  }
  return(maxHarvest)
}
