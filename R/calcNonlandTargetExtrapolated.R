#' calcNonlandTargetExtrapolated
#'
#' Aggregated low resolution target data is extrapolated to the given years
#' using toolExtrapolate. To extrapolate wood harvest weight (bioh) multiply
#' wood harvest area already extrapolated by calcLandTargetExtrapolated with
#' the historical wood harvest rate in kg C per Mha.
#'
#' @param input character, name of the input data set, currently only "magpie"
#' @param target character, name of the target data set, currently only "luh2mod"
#' @param transitionYears years to which the target data is extrapolated
#' @return extrapolated nonland target data
#'
#' @examples
#' \dontrun{
#'   calcOutput("NonlandTargetExtrapolated", input = "magpie",
#'              target = "luh2mod", transitionYears = seq(2020, 2045, 5))
#' }
#' @author Pascal Sauer
calcNonlandTargetExtrapolated <- function(input, target, transitionYears) {
  xTarget <- calcOutput("NonlandTargetLowRes", input = input, target = target, aggregate = FALSE)

  exTarget <- toolExtrapolate(xTarget[, , grep("_(fertilizer|harvest_weight_type)$", getItems(xTarget, 3))],
                              transitionYears)
  exTarget[exTarget < 0] <- 0

  woody <- c("primf", "secyf", "secmf", "primn", "secnf")

  # calculate kg C per Mha in historical period
  histBioh <- dimSums(xTarget[, , paste0(woody, "_bioh")], 2)
  getItems(histBioh, 3)  <- sub("_bioh$", "_wood_harvest_area", getItems(histBioh, 3))
  histHarvestArea <- dimSums(xTarget[, , paste0(woody, "_wood_harvest_area")], 2)
  kgCPerMha <- histBioh / histHarvestArea
  kgCPerMha[is.nan(kgCPerMha)] <- 0
  stopifnot(is.finite(kgCPerMha))

  # get wood harvest area extrapolation, then apply historical kg C per Mha
  xLand <- calcOutput("LandTargetExtrapolated", input = input, target = target,
                      transitionYears = transitionYears, aggregate = FALSE, supplementary = TRUE)
  stopifnot(xLand$unit == "Mha",
            !is.null(xLand$woodHarvestArea))
  harvestMha <- xLand$woodHarvestArea[, transitionYears, ]

  harvestKgC <- harvestMha * kgCPerMha
  getItems(harvestKgC, 3)  <- sub("_wood_harvest_area$", "_bioh", getItems(harvestKgC, 3))

  roundFuelWood <- c("roundwood_harvest_weight_type", "fuelwood_harvest_weight_type")
  harvestType <- exTarget[, , roundFuelWood]
  harvestType <- harvestType / dimSums(harvestType, 3)
  harvestType[is.nan(harvestType)] <- 0.5
  stopifnot(is.finite(harvestType))
  harvestType <- harvestType * dimSums(harvestKgC, 3)
  exTarget[, , roundFuelWood] <- harvestType

  out <- mbind(xTarget,
               mbind(exTarget, harvestMha, harvestKgC))

  toolExpectLessDiff(dimSums(out[, , paste0(woody, "_bioh")], 3),
                     dimSums(out[, , roundFuelWood], 3),
                     10^5, "Harvest weight types are consistent")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")

  return(list(x = out,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
              min = 0,
              description = "Extrapolated nonland target data for harmonization"))
}
