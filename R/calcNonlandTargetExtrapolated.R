#' calcNonlandTargetExtrapolated
#'
#' Aggregated low resolution target data is extrapolated to the given years
#' using toolExtrapolate. To extrapolate wood harvest weight (bioh) multiply
#' wood harvest area already extrapolated by calcLandTargetExtrapolated with
#' the historical wood harvest rate in kg C per Mha.
#'
#' @param input character, name of the input data set, currently only "magpie"
#' @param target character, name of the target data set, currently only "luh2mod"
#' @param harmonizationPeriod Two integer values, will extrapolate to all years
#' present in input data between harmonization start and end year
#' @return extrapolated nonland target data
#'
#' @examples
#' \dontrun{
#'   calcOutput("NonlandTargetExtrapolated", input = "magpie",
#'              target = "luh2mod", transitionYears = seq(2020, 2045, 5))
#' }
#' @author Pascal Sauer
calcNonlandTargetExtrapolated <- function(input, target, harmonizationPeriod) {
  xInput <- calcOutput("NonlandInputRecategorized", input = input, target = target, aggregate = FALSE)
  inputYears <- getYears(xInput, as.integer = TRUE)
  transitionYears <- inputYears[inputYears > harmonizationPeriod[1] & inputYears < harmonizationPeriod[2]]

  xTarget <- calcOutput("NonlandTargetLowRes", input = input, target = target, aggregate = FALSE)

  exTarget <- toolExtrapolate(xTarget[, , c("fertilizer", "harvest_weight_type")], transitionYears)
  exTarget[exTarget < 0] <- 0

  # calculate kg C per Mha in historical period
  histBioh <- dimSums(xTarget[, , "bioh"], 2)
  histHarvestArea <- dimSums(xTarget[, , "wood_harvest_area"], 2)
  kgCPerMha <- 1 / histHarvestArea * collapseDim(histBioh) # TODO order is important here, need histHarvestArea before histBioh
  kgCPerMha[is.nan(kgCPerMha)] <- 0
  stopifnot(is.finite(kgCPerMha))

  # get wood harvest area extrapolation, then apply historical kg C per Mha
  xLand <- calcOutput("LandTargetExtrapolated", input = input, target = target,
                      harmonizationPeriod = harmonizationPeriod, aggregate = FALSE, supplementary = TRUE)
  stopifnot(xLand$unit == "Mha",
            !is.null(xLand$woodHarvestArea))
  harvestMha <- xLand$woodHarvestArea[, transitionYears, ]

  stopifnot(setequal(getItems(harvestMha, 3), getItems(kgCPerMha, 3)))
  harvestKgC <- harvestMha * kgCPerMha
  getItems(harvestKgC, 3.1)  <- sub("wood_harvest_area", "bioh", getItems(harvestKgC, 3.1))

  harvestType <- exTarget[, , "harvest_weight_type"]
  harvestType <- harvestType / dimSums(harvestType, 3)
  harvestType[is.nan(harvestType)] <- 0.5
  stopifnot(is.finite(harvestType))
  harvestType <- harvestType * dimSums(harvestKgC, 3)
  exTarget[, , "harvest_weight_type"] <- harvestType

  out <- mbind(xTarget,
               mbind(exTarget, harvestMha, harvestKgC))

  toolExpectLessDiff(dimSums(out[, , "bioh"], 3),
                     dimSums(out[, , "harvest_weight_type"], 3),
                     10^5, "Harvest weight types are consistent")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")
  toolExpectLessDiff(out[, getYears(out, as.integer = TRUE) <= harmonizationPeriod[1], ],
                     xTarget[, getYears(xTarget, as.integer = TRUE) <= harmonizationPeriod[1], ],
                     10^-4, "Returning reference data before harmonization period")

  cropMha <- toolAggregateCropland(xLand$x, keepOthers = FALSE)
  # convert from Tg yr-1 to kg ha-1 yr-1
  fertilizerKgPerHa <- out[, , "fertilizer"] / cropMha * (10^9 / 10^6)
  fertilizerKgPerHa[is.nan(fertilizerKgPerHa)] <- 0
  toolExpectTrue(max(fertilizerKgPerHa) <= 1200,
                 paste0("Fertilizer application is <= 1200 kg ha-1 yr-1 (max: ",
                        signif(max(fertilizerKgPerHa), 3), ")"))

  return(list(x = out,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: Tg yr-1",
              min = 0,
              description = "Extrapolated nonland target data for harmonization"))
}
