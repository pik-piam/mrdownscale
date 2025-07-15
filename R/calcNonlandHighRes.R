#' calcNonlandHighRes
#'
#' Calculate a high resolution dataset from the low resolution input dataset
#' and high resolution data.
#'
#' Wood harvest area is disaggregated using the maximum possible harvest per
#' year which is based on the downscaled land data. Bioh
#' is disaggregated using the just disaggregated wood harvest area as weight.
#' Fertilizer is disaggregated using the downscaled land data. Harvest weight
#' type is disaggregated using the nonland target data in the first year of
#' the harmonization period as weight.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @param target name of a target dataset, currently only "luh2mod"
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset vector of years to keep in the output dataset
#' @param harmonization name of harmonization method, see \code{\link{toolGetHarmonizer}}
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @return downscaled nonland data
#'
#' @examples
#' \dontrun{
#'   calcOutput("NonlandHighRes", input = "magpie", target = "luh2mod",
#'              harmonizationPeriod = c(2015, 2050), yearsSubset = 2015:2100,
#'              harmonization = "fade", downscaling = "magpieClassic")
#' }
#' @author Pascal Sauer
calcNonlandHighRes <- function(input, target, harmonizationPeriod, yearsSubset, harmonization, downscaling) {
  x <- calcOutput("NonlandHarmonized", input = input, target = target,
                  harmonizationPeriod = harmonizationPeriod, harmonization = harmonization, aggregate = FALSE)
  x <- x[, getYears(x, as.integer = TRUE) %in% yearsSubset, ]

  futureYears <- getYears(x, as.integer = TRUE)
  futureYears <- futureYears[futureYears > harmonizationPeriod[1]]

  resmap <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)

  landHighRes <- calcOutput("LandHighRes", input = input, target = target,
                            harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset,
                            harmonization = harmonization, downscaling = downscaling, aggregate = FALSE)

  harvestArea <- x[, futureYears, "wood_harvest_area"]
  harvestAreaWeight <- toolMaxHarvestPerYear(landHighRes)[, futureYears, ] + 10^-30
  harvestAreaDownscaled <- toolAggregate(harvestArea, resmap, weight = harvestAreaWeight,
                                         from = "lowRes", to = "cell", dim = 1)


  bioh <- x[, futureYears, "bioh"]
  weightBioh <- harvestAreaDownscaled + 10^-30
  getItems(weightBioh, 3.1) <- sub("wood_harvest_area$", "bioh", getItems(weightBioh, 3.1))
  stopifnot(getItems(weightBioh, 3) == getItems(bioh, 3))
  biohDownscaled <- toolAggregate(bioh, resmap, weight = weightBioh, from = "lowRes", to = "cell", dim = 1)

  fertilizer <- x[, futureYears, "fertilizer"]
  weightFertilizer <- toolAggregateCropland(landHighRes)[, futureYears, getItems(fertilizer, 3.2)] + 10^-30
  weightFertilizer <- add_dimension(weightFertilizer, add = "category", nm = "fertilizer")
  fertilizerDownscaled <- toolAggregate(fertilizer, resmap, weight = weightFertilizer,
                                        from = "lowRes", to = "cell", dim = 1)

  nonlandTarget <- calcOutput("NonlandTarget", target = target, aggregate = FALSE)
  nonlandTarget <- as.magpie(nonlandTarget[[terra::time(nonlandTarget) <= harmonizationPeriod[1] &
                                              terra::time(nonlandTarget) %in% yearsSubset]])
  getItems(nonlandTarget, 3, raw = TRUE) <- sub("^(.+?)_(.+)$", "\\2.\\1", getItems(nonlandTarget, 3))
  names(dimnames(nonlandTarget))[3] <- "category.data"

  harvestType <- x[, futureYears, "harvest_weight_type"]
  weightHarvestType <- nonlandTarget[, harmonizationPeriod[1], "harvest_weight_type"]
  weightHarvestType <- collapseDim(weightHarvestType) + 10^-30
  harvestTypeDownscaled <- toolAggregate(harvestType, resmap, weight = weightHarvestType,
                                         from = "lowRes", to = "cell", dim = 1)


  out <- mbind(nonlandTarget,
               mbind(harvestAreaDownscaled, biohDownscaled, fertilizerDownscaled, harvestTypeDownscaled))

  # checks
  fertilizerInput <- calcOutput("NonlandInputRecategorized", input = input, target = target, aggregate = FALSE)

  inSum <- dimSums(x[, , "fertilizer", invert = TRUE], dim = 1)
  outSum <- dimSums(out[, , "fertilizer", invert = TRUE], dim = 1)
  stopifnot(identical(getYears(inSum), getYears(outSum)),
            setequal(getItems(inSum, 3), getItems(outSum, 3)))
  toolExpectLessDiff(inSum, outSum, 10^-3,
                     "No significant global sum difference per category before and after downscaling")

  # for years after harmonization make sure that total global fertilizer matches input
  years <- getYears(out, TRUE)
  years <- years[years >= harmonizationPeriod[2]]
  fertilizerInput <- dimSums(fertilizerInput[, years, "fertilizer"], 1)
  toolExpectLessDiff(fertilizerInput, dimSums(out[, years, "fertilizer"], 1), 10^-5,
                     "Total global fertilizer after harmonization period matches input data")
  toolCheckFertilizer(out[, , "fertilizer"], landHighRes)

  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(x, dim = 3)),
                 "Nonland categories remain unchanged")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")

  toolCheckWoodHarvestArea(out[, , "wood_harvest_area"], landHighRes, harmonizationPeriod[1])

  return(list(x = out,
              min = 0,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: Tg yr-1",
              description = "Downscaled nonland data"))
}

# TODO ~ [!] Check failed: Fertilizer application is <= 1200 kg ha-1 yr-1 (max: Inf)
