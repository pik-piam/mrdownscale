#' calcNonlandReportScenarioMIP
#'
#' Convert the downscaled non-land data to the format required by ScenarioMIP.
#'
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset vector of years to keep in the output dataset
#' @return land use data
#' @author Pascal Sauer
calcNonlandReportScenarioMIP <- function(harmonizationPeriod, yearsSubset) {
  input <- "magpie"
  target <- "luh3"
  x <- calcOutput("NonlandHighRes", input = input, target = target,
                  harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset, aggregate = FALSE)

  cellAreaKm2 <- readSource("LUH3", subtype = "cellArea", convert = FALSE)
  cellAreaKm2 <- as.magpie(cellAreaKm2)
  stopifnot(getItems(x, 1) %in% getItems(cellAreaKm2, 1))
  cellAreaKm2 <- collapseDim(cellAreaKm2[getItems(x, 1), , ], 3)
  # convert from km2 to ha
  cellAreaHa <- cellAreaKm2 * 100
  # convert from km2 to Mha
  cellAreaMha <- cellAreaKm2 / 10000

  # convert from kg yr-1 to kg ha-1 yr-1
  fertl <- x[, , grep("fertilizer$", getNames(x))] / cellAreaHa
  getNames(fertl) <- sub("(.+)_fertilizer$", "fertl_\\1", getNames(fertl))

  # convert from Mha to shares
  harv <- x[, , grep("wood_harvest_area$", getNames(x))] / cellAreaMha
  getNames(harv) <- sub("wood_harvest_area$", "harv", getNames(harv))

  woodTypeShares <- x[, , c("roundwood_harvest_weight_type", "fuelwood_harvest_weight_type")]

  # get rndwd/fulwd shares per country to replace NAs
  coords <- getCoords(woodTypeShares)
  mapping <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)
  mapping <- mapping[, c("x", "y", "country")]
  merged <- merge(coords, mapping, sort = FALSE)
  stopifnot(identical(merged[, c("x", "y")], coords))
  getItems(woodTypeShares, 1, raw = TRUE) <- paste0(getItems(woodTypeShares, 1), ".", merged$country)
  names(dimnames(woodTypeShares))[1] <- "x.y.country"
  countryTotals <- toolAggregate(woodTypeShares, to = "country")
  countryWoodTypeShares <- countryTotals / dimSums(countryTotals, 3)
  countryWoodTypeShares[is.na(countryWoodTypeShares)] <- 0.5 # replace NAs with share 0.5, so sum is still 1

  total <- dimSums(woodTypeShares, 3) # 1269815 cells with total == 0
  woodTypeShares <- woodTypeShares / total # NAs introduced by cells with total == 0
  stopifnot(sum(is.na(woodTypeShares)) == 2 * sum(total == 0))
  fillValuesCountry <- countryWoodTypeShares[getItems(woodTypeShares, "country", full = TRUE), , ]
  getItems(fillValuesCountry, 1, raw = TRUE) <- getItems(woodTypeShares, 1)
  names(dimnames(fillValuesCountry))[[1]] <- "x.y.country"
  woodTypeShares[is.na(woodTypeShares)] <- fillValuesCountry[is.na(woodTypeShares)]
  getNames(woodTypeShares) <- c("rndwd", "fulwd")
  woodTypeShares[, , "rndwd"] <- 1 - woodTypeShares[, , "fulwd"]
  woodTypeShares <- collapseDim(woodTypeShares, 1.3)

  out <- mbind(fertl, harv, woodTypeShares, x[, , grep("bioh$", getNames(x))])

  shares <- grep("(harv|rndwd|fulwd)$", getNames(out), value = TRUE)
  if (max(out[, , shares]) > 1.0001) {
    toolStatusMessage("warn", paste0("Some shares are > 1 (max: ", max(out[, , shares]), "), setting those to 1"))
  }
  out[, , shares][out[, , shares] > 1] <- 1

  toolExpectTrue(min(out) >= 0, "All values are >= 0")
  toolExpectTrue(max(out[, , shares]) <= 1, "All shares are <= 1")
  toolExpectLessDiff(dimSums(out[, , c("rndwd", "fulwd")], 3), 1, 10^-5, "shares sum to 1 (rndwd + fulwd)")
  toolExpectTrue(!any(is.na(out)), "No NAs in output")

  return(list(x = out,
              isocountries = FALSE,
              min = 0,
              unit = "rndwd & fulwd: 1; bioh: kg C yr-1; harv: 1; fertl: kg ha-1 yr-1",
              description = "Downscaled nonland data report for use in ESMs"))
}
