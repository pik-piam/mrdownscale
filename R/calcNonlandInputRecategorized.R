#' calcNonlandInputRecategorized
#'
#' Harmonize categories by mapping nonland input data categories to the categories of the nonland target dataset.
#' See \code{\link{calcLandInputRecategorized}} for an explanation of the mapping procedure.
#'
#' Report and discard wood harvest area if there is zero wood harvest (bioh) or vice versa.
#' If fertilizer application is larger than 600 kg ha-1 yr-1 it is set to 600 kg ha-1 yr-1.
#'
#' @param input name of the input dataset, currently only "magpie"
#' @param target name of the target dataset, currently only "luh2"
#' @param youngShareWoodHarvestArea share of wood harvest area taken from young (instead of mature) secondary forest;
#' default value is based on LUH value from 2014; used to disaggregate wood harvest area from secondary forest to
#' secondary young and mature forest
#' @param youngShareWoodHarvestWeight analogue to youngShareWoodHarvestArea for wood harvest weight instead of area
#' @return nonland data with target categories
#' @author Pascal Sauer
calcNonlandInputRecategorized <- function(input, target, youngShareWoodHarvestArea = 0.95,
                                          youngShareWoodHarvestWeight = 0.5) {
  x <- calcOutput("NonlandInput", input = input, aggregate = FALSE)
  resolutionMapping <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)
  x <- x[unique(resolutionMapping$lowRes), , ]

  resolutionMapping$cluster <- resolutionMapping$lowRes
  "!# @monitor magpie4:::addGeometry"
  x <- magpie4::addGeometry(x, resolutionMapping)
  crs <- attr(x, "crs")
  geometry <- attr(x, "geometry")

  # rename pltns category
  map <- toolLandCategoriesMapping(input, target)
  pltnsInInput <- map[map$dataOutput == "pltns", "dataInput"]
  stopifnot(length(pltnsInInput) == 1)
  getItems(x, 3, raw = TRUE) <- sub(pltnsInInput, "pltns", getItems(x, 3))

  if (target %in% c("luh2", "luh2mod")) {
    # aggregate secdforest and pltns, because LUH2 does not report wood harvest for pltns
    x[, , "secdforest"] <- add_dimension(dimSums(x[, , c("secdforest", "pltns")], "data"),
                                         3.2, "data", "secdforest")
    x <- x[, , "pltns", invert = TRUE]
  }

  # disaggregate wood harvest weight and area from secondary forest to secondary young and mature forest
  youngWeight <- youngShareWoodHarvestWeight * x[, , "wood_harvest_weight.secdforest"]
  getNames(youngWeight) <- "wood_harvest_weight.secyf"
  matureWeight <- x[, , "wood_harvest_weight.secdforest"] - youngWeight
  getNames(matureWeight) <- "wood_harvest_weight.secmf"

  youngArea <- youngShareWoodHarvestArea * x[, , "wood_harvest_area.secdforest"]
  getNames(youngArea) <- "wood_harvest_area.secyf"
  matureArea <- x[, , "wood_harvest_area.secdforest"] - youngArea
  getNames(matureArea) <- "wood_harvest_area.secmf"

  x <- mbind(youngWeight, matureWeight, youngArea, matureArea, x[, , "secdforest", invert = TRUE])

  # map fertilizer using weights from land categorization
  fertilizerTg <- collapseDim(x[, , "fertilizer"])

  ref <- calcOutput("LandCategorizationWeight", map = map, geometry = geometry, crs = crs, aggregate = FALSE)

  # sum up weights for irrigated/rainfed
  irrigatedNames <- grep("irrigated", getItems(ref, 3), value = TRUE)
  irrigated <- ref[, , irrigatedNames]
  getItems(irrigated, 3) <- gsub("_irrigated", "", irrigatedNames)

  rainfedNames <- gsub("irrigated", "rainfed", irrigatedNames)
  rainfed <- ref[, , rainfedNames]
  getItems(rainfed, 3) <- gsub("_rainfed", "", rainfedNames)
  ref <- mbind(ref[, , c(irrigatedNames, rainfedNames), invert = TRUE], irrigated + rainfed)
  stopifnot(!grepl("irrigated|rainfed", getItems(ref, 3)))

  map$reference <- sub(", irrigated", "", map$reference)
  map$dataInput <- sub("_irrigated", "", map$dataInput)
  map$dataOutput <- sub("_irrigated", "", map$dataOutput)
  map$merge <- gsub("_irrigated", "", map$merge)
  mapFertilizer <- map[map$dataInput %in% getItems(fertilizerTg, 3), ]

  fertilizerMerge <- toolAggregate(fertilizerTg, mapFertilizer, dim = 3, from = "dataInput", to = "merge",
                                   weight = ref[, , unique(mapFertilizer$merge)])
  fertilizerTg <- toolAggregate(fertilizerMerge, mapFertilizer, dim = 3, from = "merge", to = "dataOutput")
  fertilizerTg[, , "c3per"] <- fertilizerTg[, , "c3per"] + fertilizerTg[, , "c3per_biofuel_2nd_gen"]
  fertilizerTg[, , "c4per"] <- fertilizerTg[, , "c4per"] + fertilizerTg[, , "c4per_biofuel_2nd_gen"]
  fertilizerTg <- fertilizerTg[, , c("c3per_biofuel_2nd_gen", "c4per_biofuel_2nd_gen"), invert = TRUE]

  landMha <- calcOutput("LandInputRecategorized", input = input, target = target, aggregate = FALSE)
  landMha <- toolAggregateCropland(landMha)[, , getItems(fertilizerTg, 3)]
  stopifnot(setequalDims(fertilizerTg, landMha))
  # convert from Tg Mha-1 yr-1 to kg ha-1 yr-1 (relative to specific cropland, e.g. c3ann)
  fertilizer <- fertilizerTg * 10^9 / (landMha * 10^6)
  fertilizer[is.nan(fertilizer)] <- 0

  toolExpectTrue(all(fertilizer <= 1200),
                 paste0("Fertilizer application is <= 1200 kg ha-1 yr-1 (max: ",
                        signif(max(fertilizer), 3), ")"))
  if (any(fertilizer > 1200)) {
    toolStatusMessage("note", paste0("Fertilizer is capped at 1200 kg ha-1 yr-1"))
  }
  fertilizer[fertilizer > 1200] <- 1200

  fertilizer <- add_dimension(fertilizer, 3.1, "category", "fertilizer")
  x <- mbind(fertilizer, x[, , "fertilizer", invert = TRUE])

  # map other wood harvest to primn and secdn using land as weights
  mapOther <- map[map$dataInput == "other", ]

  otherWoodHarvestWeight <- collapseDim(x[, , "wood_harvest_weight.other"], dim = 3.1)
  otherWoodHarvestWeight <- toolAggregate(otherWoodHarvestWeight, mapOther, dim = 3, from = "dataInput", to = "merge",
                                          weight = ref[, , unique(mapOther$merge)])
  otherWoodHarvestWeight <- toolAggregate(otherWoodHarvestWeight, mapOther, dim = 3, from = "merge", to = "dataOutput")
  otherWoodHarvestWeight <- add_dimension(otherWoodHarvestWeight, 3.1, "category", "wood_harvest_weight")

  otherWoodHarvestArea <- collapseDim(x[, , "wood_harvest_area.other"], dim = 3.1)
  otherWoodHarvestArea <- toolAggregate(otherWoodHarvestArea, mapOther, dim = 3, from = "dataInput", to = "merge",
                                        weight = ref[, , unique(mapOther$merge)])
  otherWoodHarvestArea <- toolAggregate(otherWoodHarvestArea, mapOther, dim = 3, from = "merge", to = "dataOutput")
  otherWoodHarvestArea <- add_dimension(otherWoodHarvestArea, 3.1, "category", "wood_harvest_area")

  x <- mbind(otherWoodHarvestWeight, otherWoodHarvestArea, x[, , "other", invert = TRUE])

  getNames(x) <- paste0(getItems(x, 3.2, full = TRUE), "_", getItems(x, 3.1, full = TRUE))
  getNames(x) <- sub("_wood_harvest_weight_type", "_harvest_weight_type", getNames(x))
  getNames(x) <- sub("_wood_harvest_weight", "_bioh", getNames(x))
  getNames(x) <- sub("secdn", "secnf", getNames(x))
  getNames(x) <- sub("primforest", "primf", getNames(x))
  x <- collapseDim(x)
  getSets(x)["d3.1"] <- "data"

  biohCategories <- paste0(c("primf", "primn", "secmf", "secyf", "secnf", "pltns"), "_bioh")
  if (target %in% c("luh2", "luh2mod")) {
    biohCategories <- setdiff(biohCategories, "pltns_bioh")
  }
  # wha = wood harvest area
  whaCategories <- sub("bioh", "wood_harvest_area", biohCategories)

  biohRenamed <- x[, , biohCategories]
  getItems(biohRenamed, 3) <- sub("_bioh", "_wood_harvest_area", getItems(biohRenamed, 3))
  problematic <- x[, , whaCategories] > 0 & biohRenamed == 0
  stopifnot(setequalDims(x[, , whaCategories], problematic))
  if (any(problematic)) {
    maxWha <- max(x[, , whaCategories][problematic])
    toolStatusMessage(if (maxWha > 10^-10) "warn" else "note",
                      paste0("setting wood harvest area to zero where corresponding bioh is zero ",
                             "(max such wood harvest area: ",
                             signif(maxWha, 3), " Mha yr-1)"))
    x[, , whaCategories][problematic] <- 0
  }

  whaRenamed <- x[, , whaCategories]
  getItems(whaRenamed, 3) <- sub("_wood_harvest_area", "_bioh", getItems(whaRenamed, 3))
  problematic <- x[, , biohCategories] > 0 & whaRenamed == 0
  stopifnot(setequalDims(x[, , biohCategories], problematic))
  if (any(problematic)) {
    maxBioh <- max(x[, , biohCategories][problematic])
    toolStatusMessage(if (maxBioh > 1) "warn" else "note",
                      paste0("setting bioh to zero where corresponding wood harvest area is zero (max such bioh: ",
                             signif(maxBioh, 3), " kg C yr-1)"))
    x[, , biohCategories][problematic] <- 0
  }

  attr(x, "crs") <- crs
  attr(x, "geometry") <- geometry

  # check data for consistency
  toolExpectTrue(identical(unname(getSets(x)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")

  toolExpectTrue(setequal(getNames(x),
                          c(whaCategories,
                            biohCategories,
                            paste0(c("roundwood", "fuelwood"), "_harvest_weight_type"),
                            paste0(c("c3ann", "c4ann", "c3per", "c4per", "c3nfx"), "_fertilizer"))),
                 "Nonland categories match target definition")
  toolExpectTrue(all(x >= 0), "All values are >= 0")

  whaRenamed <- x[, , whaCategories]
  getItems(whaRenamed, 3) <- sub("_wood_harvest_area", "_bioh", getItems(whaRenamed, 3))
  toolExpectTrue(all(whaRenamed > 0 | x[, , biohCategories] == 0),
                 "If bioh > 0 then wood harvest area > 0")
  toolExpectTrue(all(whaRenamed == 0 | x[, , biohCategories] > 0),
                 "If wood harvest area > 0 then bioh > 0")

  return(list(x = x,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg ha-1 yr-1",
              min = 0,
              description = "Input data with nonland categories remapped to categories of target dataset"))
}
# TODO adapt to new fertilizer unit