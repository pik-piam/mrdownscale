#' calcNonlandInputRecategorized
#'
#' Harmonize categories by mapping nonland input data categories to the categories of the nonland target dataset.
#' See \code{\link{calcLandInputRecategorized}} for an explanation of the mapping procedure.
#'
#' Report and discard wood harvest area if there is zero wood harvest (bioh) or vice versa.
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
  landMha <- calcOutput("LandInputRecategorized", input = input, target = target, aggregate = FALSE)
  nonlandInput <- calcOutput("NonlandInput", input = input, aggregate = FALSE)
  resolutionMapping <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)
  x <- nonlandInput[unique(resolutionMapping$lowRes), , ]

  resolutionMapping$cluster <- resolutionMapping$lowRes
  "!# @monitor magpie4:::addGeometry"
  x <- magpie4::addGeometry(x, resolutionMapping)
  crs <- attr(x, "crs")
  geometry <- attr(x, "geometry")

  map <- toolLandCategoriesMapping(input, target)

  # rename pltns category
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

  ref <- calcOutput("LandCategorizationWeight", map = map, geometry = geometry, crs = crs, aggregate = FALSE)
  x <- mbind(x[, , "fertilizer", invert = TRUE],
             toolRecategorizeFertilizer(x[, , "fertilizer"], ref, map, landMha))

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

  getNames(x) <- sub("wood_harvest_weight_type", "harvest_weight_type", getNames(x))
  getNames(x) <- sub("wood_harvest_weight", "bioh", getNames(x))
  getNames(x) <- sub("secdn", "secnf", getNames(x))
  getNames(x) <- sub("primforest", "primf", getNames(x))

  biohRenamed <- x[, , "bioh"]
  getItems(biohRenamed, 3.1) <- sub("bioh", "wood_harvest_area", getItems(biohRenamed, 3.1))
  problematic <- x[, , "wood_harvest_area"] > 0 & biohRenamed == 0
  stopifnot(setequalDims(x[, , "wood_harvest_area"], problematic))
  if (any(problematic)) {
    maxWha <- max(x[, , "wood_harvest_area"][problematic])
    toolStatusMessage(if (maxWha > 10^-10) "warn" else "note",
                      paste0("setting wood harvest area to zero where corresponding bioh is zero ",
                             "(max such wood harvest area: ",
                             signif(maxWha, 3), " Mha yr-1)"))
    x[, , "wood_harvest_area"][problematic] <- 0
  }

  whaRenamed <- x[, , "wood_harvest_area"]
  getItems(whaRenamed, 3.1) <- sub("wood_harvest_area", "bioh", getItems(whaRenamed, 3.1))
  problematic <- x[, , "bioh"] > 0 & whaRenamed == 0
  stopifnot(setequalDims(x[, , "bioh"], problematic))
  if (any(problematic)) {
    maxBioh <- max(x[, , "bioh"][problematic])
    toolStatusMessage(if (maxBioh > 1) "warn" else "note",
                      paste0("setting bioh to zero where corresponding wood harvest area is zero (max such bioh: ",
                             signif(maxBioh, 3), " kg C yr-1)"))
    x[, , "bioh"][problematic] <- 0
  }

  attr(x, "crs") <- crs
  attr(x, "geometry") <- geometry

  # check data for consistency
  toolExpectTrue(identical(unname(getSets(x)), c("region", "id", "year", "category", "data")),
                 "Dimensions are named correctly")

  stopifnot(length(setdiff(getItems(x, 1), getItems(nonlandInput, 1))) == 0)
  omitted <- setdiff(getItems(nonlandInput, 1), getItems(x, 1))
  omittedMessage <- paste0(" (omitted: ",
                           paste0(omitted, collapse = ", "),
                           ")")
  toolExpectTrue(length(omitted) == 0, paste0("Using full spatial dimension of input data",
                                              if (length(omitted) > 0) omittedMessage))

  woodland <- c("primf", "primn", "secmf", "secyf", "secnf",
                if (!target %in% c("luh2", "luh2mod")) "pltns")
  toolExpectTrue(setequal(getItems(x, 3),
                          c(paste0("wood_harvest_area.", woodland),
                            paste0("bioh.", woodland),
                            paste0("harvest_weight_type.", c("roundwood", "fuelwood")),
                            paste0("fertilizer.", c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")))),
                 "Nonland categories match target definition")
  toolExpectTrue(all(x >= 0), "All values are >= 0")

  whaRenamed <- x[, , "wood_harvest_area"]
  getItems(whaRenamed, 3.1) <- sub("wood_harvest_area", "bioh", getItems(whaRenamed, 3.1))
  toolExpectTrue(all(whaRenamed > 0 | x[, , "bioh"] == 0),
                 "If bioh > 0 then wood harvest area > 0")
  toolExpectTrue(all(whaRenamed == 0 | x[, , "bioh"] > 0),
                 "If wood harvest area > 0 then bioh > 0")

  toolExpectLessDiff(dimSums(x[, , "fertilizer"], 3),
                     dimSums(nonlandInput[unique(resolutionMapping$lowRes), , "fertilizer"], 3),
                     10^-5, "Total fertilizer is not changed by recategorization")
  toolCheckFertilizer(x[, , "fertilizer"], landMha)

  return(list(x = x,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: Tg yr-1",
              min = 0,
              description = "Input data with nonland categories remapped to categories of target dataset"))
}

toolRecategorizeFertilizer <- function(x, ref, map, landMha) {
  # map fertilizer using weights from land categorization
  fertilizer <- collapseDim(x)

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
  mapFertilizer <- map[map$dataInput %in% getItems(fertilizer, 3), ]

  fertilizerMerge <- toolAggregate(fertilizer, mapFertilizer, dim = 3, from = "dataInput", to = "merge",
                                   weight = ref[, , unique(mapFertilizer$merge)])
  fertilizer <- toolAggregate(fertilizerMerge, mapFertilizer, dim = 3, from = "merge", to = "dataOutput")

  fertilizer[, , "c3per"] <- fertilizer[, , "c3per"] + fertilizer[, , "c3per_biofuel_2nd_gen"]
  fertilizer[, , "c4per"] <- fertilizer[, , "c4per"] + fertilizer[, , "c4per_biofuel_2nd_gen"]
  fertilizer <- fertilizer[, , c("c3per_biofuel_2nd_gen", "c4per_biofuel_2nd_gen"), invert = TRUE]
  fertilizer <- add_dimension(fertilizer, 3.1, "category", "fertilizer")
  perHa <- toolFertilizerKgPerHa(fertilizer, landMha)
  perHaCapped <- perHa
  perHaCapped[perHaCapped > 1200] <- 1190
  # TODO scale others to make up for this
  fertilizer <- toolFertilizerTg(perHaCapped, landMha)
  return(fertilizer)
}

# TODO [!] Check failed: Total fertilizer is not changed by recategorization (maxdiff = 0.01, threshold = 1e-05)

# TODO ~ [WARNING] setting bioh to zero where corresponding wood harvest area is zero (max such bioh: 11 kg C yr-1)
