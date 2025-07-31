#' calcLandInputRecategorized
#'
#' Computes the land input data in target land categories. Splitting of land
#' categories is performed under use of internal land weights reflecting the
#' prevalence of a certain land category in the given area.
#'
#' Mapping from input to target categories is achieved via a merge of a land input
#' mapping to reference categories and a mapping between land target categories and
#' the same reference categories. Thereby a new source or new target can be supported
#' by supplying a map of that new input and/or target to the reference categories.
#'
#' @param input name of the land input source to be used
#' @param target name of the land target source to be used
#' @author Jan Philipp Dietrich, Pascal Sauer
calcLandInputRecategorized <- function(input, target) {
  x   <- calcOutput("LandInput", input = input, aggregate = FALSE)
  if (input == "witch") {
    # add artificial region numbers so addGeometry works
    mapping <- readSource("WITCH", subtype = "resolutionMapping")
    x <- toolAggregate(x, unique(mapping[, c("witch17", "lowRes")]))
  }

  resolutionMapping <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)
  x <- x[unique(resolutionMapping$lowRes), , ]

  resolutionMapping$cluster <- resolutionMapping$lowRes
  "!# @monitor magpie4:::addGeometry"
  x <- magpie4::addGeometry(x, resolutionMapping)

  map <- toolLandCategoriesMapping(input, target)
  # get weights for disaggregation to reference categories
  ref <- calcOutput("LandCategorizationWeight", map = map, geometry = attr(x, "geometry"),
                    crs = attr(x, "crs"), aggregate = FALSE)
  y   <- toolAggregate(x, map, dim = 3, from = "dataInput", to = "merge", weight = ref)
  out <- toolAggregate(y, map, dim = 3, from = "merge",     to = "dataOutput")

  # rename primf/secdf/primn/secdn equivalent dataOutput categories to primf/secdf/primn/secdn
  # save original names in woodlandMap to rename back in the end
  woodlandMap <- list()
  for (name in c("primn", "secdn", "primf", "secdf")) {
    woodlandMap[name] <- map$dataOutput[map$reference == name]
    if (sum(map$dataOutput == woodlandMap[name]) == 1) {
      stopifnot(name == woodlandMap[name] || !name %in% getItems(out, 3))
      getItems(out, 3) <- sub(woodlandMap[name], name, getItems(out, 3), fixed = TRUE)
    } else {
      woodlandMap[name] <- NA_character_
    }
  }

  if ("primn" %in% getItems(out, 3)) {
    # category remapping does not take into account that primn cannot expand, so redistribute:
    # if totaln shrinks, shrink primn and secdn according to their proportions in the previous timestep
    # if totaln expands, expand only secdn, primn stays constant
    totaln <- dimSums(out[, , c("primn", "secdn")], 3)
    out <- toolReplaceExpansion(out, "primn", "secdn", warnThreshold = 100)

    toolExpectLessDiff(dimSums(out[, , c("primn", "secdn")], 3), totaln, 10^-5,
                       paste("No change in sum of primn and secdn after replacing",
                             "primn expansion with secdn expansion"))
  }

  attr(out, "crs") <- attr(x, "crs")
  attr(out, "geometry") <- attr(x, "geometry")

  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  expectedCategories <- setdiff(map$dataOutput, woodlandMap)
  expectedCategories <- c(expectedCategories, names(woodlandMap)[!is.na(woodlandMap)])
  toolExpectTrue(setequal(getItems(out, dim = 3), expectedCategories), "Land categories match target definition")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-6, "Total areas stay constant over time")
  toolExpectLessDiff(outSum, dimSums(x, dim = 3), 10^-6,
                     "Total areas are not affected by recategorization")
  toolPrimExpansionCheck(out)

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Input data with land categories remapped to categories of target dataset",
              woodlandMap = woodlandMap))
}
