#' calcLandReport
#'
#' Convert the downscaled land use data to the format required by the given project.
#'
#' @param outputFormat format in which the outputs should be prepared. Currently,
#' only "ESM" for earth system model compatible input data is available.
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset vector of years to keep in the output dataset
#' @return land use data
#' @author Pascal Sauer
calcLandReport <- function(outputFormat, harmonizationPeriod, yearsSubset) {
  # TODO outputFormat ScenarioMIP
  # TODO rename ESM to RESCUE?
  if (outputFormat == "ESM") {
    native <- calcOutput("LandHighRes", input = "magpie", target = "luh2mod",
                         harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset,
                         aggregate = FALSE)
    cellArea <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    cellArea <- collapseDim(as.magpie(cellArea), 3)

    # calc irrigation shares
    cropTypes <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per")
    cropData <- do.call(mbind, lapply(cropTypes, function(cropType) {
      x <- native[, , grep(cropType, getItems(native, 3))]
      total <- dimSums(x, dim = 3)

      irrigationShare <- dimSums(x[, , grep("irrigated", getItems(x, 3))], dim = 3) / total
      getNames(irrigationShare) <- paste0("irrig_", cropType)
      irrigationShare[total == 0] <- 0 # replace NAs introduced by 0 / 0

      biofuel1stGenShare <- dimSums(x[, , grep("biofuel_1st_gen", getItems(x, 3))], dim = 3) / total
      getNames(biofuel1stGenShare) <- paste0("crpbf_", cropType)
      biofuel1stGenShare[total == 0] <- 0 # replace NAs introduced by 0 / 0

      # multiply by 10000 to convert from Mha to km2, divide by cellArea to get shares
      cellAreaShare <- total * 10000 / cellArea[getItems(cellArea, 1) %in% getItems(x, 1), , ]
      getNames(cellAreaShare) <- cropType

      combined <- mbind(irrigationShare, biofuel1stGenShare, cellAreaShare)
      if (any(grepl("biofuel_2nd_gen", getItems(x, 3)))) {
        biofuel2ndGenShare <- dimSums(x[, , grep("biofuel_2nd_gen", getItems(x, 3))], dim = 3) / total
        getNames(biofuel2ndGenShare) <- paste0("crpbf2_", cropType)
        biofuel2ndGenShare[total == 0] <- 0 # replace NAs introduced by 0 / 0
        combined <- mbind(combined, biofuel2ndGenShare)
      }
      return(combined)
    }))

    totalSecondaryForest <- dimSums(native[, , c("forestry", "secdf")], dim = 3)
    # calculate manaf (managed forest) = forestry share of secondary forest
    # TODO report pltns instead of manaf
    manaf <- native[, , "forestry"] / totalSecondaryForest
    manaf[totalSecondaryForest == 0] <- 0 # replace NAs introduced by 0 / 0
    getNames(manaf) <- "manaf"

    nonCropData <- native[, , c("primf", "primn", "secdf", "secdn", "urban", "pastr", "range")]
    nonCropData[, , "secdf"] <- totalSecondaryForest
    nonCropData <- nonCropData * 10000 / cellArea[getItems(cellArea, 1) %in% getItems(nonCropData, 1), , ]

    out <- mbind(cropData, nonCropData, manaf)

    if (max(out) > 1.0001) {
      toolStatusMessage("warn", paste0("Some shares are > 1 (max: ", max(out), "), setting those to 1"))
    }
    out[out > 1] <- 1

    toolExpectTrue(min(out) >= 0, "All values are >= 0")
    toolExpectTrue(max(out) <= 1, "All shares are <= 1")
    landShares <- c("c3ann", "c3nfx",  "c3per", "c4ann", "c4per", "primf",
                    "primn", "secdf", "secdn", "urban", "pastr", "range")
    toolExpectTrue(max(dimSums(out[, , landShares], 3)) <= 1 + 10^-5,
                   "land shares sum to <= 1 + 10^-5") # if < 1 the rest is likely water
    toolExpectTrue(all(out[, -1, c("primf", "primn")] <= setYears(out[, -nyears(out), c("primf", "primn")],
                                                                  getYears(out[, -1, ]))),
                   "primf and primn are never expanding", falseStatus = "warn")

    return(list(x = out,
                isocountries = FALSE,
                unit = "1",
                min = 0,
                max = 1,
                description = paste("MAgPIE land use data downscaled to LUH2 resolution")))
  } else {
    stop("Can only report for outputFormat = 'ESM'")
  }
}
