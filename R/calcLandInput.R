#' calcLandInput
#'
#' Prepare the land input data for the category mapping, checking data for consistency before returning.
#' All "Land" functions deal with area data, as opposed to "Nonland" functions which deal with non-area
#' data such as the amount of applied fertilizer. These are treated differently, because for area
#' data other constraints apply, e.g. the total area must be constant over time.
#'
#' input = "magpie": includes the land use categories
#' past (pasture, including rangeland), forestry (managed forest plantations),
#' primforest, secdforest, urban, other (other land) and many
#' specific crop types. Furthermore, 1st gen biofuel is
#' added and filled with zeros. 1st gen biofuel is only modeled implicitly in
#' magpie via demand, and because of trade it is unclear on what area 1st gen
#' biofuel is grown, also 1st gen biofuel is quickly phased out in magpie, so
#' we fill biofuel_1st_gen with zeros and rely on the harmonization to produce
#' a plausible 1st gen biofuel time series.
# TODO witch documentation
#'
#' @param input name of an input dataset, options: "magpie", "witch"
#' @return land input data
#' @author Jan Philipp Dietrich, Pascal Sauer
calcLandInput <- function(input) {
  if (input == "magpie") {
    land <- readSource("MagpieFulldataGdx", subtype = "land")
    crop <- readSource("MagpieFulldataGdx", subtype = "crop")
    getItems(crop, dim = 3.1, full = TRUE) <- sub("\\.", "_", getItems(crop, dim = 3, full = TRUE))
    getItems(crop, dim = 3.2) <- NULL

    toolExpectLessDiff(land[, , "crop_area"],
                       dimSums(crop, dim = 3),
                       10^-5, "sum over all crops equals crop_area")

    # in case we have no crop_area, but fallow and/or treecover: assign to bio energy trees
    fallowTreecover <- dimSums(land[, , c("crop_fallow", "crop_treecover")], 3)
    zeroCropArea <- collapseDim(land[, , "crop_area"]) == 0
    # keep fallow plus treecover only where crop_area is zero
    fallowTreecover <- fallowTreecover * ifelse(zeroCropArea, 1, 0)
    land[, , c("crop_fallow", "crop_treecover")] <- ifelse(zeroCropArea, 0,
                                                           land[, , c("crop_fallow", "crop_treecover")])
    crop[, , "betr_rainfed"] <- crop[, , "betr_rainfed"] + fallowTreecover
    land[, , "crop_area"] <- land[, , "crop_area"] + fallowTreecover

    # scale crop to take up the whole area of crop_area + crop_fallow + crop_treecover
    # need this to report for ScenarioMIP/LUH-format, might not want this for other applications
    totalCrop <- dimSums(land[, , c("crop_area", "crop_fallow", "crop_treecover")], 3)
    scalingFactors <- totalCrop / dimSums(crop, dim = 3)
    scalingFactors[is.nan(scalingFactors)] <- 1
    stopifnot(1 <= scalingFactors, scalingFactors < Inf)
    crop <- crop * scalingFactors

    toolExpectLessDiff(dimSums(crop, dim = 3), totalCrop, 10^-5,
                       "after scaling, sum over all crops equals crop_area + crop_fallow + crop_treecover")

    out <- mbind(land[, , c("crop_area", "crop_fallow", "crop_treecover"), invert = TRUE], crop)

    # see note in the documentation of this function
    out <- add_columns(out, "biofuel_1st_gen", fill = 0)

    expectedSetNames <- c("region", "id", "year", "data")
    expectedCategories <- unique(toolGetMapping("referenceMappings/magpie.csv", where = "mrdownscale")$data)
    primf <- "primforest"
  } else if (input == "witch") {
    x <- readSource("WITCH")
    stopifnot(length(unique(x$model)) == 1,
              length(unique(x$scenario)) == 1)
    x <- x[x$variable %in% c("primf", "secdf", "pltns", "primn", "secdn", "pastr",
                             "c3ann", "irrig_c3ann", "c4per"), ]

    # check irrig_c3ann is fraction of crop area, need to handle that later
    stopifnot(identical(unique(x[x$variable == "irrig_c3ann", ]$units),
                        "fraction of crop area"))
    x[x$variable == "irrig_c3ann", ]$units <- "fraction of grid cell"
    stopifnot(identical(unique(x$units), "fraction of grid cell"))

    x <- x[, c("region", "year", "variable", "value")]
    out <- as.magpie(x, spatial = "region", temporal = "year")
    out <- out["world", , , invert = TRUE]

    if (anyNA(out)) {
      warning("NAs detected, replacing with 0.")
      out[is.na(out)] <- 0
    }
    if (min(out) < 0) {
      warning("Negative values detected, replacing with 0.")
      # TODO this leads to sum of shares > 1, scaling below won't be needed once this is fixed
      out[out < 0] <- 0
    }
    names(dimnames(out))[3] <- "data"

    out <- add_columns(out, c("c3ann_irrigated", "c3ann_rainfed"))

    # convert from fraction of crop area to fraction of grid cell
    stopifnot(out[, , "irrig_c3ann"] <= 1)
    out[, , "c3ann_irrigated"] <- out[, , "irrig_c3ann"] * out[, , "c3ann"]

    out[, , "c3ann_rainfed"] <- (1 - out[, , "irrig_c3ann"]) * out[, , "c3ann"]
    out <- out[, , c("c3ann", "irrig_c3ann"), invert = TRUE]

    shareTotal <- dimSums(out, 3)
    if (max(shareTotal - 1) > 10^-10) {
      warning("Scaling land so that land share sum is equal to 1.")
      shareTotal[shareTotal < 1] <- 1
      out <- out / shareTotal
    }

    rest <- magclass::setNames(1 - dimSums(out, 3), "rest")
    rest[rest < 0] <- 0
    out <- mbind(out, rest)

    toolExpectLessDiff(dimSums(out, 3), 1, 10^-10, "land shares sum up to 1")

    # TODO convert from fraction of grid cell to Mha

    expectedSetNames <- c("region", "year", "data")
    expectedCategories <- c("primf", "secdf", "pltns", "primn", "secdn", "pastr",
                            "c3ann_irrigated", "c3ann_rainfed", "c4per", "rest")
    primf <- "primf"
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  # check data for consistency
  toolExpectTrue(identical(unname(getSets(out)), expectedSetNames),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), expectedCategories),
                 "Land input categories match expactation")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-4, "Total area is constant over time")
  toolExpectTrue(all(out[, -1, primf] <= setYears(out[, -nyears(out), primf], getYears(out[, -1, ]))),
                 "primary forest is never expanding", falseStatus = "warn")

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Land input data for data harmonization and downscaling pipeline"))
}
