#' calcLandTarget
#'
#' Prepare the high resolution target land use dataset for
#' harmonization and downscaling, checking data for consistency before returning.
#'
#' @param target name of the target dataset, options are: luh2, luh2mod, landuseinit
#' luh2mod will split secdf into forestry and secdf
#' @return land target data
#' @author Pascal Sauer
calcLandTarget <- function(target) {
  if (target %in% c("luh2", "luh2mod")) {
    cropTypes <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per")

    states <- readSource("LUH2v2h", subtype = "states")
    nonCropStates <- states[[grep(paste(cropTypes, collapse = "|"), names(states),
                                  value = TRUE, invert = TRUE)]]
    states <- toolSpatRasterToDataset(states)
    man <- readSource("LUH2v2h", subtype = "management", convert = FALSE)
    man <- toolSpatRasterToDataset(man)
    man <- man[c(paste0("crpbf_", cropTypes), paste0("irrig_", cropTypes))]
    stopifnot(all.equal(terra::time(states[1]), terra::time(man[1])))

    out <- list(nonCropStates)
    for (cropType in cropTypes) {
      crpbf <- man[paste0("crpbf_", cropType)]
      irrig <- man[paste0("irrig_", cropType)]
      stopifnot(all.equal(terra::time(crpbf), terra::time(states[cropType])),
                all.equal(terra::time(irrig), terra::time(states[cropType])))

      # crpbf_<cropType> = 1st gen biofuel crop share of <cropType>
      # -> get <cropType>_biofuel_1st_gen area in Mha by multiplying with <cropType>
      biofuel1stGen <- crpbf * states[cropType]

      irrigatedBiofuel1stGen <- irrig * biofuel1stGen
      rainfedBiofuel1stGen <- biofuel1stGen - irrigatedBiofuel1stGen
      names(irrigatedBiofuel1stGen) <- sub("\\.\\..+$", paste0("..", cropType, "_irrigated_biofuel_1st_gen"),
                                           names(irrigatedBiofuel1stGen))
      names(rainfedBiofuel1stGen) <- sub("\\.\\..+$", paste0("..", cropType, "_rainfed_biofuel_1st_gen"),
                                         names(rainfedBiofuel1stGen))

      nonBiofuel <- states[cropType] - biofuel1stGen

      irrigatedBiofuel2ndGen <- NULL
      rainfedBiofuel2ndGen <- NULL
      if (grepl("per", cropType)) {
        # 2nd gen biofuel is not part of LUH2v2h, but we need it for the harmonization, so fill with zeros
        irrigatedBiofuel2ndGen <- 0 * states[cropType]
        rainfedBiofuel2ndGen <- 0 * states[cropType]
        names(irrigatedBiofuel2ndGen) <- paste0(names(irrigatedBiofuel2ndGen), "_irrigated_biofuel_2nd_gen")
        names(rainfedBiofuel2ndGen) <- paste0(names(rainfedBiofuel2ndGen), "_rainfed_biofuel_2nd_gen")
        nonBiofuel <- nonBiofuel - irrigatedBiofuel2ndGen - rainfedBiofuel2ndGen
      }

      irrigatedNonBiofuel <- irrig * nonBiofuel
      rainfedNonBiofuel <- nonBiofuel - irrigatedNonBiofuel
      names(irrigatedNonBiofuel) <- sub("\\.\\..+$", paste0("..", cropType, "_irrigated"), names(irrigatedNonBiofuel))
      names(rainfedNonBiofuel) <- sub("\\.\\..+$", paste0("..", cropType, "_rainfed"), names(rainfedNonBiofuel))

      out <- c(out, irrigatedNonBiofuel, rainfedNonBiofuel, irrigatedBiofuel1stGen,
               rainfedBiofuel1stGen, irrigatedBiofuel2ndGen, rainfedBiofuel2ndGen)
    }
    out <- do.call(c, out)
    terra::time(out, tstep = "years") <- as.integer(substr(names(out), 2, 5))
    # need to write raster to disk to avoid memory issues
    # cannot use withr::local_tempfile because the SpatRaster is invalid as soon as the underlying file is deleted
    out <- terra::writeRaster(out, filename = tempfile(fileext = ".tif"))

    if (target == "luh2mod") {
      # split secdf into forestry and secdf
      forestryShare <- read.magpie(system.file("extdata/forestryShare.mz", package = "mrdownscale"))
      forestryShare <- as.SpatRaster(forestryShare)
      forestryShare <- terra::extend(forestryShare, out)
      forestry <- out["secdf"] * forestryShare
      names(forestry) <- sub("secdf", "forestry", names(forestry))
      secdf <- out["secdf"] - forestry
      out <- c(out[[!grepl("secdf", names(out))]], forestry, secdf)

      # cannot cache SpatRaster with both in-memory and on-disk/file sources,
      # so write `out` to a tif file to get SpatRaster with a single source (the tif file)
      out <- terra::writeRaster(out, filename = tempfile(fileext = ".tif"))
    }
    expectedCategories <- toolLandCategoriesMapping(input = "magpie", target = target)$dataOutput
  } else if (target == "landuseinit") {
    out <- readSource("LanduseInit")
    getItems(out, 3) <- sub("primforest", "primf", getItems(out, 3))
    getItems(out, 3) <- sub("secdforest", "secdf", getItems(out, 3))

    out <- toolScaleConstantArea(out)
    out <- toolReplaceExpansion(out, "primf", "secdf")
    out <- as.SpatRaster(out)

    expectedCategories <- c("crop", "past", "forestry", "primf", "secdf", "urban",  "other")
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

  # checks
  toolExpectTrue(terra::crs(out) != "", "Data contains CRS information")
  toolExpectTrue(setequal(sub("y[0-9]+\\.\\.", "", names(out)), expectedCategories),
                 "Land target categories match the corresponding mapping")
  toolExpectTrue(min(terra::minmax(out)) >= 0, "All values are >= 0")
  totalAreas <- vapply(unique(terra::time(out)), function(year) {
    sum(terra::values(out[[terra::time(out) == year]]), na.rm = TRUE)
  }, double(1))
  toolExpectLessDiff(max(totalAreas), min(totalAreas), 10^-4,
                     "Total area is constant over time")

  years <- sort(unique(terra::time(out)))
  primfn <- out["primf|primn"]
  primfnTime <- terra::time(primfn)
  primfnDiff <- primfn[[primfnTime %in% years[-1]]] - primfn[[primfnTime %in% years[-length(years)]]]
  toolExpectTrue(max(terra::minmax(primfnDiff)) <= 0,
                 "primary land is never expanding", falseStatus = "warn")

  return(list(x = out,
              class = "SpatRaster",
              unit = "Mha",
              description = "Land target data for data harmonization"))
}
