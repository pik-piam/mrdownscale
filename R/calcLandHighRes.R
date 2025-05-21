#' calcLandHighRes
#'
#' This function performs the downscaling: It calculates a high resolution dataset
#' from the low resolution input dataset and the high resolution target dataset
#' using the given downscaling method.
#'
#' @param input name of an input dataset
#' @param target name of a target dataset
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset vector of years to keep in the output dataset
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @param harmonization name of harmonization method, see \link{\code{toolGetHarmonizer}}
#' @return downscaled land use data
#' @author Jan Philipp Dietrich, Pascal Sauer
calcLandHighRes <- function(input, target, harmonizationPeriod, yearsSubset,
                            downscaling = "magpieClassic", harmonization = "fade") {
  x <- calcOutput("LandHarmonized", input = input, target = target,
                  harmonizationPeriod = harmonizationPeriod, method = harmonization,
                  aggregate = FALSE)
  x <- x[, getYears(x, as.integer = TRUE) %in% yearsSubset, ]

  hp1 <- harmonizationPeriod[1]

  xTarget <- calcOutput("LandTarget", target = target, aggregate = FALSE)
  xTarget <- as.magpie(xTarget[[terra::time(xTarget) %in% yearsSubset]])
  stopifnot(hp1 %in% getYears(xTarget, as.integer = TRUE))

  x <- x[, , getItems(xTarget, 3)]

  landTargetLowRes <- calcOutput("LandTargetLowRes", input = input, target = target, aggregate = FALSE)
  landTargetLowRes <- landTargetLowRes[, hp1, getItems(x, 3)]
  stopifnot(setequal(getItems(landTargetLowRes, 1), getItems(x, 1)))

  mapping <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)

  if (harmonization == "none") {
    # no harmonization means area is not equal, but that is required by downscaling functions
    # want to preserve input as much as possible, so scale target

    # if some clusters have 0 land area in target (so cannot scale) set them to input
    totalArea <- dimSums(landTargetLowRes, 3)
    zeroClusters <- Filter(x = getItems(totalArea, 1), f = function(cluster) totalArea[cluster, , ] == 0)
    stopifnot(x[zeroClusters, hp1, ] < 10^-3)
    landTargetLowRes[zeroClusters, hp1, ] <- x[zeroClusters, hp1, ]
    landTargetLowRes <- toolEqualizeArea(landTargetLowRes, x[, hp1, ])
  }

  if (downscaling == "magpieClassic") {
    out <- toolDownscaleMagpieClassic(x[, getYears(x, as.integer = TRUE) >= hp1, ],
                                      xTarget[, hp1, ],
                                      xTargetLowRes = landTargetLowRes,
                                      mapping = mapping)
  } else {
    stop("Unsupported downscaling method \"", downscaling, "\"")
  }
  histYears <- getYears(x, as.integer = TRUE)
  histYears <- histYears[histYears < hp1]
  if (length(histYears) > 0) {
    out <- mbind(xTarget[, histYears, ], out)
  }

  out <- toolReplaceExpansion(out, "primf", "secdf", warnThreshold = 100)
  out <- toolReplaceExpansion(out, "primn", "secdn", warnThreshold = 100)

  toolExpectTrue(identical(unname(getSets(out)), c("x", "y", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(x, dim = 3)),
                 "Land categories remain unchanged")
  toolExpectLessDiff(out[, hp1, ], xTarget[, hp1, ], 10^-5,
                     paste("In", hp1, "output equals target"))
  toolExpectTrue(all(out >= 0), "All values are >= 0")

  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5,
                     "Total land area per cell in output stays constant over time")

  globalSumIn <- dimSums(x[, getYears(out), ], dim = 1)
  globalSumOut <- dimSums(out, dim = 1)
  toolExpectLessDiff(dimSums(globalSumIn, 3), dimSums(globalSumOut, 3), 10^-5,
                     "Total global land area remains unchanged")
  toolExpectLessDiff(globalSumIn, globalSumOut, 10^-5,
                     "Global area of each land type remains unchanged")
  toolPrimExpansionCheck(out)

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled land use data"))
}
