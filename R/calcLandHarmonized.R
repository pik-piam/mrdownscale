#' calcLandHarmonized
#'
#' This function computes a version of the chosen land input data in the resolution
#' of the land input data but harmonized to the land use information of the
#' chosen land target data set (harmonized categories as well as harmonized
#' transition from historic target data to simulated input data).
#'
#' @param input name of the land input source to be used
#' @param target name of the land target source to be used
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param method transitioning method
#' @author Pascal Sauer, Jan Philipp Dietrich
calcLandHarmonized <- function(input, target, harmonizationPeriod,
                               method = "fade") {
  xInput    <- calcOutput("LandInputRecategorized", input = input,
                          target = target, aggregate = FALSE)
  geometry <- attr(xInput, "geometry")
  crs      <- attr(xInput, "crs")

  inputYears <- getYears(xInput, as.integer = TRUE)
  transitionYears <- inputYears[inputYears > harmonizationPeriod[1] & inputYears < harmonizationPeriod[2]]
  xTarget <- calcOutput("LandTargetExtrapolated", input = input, target = target,
                        transitionYears = transitionYears, aggregate = FALSE)

  droppedLowRes <- setdiff(getItems(xInput, 1), getItems(xTarget, 1))
  if (length(droppedLowRes) > 0) {
    toolStatusMessage("note", paste("dropped the following spatial objects from",
                                    "input data as it is missing in reference:",
                                    paste(droppedLowRes, collapse = ", ")))
  }
  xInput <- xInput[getItems(xTarget, 1), , ]

  # checks and corrections
  inSum <- dimSums(xInput, dim = 3)
  tSum <- dimSums(xTarget, dim = 3)
  toolExpectLessDiff(inSum, inSum[, 1, ], 10^-5, "Total areas in input stay constant over time")
  toolExpectLessDiff(tSum, tSum[, 1, ], 10^-5, "Total areas in target stay constant over time")
  toolExpectLessDiff(inSum[, 1, ], tSum[, 1, ], 10^-5,
                     "Total areas are the same in target and input data")
  if (max(abs(inSum[, 1, ] - tSum[, 1, ])) >= 10^-5) {
    corr <- setYears(dimSums(xTarget[, 1, ], dim = 3) / dimSums(xInput[, 1, ], dim = 3), NULL)
    stopifnot(is.finite(corr), corr >= 0)
    xInput <- xInput * corr
    toolStatusMessage("note", paste0("input data multiplied with correction factors to match target areas ",
                                     "(max ratio = ", round(max(corr), 2),
                                     ", min ratio = ", round(min(corr), 2),  ")"))
  }

  harmonizer <- toolGetHarmonizer(method)
  out <- harmonizer(xInput, xTarget, harmonizationPeriod = harmonizationPeriod)

  # during harmonization primf and primn expansion might be introduced due to
  # primf or primn differences between input and target dataset
  # replace primf and primn expansion with secdf and secdn
  out <- toolReplaceExpansion(out, "primf", "secdf", warnThreshold = 100)
  out <- toolReplaceExpansion(out, "primn", "secdn", warnThreshold = 100)

  attr(out, "geometry") <- geometry
  attr(out, "crs")      <- crs

  # checks
  toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
  toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(xInput, dim = 3)),
                 "Land categories remain unchanged")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5, "Total areas in output stay constant over time")
  toolExpectLessDiff(outSum, dimSums(xInput, dim = 3), 10^-5, "Total areas remain unchanged")
  toolPrimExpansionCheck(out)
  toolExpectLessDiff(out[, getYears(out, as.integer = TRUE) <= harmonizationPeriod[1], ],
                     xTarget[, getYears(xTarget, as.integer = TRUE) <= harmonizationPeriod[1], ],
                     10^-5, "Returning reference data before harmonization period")

  outAfterHarmonization <- out[, getYears(out, as.integer = TRUE) >= harmonizationPeriod[2], ]
  inputAfterHarmonization <- xInput[, getYears(xInput, as.integer = TRUE) >= harmonizationPeriod[2], ]
  nonprimfix <- setdiff(getItems(out, dim = 3), c("primf", "primn", "secdf", "secdn"))
  toolExpectLessDiff(outAfterHarmonization[, , nonprimfix],
                     inputAfterHarmonization[, , nonprimfix],
                     10^-5, "Returning input data after harmonization period (not checking primf/primn/secdf/secdn)")
  toolExpectLessDiff(dimSums(outAfterHarmonization[, , c("primf", "secdf")], 3),
                     dimSums(inputAfterHarmonization[, , c("primf", "secdf")], 3),
                     10^-5, "Returning input data after harmonization period (checking primf + secdf)")
  if ("primn" %in% getItems(out, 3)) {
    toolExpectLessDiff(dimSums(outAfterHarmonization[, , c("primn", "secdn")], 3),
                       dimSums(inputAfterHarmonization[, , c("primn", "secdn")], 3),
                       10^-5, "Returning input data after harmonization period (checking primn + secdn)")
  }

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Harmonized land data"))
}
