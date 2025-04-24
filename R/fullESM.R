#' fullESM
#'
#' Run the pipeline to generate harmonized and downscaled data to report for the RESCUE, OptimESM
#' and other projects where ESM compatible land use inputs are required.
#' Write .nc files, print full report on consistency checks and write it to report.log.
#'
#' @param rev revision number of the data. If not provided the current date will be used instead.
#' When called via madrat::retrieveData rev will be converted to numeric_version.
#' @param ... reserved for future use
#' @param scenario scenario name to be included in filenames
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset remove years from the returned data which are not in yearsSubset
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#' @param progress boolean defining whether progress should be printed
#'
#' @author Pascal Sauer, Jan Philipp Dietrich
fullESM <- function(rev = numeric_version("0"), ..., scenario = "", harmonizationPeriod = c(2015, 2050),
                    yearsSubset = 2015:2150, compression = 2, progress = TRUE) {
  # TODO for ScenarioMIP use harmonization period c(2020, 2050)
  # TODO allow skipping harmonization altogether
  # TODO allow setting target
  stopifnot(...length() == 0)

  revision <- if (identical(rev, numeric_version("0"))) format(Sys.time(), "%Y-%m-%d") else rev

  fileSuffix <- paste0("_input4MIPs_landState_RESCUE_PIK-MAgPIE-4-7-",
                       scenario, if (scenario != "") "-",
                       revision, "_gn_", min(yearsSubset), "-", max(yearsSubset), ".nc")

  writeArgs <- list(compression = compression, missval = 1e20, progress = progress,
                    gridDefinition = c(-179.875, 179.875, -89.875, 89.875, 0.25))

  metadataArgs <- list(revision = revision, missingValue = 1e20, resolution = 0.25,
                       compression = compression, harmonizationPeriod = harmonizationPeriod)

  file <- paste0("multiple-states", fileSuffix)
  calcOutput("ESMStates", harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset,
             aggregate = FALSE, file = file, writeArgs = writeArgs)
  toolAddMetadataESM(file, metadataArgs)

  file <- paste0("multiple-management", fileSuffix)
  calcOutput("ESMManagement", harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset,
             aggregate = FALSE, file = file, writeArgs = writeArgs)
  toolAddMetadataESM(file, metadataArgs)

  file <- paste0("multiple-transitions", fileSuffix)
  calcOutput("ESMTransitions", harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset,
             aggregate = FALSE, file = file, writeArgs = writeArgs)
  toolAddMetadataESM(file, metadataArgs)

  toolWriteMadratLog(logPath = "consistencyCheck.log")
}
