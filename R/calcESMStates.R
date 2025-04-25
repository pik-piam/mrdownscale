#' calcStatesNC
#'
#' Prepare LandReport data to be written as .nc file. Call this via calcOutput
#' in a full function, and set calcOutput's file argument toa .nc file path.
#'
#' @param outputFormat options: ESM, ScenarioMIP
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset remove years from the returned data which are not in yearsSubset
#' @param statesVariables which variables should be kept/written to nc file
#' @return LandReport data prepared to be written as .nc file
#' @author Pascal Sauer, Jan Philipp Dietrich
calcStatesNC <- function(outputFormat, harmonizationPeriod, yearsSubset, statesVariables) {
  x <- calcOutput("LandReport", outputFormat = outputFormat,
                  harmonizationPeriod = harmonizationPeriod,
                  yearsSubset = yearsSubset, aggregate = FALSE)

  x <- x[, , statesVariables]

  # account for unit "years since 1970-01-01 0:0:0"
  x <- setYears(x, getYears(x, as.integer = TRUE) - 1970)

  return(list(x = x,
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1,
              cache = FALSE,
              description = "LandReport data prepared to be written as .nc file"))
}
