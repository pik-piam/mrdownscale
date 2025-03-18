#' calcESMStates
#'
#' Create ESM compatible state information
#'
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset remove years from the returned data which are not in yearsSubset
#' @return ESM compatible states information
#' @author Pascal Sauer, Jan Philipp Dietrich
calcESMStates <- function(harmonizationPeriod, yearsSubset) {
  x <- calcOutput("LandReport", outputFormat = "ESM",
                  harmonizationPeriod = harmonizationPeriod,
                  yearsSubset = yearsSubset, aggregate = FALSE)

  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")
  x <- x[, , statesVariables]

  # account for unit "years since 1970-01-01 0:0:0"
  x <- setYears(x, getYears(x, as.integer = TRUE) - 1970)

  return(list(x = x,
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1,
              cache = FALSE,
              description = "ESM compatible states information"))
}
