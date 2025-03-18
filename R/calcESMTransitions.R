#' calcESMTransitions
#'
#' Create ESM compatible transitions information
#'
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset remove years from the returned data which are not in yearsSubset
#' @return ESM compatible transistions information
#' @author Pascal Sauer, Jan Philipp Dietrich
calcESMTransitions <- function(harmonizationPeriod, yearsSubset) {
  nonland <- calcOutput("NonlandReport", outputFormat = "ESM",
                        harmonizationPeriod = harmonizationPeriod,
                        yearsSubset = yearsSubset, aggregate = FALSE)

  woodSources <- c("primf", "secyf", "secmf", "primn", "secnf")
  woodHarvestVariables <- c(paste0(woodSources, "_bioh"), paste0(woodSources, "_harv"))
  nonland <- nonland[, , woodHarvestVariables]

  x <- calcOutput("LandTransitions", outputFormat = "ESM",
                  harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset,
                  gross = TRUE, aggregate = FALSE)
  getItems(x, raw = TRUE, dim = 3) <- sub("\\.", "_to_", getItems(x, dim = 3))
  getSets(x, fulldim = FALSE)[3] <- "transitions"

  # we use to-semantics for transitions (value for 1994 describes what happens from 1993 to 1994)
  # by subtracting 1 we get from-semantics (value for 1994 describes what happens from 1994 to 1995)
  # which is what LUH uses
  getYears(x) <- getYears(x, as.integer = TRUE) - 1
  x <- mbind(x[, getYears(x, as.integer = TRUE) %in% yearsSubset, ],
             nonland[, getYears(nonland, as.integer = TRUE) %in% yearsSubset, ])

  # account for unit "years since 1970-01-01 0:0:0"
  x <- setYears(x, getYears(x, as.integer = TRUE) - 1970)

  return(list(x = x,
              isocountries = FALSE,
              unit = "1 or kg C yr-1",
              min = 0,
              cache = FALSE,
              description = "ESM compatible transistion information"))
}
