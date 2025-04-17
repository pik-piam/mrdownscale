#' calcESMManagement
#'
#' Create ESM compatible management information
#'
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset remove years from the returned data which are not in yearsSubset
#' @return ESM compatible management information
#' @author Pascal Sauer, Jan Philipp Dietrich
calcESMManagement <- function(harmonizationPeriod, yearsSubset) {
  land <- calcOutput("LandReport", outputFormat = "ESM",
                     harmonizationPeriod = harmonizationPeriod,
                     yearsSubset = yearsSubset, aggregate = FALSE)

  landManagementVariables <- c("irrig_c3ann", "crpbf_c3ann", "irrig_c3nfx", "crpbf_c3nfx",
                               "irrig_c3per", "crpbf_c3per", "crpbf2_c3per", "irrig_c4ann",
                               "crpbf_c4ann", "irrig_c4per", "crpbf_c4per", "crpbf2_c4per", "manaf")
  land <- land[, , landManagementVariables]

  nonland <- calcOutput("NonlandReport", outputFormat = "ESM",
                        harmonizationPeriod = harmonizationPeriod,
                        yearsSubset = yearsSubset, aggregate = FALSE)
  nonlandManagementVariables <- c("fertl_c3nfx", "fertl_c3per", "fertl_c3ann", "fertl_c4ann",
                                  "fertl_c4per", "rndwd", "fulwd")
  nonland <- nonland[, , nonlandManagementVariables]

  x <- mbind(land, nonland)

  # account for unit "years since 1970-01-01 0:0:0"
  x <- setYears(x, getYears(x, as.integer = TRUE) - 1970)

  return(list(x = x,
              isocountries = FALSE,
              unit = "1 or kg ha-1 yr-1",
              min = 0,
              cache = FALSE,
              description = "ESM compatible management information"))
}
