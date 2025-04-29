#' calcManagementNC
#'
#' Prepare data to be written as a LUH-style management.nc file.  Call this via calcOutput
#' in a full function, and set calcOutput's file argument to a .nc file path.
#'
#' @param outputFormat options: ESM, ScenarioMIP
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset remove years from the returned data which are not in yearsSubset
#' @return data prepared to be written as a LUH-style management.nc file
#' @author Pascal Sauer, Jan Philipp Dietrich
calcManagementNC <- function(outputFormat, harmonizationPeriod, yearsSubset) {
# TODO rename file
  land <- calcOutput("LandReport", outputFormat = outputFormat,
                     harmonizationPeriod = harmonizationPeriod,
                     yearsSubset = yearsSubset, aggregate = FALSE)

  if (outputFormat == "ESM") {
    landManagementVariables <- c("irrig_c3ann", "crpbf_c3ann", "irrig_c3nfx", "crpbf_c3nfx",
                                 "irrig_c3per", "crpbf_c3per", "crpbf2_c3per", "irrig_c4ann",
                                 "crpbf_c4ann", "irrig_c4per", "crpbf_c4per", "crpbf2_c4per", "manaf")
  } else if (outputFormat == "ScenarioMIP") {
    landManagementVariables <- c("irrig_c3ann", "cpbf1_c3ann", "irrig_c3nfx", "cpbf1_c3nfx",
                                 "irrig_c3per", "cpbf1_c3per", "cpbf2_c3per", "irrig_c4ann",
                                 "cpbf1_c4ann", "irrig_c4per", "cpbf1_c4per", "cpbf2_c4per")
    # TODO still missing: addtc, pltns_wdprd, pltns_bfuel,
    # prtct_primf, prtct_primn, prtct_secdf, prtct_secdn, prtct_pltns
  }
  land <- land[, , landManagementVariables]

  nonland <- calcOutput("NonlandReport", outputFormat = outputFormat,
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
              description = "data prepared to be written as a LUH-style management.nc file"))
}
