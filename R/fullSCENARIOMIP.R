fullSCENARIOMIP <- function(rev = numeric_version("0"), ..., scenario = "",
                            harmonizationPeriod = c(2020, 2050),
                            yearsSubset = 2020:2150,
                            compression = 2, progress = TRUE) {
  stopifnot(...length() == 0)

  revision <- if (identical(rev, numeric_version("0"))) format(Sys.time(), "%Y-%m-%d") else rev

  fileSuffix <- paste0("_input4MIPs_landState_ScenarioMIP_PIK-MAgPIE-",
                       scenario, if (scenario != "") "-",
                       revision, "_gn_", min(yearsSubset), "-", max(yearsSubset), ".nc")

  writeArgs <- list(compression = compression, missval = 1e20, progress = progress,
                    gridDefinition = c(-179.875, 179.875, -89.875, 89.875, 0.25))

  calcOutput("StatesNC", outputFormat = "ScenarioMIP", harmonizationPeriod = harmonizationPeriod,
             yearsSubset = yearsSubset,
             statesVariables = c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                                 "primf", "primn", "range", "secdf", "secdn", "urban", "pltns"),
             aggregate = FALSE, file = paste0("multiple-states", fileSuffix), writeArgs = writeArgs)

  toolWriteMadratLog(logPath = "consistencyCheck.log")
}
