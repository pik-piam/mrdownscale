toolCheckFertilizer <- function(fertilizerTg, landMha, threshold = 1200) {
  fertilizerKgPerHa <- toolFertilizerKgPerHa(fertilizerTg, landMha)
  toolExpectTrue(max(fertilizerKgPerHa) <= threshold,
                 paste0("Fertilizer application is <= ", threshold, " kg ha-1 yr-1 (max: ",
                        signif(max(fertilizerKgPerHa), 3), ")"),
                 level = 1)
}

toolFertilizerKgPerHa <- function(fertilizerTg, landMha, threshold = NULL, replacement = 0) {
  cropMha <- toolAggregateCropland(landMha, getItems(fertilizerTg, 3.2), keepOthers = FALSE)
  # convert from Tg yr-1 to kg ha-1 yr-1
  fertilizerKgPerHa <- fertilizerTg / cropMha * (10^9 / 10^6)
  fertilizerKgPerHa[is.nan(fertilizerKgPerHa)] <- 0

  if (!is.null(threshold) && any(fertilizerKgPerHa >= threshold)) {
    message("replaced fertilizer larger than threshold (", threshold, ") replaced with ", replacement)
    fertilizerKgPerHa[fertilizerKgPerHa >= threshold] <- replacement
  }

  return(fertilizerKgPerHa)
}

toolFertilizerTg <- function(fertilizerKgPerHa, landMha) {
  cropMha <- toolAggregateCropland(landMha, getItems(fertilizerKgPerHa, 3.2), keepOthers = FALSE)
  # convert from Tg yr-1 to kg ha-1 yr-1
  fertilizerTg <- fertilizerKgPerHa * cropMha * (10^6 / 10^9)
  return(fertilizerTg)
}
