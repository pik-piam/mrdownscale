toolCheckFertilizer <- function(fertilizer, landMha = NULL, threshold = 1200) {
  # assume fertilizer is in kg ha-1 yr-1 if landMha is NULL, in Tg yr-1 otherwise
  if (!is.null(landMha)) {
    fertilizer <- toolFertilizerKgPerHa(fertilizer, landMha)
  }
  toolExpectTrue(max(fertilizer) <= threshold,
                 paste0("Fertilizer application is <= ", threshold, " kg ha-1 yr-1 (max: ",
                        signif(max(fertilizer), 3), ")"),
                 level = 1)
}

toolFertilizerKgPerHa <- function(fertilizerTg, landMha) {
  cropMha <- toolAggregateCropland(landMha, getItems(fertilizerTg, 3.2), keepOthers = FALSE)
  # convert from Tg yr-1 to kg ha-1 yr-1
  fertilizerKgPerHa <- fertilizerTg / cropMha * (10^9 / 10^6)
  fertilizerKgPerHa[is.nan(fertilizerKgPerHa)] <- 0
  return(fertilizerKgPerHa)
}

toolFertilizerTg <- function(fertilizerKgPerHa, landMha) {
  cropMha <- toolAggregateCropland(landMha, getItems(fertilizerKgPerHa, 3.2), keepOthers = FALSE)
  # convert from Tg yr-1 to kg ha-1 yr-1
  fertilizerTg <- fertilizerKgPerHa * cropMha * (10^6 / 10^9)
  return(fertilizerTg)
}
