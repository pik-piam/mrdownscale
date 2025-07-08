toolCheckFertilizer <- function(fertilizerTg, landMha, threshold = 1200) {
  cropMha <- toolAggregateCropland(landMha, getItems(fertilizerTg, 3.2), keepOthers = FALSE)
  # convert from Tg yr-1 to kg ha-1 yr-1
  fertilizerKgPerHa <- fertilizerTg / cropMha * (10^9 / 10^6)
  fertilizerKgPerHa[is.nan(fertilizerKgPerHa)] <- 0
  toolExpectTrue(max(fertilizerKgPerHa) <= threshold,
                 paste0("Fertilizer application is <= ", threshold, " kg ha-1 yr-1 (max: ",
                        signif(max(fertilizerKgPerHa), 3), ")"),
                 level = 1)
}
