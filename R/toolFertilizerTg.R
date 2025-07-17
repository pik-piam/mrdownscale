toolFertilizerTg <- function(fertilizerKgPerHa, landMha) {
  landMha <- landMha[getItems(fertilizerKgPerHa, 1), getItems(fertilizerKgPerHa, 2), ]
  cropMha <- toolAggregateCropland(landMha, getItems(fertilizerKgPerHa, 3.2), keepOthers = FALSE)
  # convert from kg ha-1 yr-1 to Tg yr-1
  fertilizerTg <- fertilizerKgPerHa * cropMha * (10^6 / 10^9)
  return(fertilizerTg)
}
