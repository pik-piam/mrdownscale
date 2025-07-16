toolAggregateCropland <- function(land,
                                  cropTypes = c("c3ann", "c4ann", "c3per", "c4per", "c3nfx"),
                                  ...,
                                  keepOthers = TRUE) {
  stopifnot(...length() == 0)
  map <- toolCropMapping(land, cropTypes)
  if (!keepOthers) {
    map <- map[map$coarse %in% cropTypes, ]
  }
  return(toolAggregate(land[, , map$fine], map, from = "fine", to = "coarse", dim = 3))
}

toolCropMapping <- function(land, cropTypes) {
  map <- data.frame(fine = getItems(land, 3), coarse = getItems(land, 3))
  for (cropType in cropTypes) {
    map$coarse <- sub(paste0("^", cropType, "_.*$"), cropType, map$coarse)
  }
  return(map)
}
