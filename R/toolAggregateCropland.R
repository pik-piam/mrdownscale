toolAggregateCropland <- function(land,
                                  cropTypes = c("c3ann", "c4ann", "c3per", "c4per", "c3nfx"),
                                  ...,
                                  keepOthers = TRUE) {
  stopifnot(...length() == 0)
  map <- data.frame(from = getItems(land, 3), to = getItems(land, 3))
  for (cropType in cropTypes) {
    map$to <- sub(paste0("^", cropType, "_.*$"), cropType, map$to)
  }
  if (!keepOthers) {
    map <- map[map$to %in% cropTypes, ]
  }
  return(toolAggregate(land[, , map$from], map, from = "from", to = "to", dim = 3))
}
