toolAggregateCropland <- function(land) {
  map <- data.frame(from = getItems(land, 3), to = getItems(land, 3))
  for (crop in c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")) {
    map$to <- sub(paste0("^", crop, "_.*$"), crop, map$to)
  }
  return(toolAggregate(land, map, from = "from", to = "to", dim = 3))
}
