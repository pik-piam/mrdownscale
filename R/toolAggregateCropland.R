#' toolAggregateCropland
#'
#' Aggregate variables like c3ann_irrigated and c3ann_rainfed to just c3ann.
#'
#' @param land magpie object with variables starting with cropTypes in dim 3
#' @param cropTypes character vector, the variables to aggregate to
#' @param ... reserved for future expansion
#' @param keepOthers logical, if FALSE cropTypes will be the only variables in output
#' @return land with crop data aggregated to cropTypes
#'
#' @author Pascal Sauer
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
