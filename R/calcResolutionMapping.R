#' calcResolutionMapping
#'
#' Calculate a complete mapping from low (input dataset, clusters/countries/regions)
#' to high resolution (target dataset, grid). As a basis the mapping from the low
#' resolution clusters/countries/regions to grid cells is used.
#' Cells which are present in that mapping, but not in the target
#' dataset are discarded. Cells which are present in the target dataset, but not in the mapping are
#' added using a nearest-neighbor approach: These cells are mapped to the same low resolution
#' cluster/country/region as the closest cell which is already present in the mapping.
#'
#' @param input character, the input dataset
#' @param target character, the target dataset
#' @return a list including a data.frame with columns x, y, lowRes, countrycode
#'
#' @author Pascal Sauer
calcResolutionMapping <- function(input, target) {
  if (input == "magpie") {
    clustermap <- readSource("MagpieFulldataGdx", subtype = "clustermap")
    coords <- strsplit(clustermap$cell, "\\.")
    xCoords <- vapply(coords, function(x) as.double(sub("p", ".", x[1])), double(1))
    yCoords <- vapply(coords, function(x) as.double(sub("p", ".", x[2])), double(1))
    clustermap$cellOriginal <- sub("\\.[A-Z]{3}$", "", clustermap$cell)
    clustermap <- cbind(x = xCoords, y = yCoords, clustermap[, -which(colnames(clustermap) == "cell")])
    colnames(clustermap)[colnames(clustermap) == "cluster"] <- "lowRes"
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  targetGrid <- calcOutput("LandTarget", target = target, aggregate = FALSE)
  mapping <- toolResolutionMapping(clustermap, targetGrid)

  toolExpectTrue(setequal(colnames(mapping), c("x", "y", "lowRes", "region", "country",
                                               "global", "cellOriginal", "cell")),
                 "resolution mapping has the expected columns")
  coords <- terra::crds(targetGrid, df = TRUE)
  allTargetCells <- paste0(sub("\\.", "p", coords$x),
                           ".",
                           sub("\\.", "p", coords$y))
  toolExpectTrue(setequal(mapping$cell, allTargetCells),
                 "all target cells are mapped")
  toolExpectTrue(all(mapping$cellOriginal %in% clustermap$cellOriginal),
                 "a subset of input cells is mapped")

  return(list(x = mapping,
              class = "data.frame",
              unit = NA,
              description = "mapping of high to low resolution and countrycode"))
}

#' toolResolutionMapping
#'
#' See description of \code{\link{calcResolutionMapping}}. Here we are
#' assuming target resolution is finer than what mapping already provides.
#'
#' @param mapping a data.frame with columns x, y, lowRes
#' @param targetGrid a terra SpatRaster with the target resolution
#' @return a data.frame with columns x, y, lowRes, countrycode
#'
#' @author Pascal Sauer
#' @export
toolResolutionMapping <- function(mapping, targetGrid) {
  stopifnot(c("x", "y", "lowRes") %in% colnames(mapping))

  targetGridRes <- terra::res(targetGrid)
  mappingRes <- guessResolution(mapping[, c("x", "y")])

  stopifnot(targetGridRes[1] == targetGridRes[2],
            targetGridRes[1] <= mappingRes,
            mappingRes %% targetGridRes[1] == 0)

  mappingColumns <- colnames(mapping)
  mapping$cellId <- seq_len(nrow(mapping))
  pointsMapping <- terra::vect(mapping, geom = c("x", "y"), crs = terra::crs(targetGrid))

  if (targetGridRes[1] == mappingRes) {
    targetGridAggregated <- targetGrid
  } else {
    targetGridAggregated <- terra::aggregate(targetGrid, fact = mappingRes / targetGridRes)
  }
  rasterMapping <- terra::rasterize(pointsMapping, targetGridAggregated, field = "cellId")
  names(rasterMapping) <- "cellId"
  polygonsMapping <- terra::as.polygons(rasterMapping)
  rasterMapping <- terra::rasterize(polygonsMapping, targetGrid, field = "cellId")

  xyMapping <- terra::as.data.frame(rasterMapping, xy = TRUE)

  xyTarget <- terra::crds(targetGrid, df = TRUE, na.all = TRUE)
  xyTarget$source <- "target"

  mapAllInput <- merge(xyMapping, xyTarget, by = c("x", "y"), all.x = TRUE)
  missingInTarget <- mapAllInput[is.na(mapAllInput$source), ]
  if (nrow(missingInTarget) > 0) {
    missingShare <- nrow(missingInTarget) / nrow(xyMapping)
    toolStatusMessage(if (missingShare < 0.05) "note" else "warn",
                      paste0(round(missingShare * 100, 2),
                             "% of input cells missing in target, these are discarded"))
  } else {
    toolStatusMessage("ok", "target includes all input cells")
  }

  mapAllTarget <- merge(xyMapping, xyTarget, by = c("x", "y"), all.y = TRUE)
  missingInMapping <- mapAllTarget[is.na(mapAllTarget$cellId), c("x", "y")]
  if (nrow(missingInMapping) > 0) {
    missingShare <- nrow(missingInMapping) / nrow(xyTarget)
    toolStatusMessage(if (missingShare < 0.01) "note" else "warn",
                      paste0(round(missingShare * 100, 2),
                             "% of target cells missing in mapping, ",
                             "adding those to mapping (nearest neighbor)"))

    # method = "cosine" is about 12 times as fast compared to method = "geo" (which is more precise)
    near <- terra::nearest(terra::vect(missingInMapping, geom = c("x", "y"), crs = terra::crs(targetGrid)),
                           pointsMapping, method = "cosine")
    toolStatusMessage("note", paste0("nearest neighbor distances: ",
                                     "max = ", round(max(near$distance) / 1000, 1), "km",
                                     ", 90% quantile = ",
                                     round(stats::quantile(near$distance, probs = 0.90) / 1000, 1), "km",
                                     ", mean = ", round(mean(near$distance) / 1000, 1), "km"))

    near <- as.data.frame(near)
    colnames(near)[colnames(near) == "to_id"] <- "cellId"

    mappingAddition <- merge(near, mapping, by = "cellId")
    mappingAddition$x <- mappingAddition$from_x
    mappingAddition$y <- mappingAddition$from_y
    mappingAddition <- mappingAddition[, mappingColumns]
    stopifnot(nrow(mappingAddition) == nrow(missingInMapping))
  } else {
    toolStatusMessage("ok", "input includes all target cells")
    mappingAddition <- mapping[integer(0), ]
  }

  mapping$cellOriginal <- paste0(sub("\\.", "p", mapping$x), ".", sub("\\.", "p", mapping$y))
  mapping <- mapping[, setdiff(colnames(mapping), c("x", "y"))]

  # removing cells which are not in the target dataset here
  mappingBase <- merge(mapAllTarget[!is.na(mapAllTarget$cellId), ], mapping, by = "cellId")

  result <- rbind(mappingBase[, mappingColumns], mappingAddition[, mappingColumns])
  result$cell <- paste0(sub("\\.", "p", result$x), ".", sub("\\.", "p", result$y))

  stopifnot(setequal(paste(result$x, result$y), paste(xyTarget$x, xyTarget$y)))
  return(result)
}
