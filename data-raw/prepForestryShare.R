withr::with_package("mrlandcore", {
  x <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = "lpjcell",
                  selectyears = seq(1995, 2015, 5), aggregate = FALSE)
  x <- x[, , c("forestry", "secdforest")]
  x <- collapseDim(x, "iso")

  # map from 67k magpie/lpjml cells to LUH 0.25deg cells
  withr::with_package("mrdownscale", {
    resolutionMapping <- calcOutput("ResolutionMapping", input = "magpie", target = "luh2mod", aggregate = FALSE)
  })
  x <- x[getItems(x, 1) %in% resolutionMapping$cellOriginal, , ]
  x <- toolAggregate(x, resolutionMapping, from = "cellOriginal", to = "cell")

  forestrySecdforest <- dimSums(x)
  x <- x[, , "forestry"] / forestrySecdforest # calc the share forestry / (forestry + secdforest)
  x[forestrySecdforest == 0] <- 0 # replace expected NAs with 0

  stopifnot(!is.na(x))
  write.magpie(x, "inst/extdata/forestryShare.mz")
})
