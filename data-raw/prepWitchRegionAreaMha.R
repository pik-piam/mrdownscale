pkgload::load_all()
carea <- readSource("LUH3", subtype = "cellArea", convert = FALSE)
stopifnot(terra::units(carea) == "km2")
# convert from km2 to Mha
mcarea <- as.magpie(carea) / 10000
resmap <- calcOutput("ResolutionMapping", input = "witch", target = "luh3", aggregate = FALSE)
mcarea <- mcarea[unique(resmap$cell), , ]
out <- toolAggregate(mcarea, resmap[, c("cell", "lowRes")])
out <- clean_magpie(out)
names(dimnames(out))[1] <- "region"
out <- collapseDim(out)
write.magpie(out, "witchRegionAreaMha.mz")
