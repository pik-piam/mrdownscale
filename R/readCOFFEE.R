readCOFFEE <- function(subtype = "data") {
  if (subtype == "data") {
    x <- utils::read.csv("Land Cover - COFFEE model.csv")
    return(list(x = x,
                class = "data.frame",
                unit = paste(unique(x$Unit), collapse = ", "),
                description = "COFFEE data"))
  } else if (subtype == "regionMapping") {
    mapping <- utils::read.csv("COFFEE Regional Definition.csv")
    mapping <- mapping[, c("Native.Region.Code", "ISO.Code")]
    colnames(mapping) <- c("region", "country")
    mapping <- rbind(mapping, c("CA", "CUW"))
    regions <- unique(mapping$region)
    addId <- data.frame(region = regions, lowRes = paste0(regions, ".", seq_along(regions)))
    mapping <- merge(mapping, addId, "region")

    return(list(x = mapping,
                class = "data.frame",
                description = "COFFEE resolution mapping"))
  } else {
    stop("Unexpected subtype, only data is accepted")
  }
}
