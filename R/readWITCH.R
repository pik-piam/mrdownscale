readWITCH <- function(subtype = "data") {
  if (subtype == "data") {
    x <- utils::read.csv("db_ssp2_M_luh3.csv")
    return(list(x = x,
                class = "data.frame",
                unit = paste(unique(x$units), collapse = ", "),
                description = "WITCH data"))
  } else if (subtype == "resolutionMapping") {
    mapping <- utils::read.csv("resolution_mapping_witch.csv")
    regions <- unique(mapping$witch17)
    mapping <- mapping[, setdiff(colnames(mapping), "lowRes")]
    addId <- data.frame(witch17 = regions, lowRes = paste0(regions, ".", seq_along(regions)))
    mapping <- merge(mapping, addId, "witch17")
    return(list(x = mapping,
                class = "data.frame",
                description = "WITCH resolution mapping"))
  } else {
    stop("Unexpected subtype, only data and resolutionMapping are accepted")
  }
}
