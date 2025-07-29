readWITCH <- function(subtype = "data") {
  if (subtype == "data") {
    x <- utils::read.csv("db_ssp2_M_luh3.csv")
    return(list(x = x,
                class = "data.frame",
                unit = paste(unique(x$units), collapse = ", "),
                description = "WITCH data"))
  } else if (subtype == "resolutionMapping") {
    x <- utils::read.csv("resolution_mapping_witch.csv")
    return(list(x = x,
                class = "data.frame",
                description = "WITCH resolution mapping"))
  } else {
    stop("Unexpected subtype, only data and resolutionMapping are accepted")
  }
}
