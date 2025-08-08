readCOFFEE <- function(subtype = "data") {
  if (subtype == "data") {
    x <- utils::read.csv("Land Cover - COFFEE model.csv")
    return(list(x = x,
                class = "data.frame",
                unit = paste(unique(x$Unit), collapse = ", "),
                description = "COFFEE data"))
  } else {
    stop("Unexpected subtype, only data is accepted")
  }
}
