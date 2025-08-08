convertCOFFEE <- function(x) {
  columns <- c("Model", "Scenario", "Region", "Variable", "Unit")
  stopifnot(colnames(x) %in% columns | startsWith(colnames(x), "X"))
  x$Year <- NA
  x <- Reduce(rbind, lapply(grep("^X", colnames(x), value = TRUE), function(year) {
    a <- x[, c(columns, "Year", year)]
    a$Year <- as.integer(sub("^X", "", year))
    colnames(a)[ncol(a)] <- "Value"
    return(a)
  }))
  return(list(x = x,
              class = "data.frame",
              unit = paste(unique(x$Unit), collapse = ", "),
              description = "COFFEE data"))
}
