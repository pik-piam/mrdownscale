correctCOFFEE <- function(x) {
  if (all(is.na(x[1, ]) | x[1, ] == "")) {
    x <- x[-1, ]
    rownames(x) <- seq_len(nrow(x))
  }
  return(list(x = x,
              class = "data.frame",
              unit = paste(unique(x$Unit), collapse = ", "),
              description = "COFFEE data"))
}
