readWITCH <- function() {
  if (!requireNamespace("nanoparquet", quietly = TRUE)) {
    stop("To read WITCH data please install nanoparquet, e.g. with install.packages('nanoparquet')")
  }
  x <- nanoparquet::read_parquet("db_ssp2_M_luh3.parquet")
  x$region <- levels(x$region)[x$region]
  return(list(x = x,
              class = "data.frame",
              unit = paste(unique(x$units), collapse = ", "),
              description = "WITCH data"))
}
