readWITCH <- function() {
  if (!requireNamespace("nanoparquet", quietly = TRUE)) {
    stop("Please install nanoparquet, e.g. with install.packages('nanoparquet')")
  }
  x <- nanoparquet::read_parquet("db_ssp2_M_luh3.parquet")
  unique(x$units)
  return(list(x = x,
              class = "data.frame",
              unit = paste(unique(x$units), collapse = ", "),
              description = "WITCH data"))
}
