readWITCH <- function() {
  x <- utils::read.csv("db_ssp2_M_luh3.csv")
  return(list(x = x,
              class = "data.frame",
              unit = paste(unique(x$units), collapse = ", "),
              description = "WITCH data"))
}
