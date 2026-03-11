#' toolMaxExpansion
#'
#' Return the maximum expansion from one timestep to the next.
#'
#' @param x A magclass object
#' @param removeNA A logical indicating whether NA values should be ignored
#' @return A numeric value indicating the maximum expansion
#'
#' @author Pascal Sauer
#' @export
toolMaxExpansion <- function(x, removeNA = FALSE) {
  stopifnot(nyears(x) >= 2)
  return(max(x[, -1, ] - setYears(x[, -nyears(x), ],
                                  getYears(x[, -1, ])),
             na.rm = removeNA))
}
