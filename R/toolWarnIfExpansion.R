#' toolWarnIfExpansion
#'
#' Warn if expansion is detected one of the given variables, giving the
#' maximum expansion in each respective variable.
#'
#' @param x A magclass object
#' @param variable A vector of variables to be checked
#'
#' @author Pascal Sauer
toolWarnIfExpansion <- function(x, variable) {
  maxExpansion <- vapply(intersect(variable, getItems(x, 3)),
                         function(v) toolMaxExpansion(x[, , v]),
                         numeric(1))
  maxExpansion <- maxExpansion[maxExpansion > 0]
  if (length(maxExpansion) >= 1) {
    warning(paste0("Expansion of ", names(maxExpansion),
                   " detected, max expansion: ", signif(maxExpansion, 3),
                   collapse = "\n  "))
  }
}
