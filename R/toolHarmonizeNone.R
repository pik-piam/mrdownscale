#' toolHarmonizeNone
#'
#' Do not actually harmonize, but just combine xTarget and xInput,
#' using xTarget until start of harmonization period, then xInput.
#'
#' @param xInput input data as magpie object
#' @param xTarget target data as magpie object
#' @param harmonizationPeriod Two integer values, until the first given
#' year xTarget is used, after that use xInput; the second value is ignored
#' @return magpie object
#' @author Pascal Sauer
toolHarmonizeNone <- function(xInput, xTarget, harmonizationPeriod) {
  endOfHistory <- harmonizationPeriod[1]
  xTarget <- xTarget[, getYears(xTarget, as.integer = TRUE) <= endOfHistory, ]
  xInput <- xInput[, getYears(xInput, as.integer = TRUE) > endOfHistory, ]
  return(mbind(xTarget, xInput))
}
