#' toolHarmonizeNone
#'
#' Do not actually harmonize, just return xInput.
#'
#' @param xInput input data as magpie object
#' @param xTarget ignored for this function
#' @param harmonizationPeriod ignored for this function
#' @return unchanged xInput
#' @author Pascal Sauer
toolHarmonizeNone <- function(xInput, xTarget, harmonizationPeriod) {
  return(xInput)
}
