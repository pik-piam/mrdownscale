toolHarmonizeNone <- function(xInput, xTarget, harmonizationPeriod) {
  endOfHistory <- harmonizationPeriod[1]
  xTarget <- xTarget[, getYears(xTarget, as.integer = TRUE) <= endOfHistory, ]
  xInput <- xInput[, getYears(xInput, as.integer = TRUE) > endOfHistory, ]
  return(mbind(xTarget, xInput))
}
# TODO integrate
