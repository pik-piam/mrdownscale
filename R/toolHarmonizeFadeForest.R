#' toolHarmonizeFadeForest
#'
#' Harmonize using \code{\link{toolHarmonizeFade}}, then sum up primf + secdf
#' and disaggregate according to xTarget weights, in order to avoid unintended
#' conversion from primf to secdf.
#'
#' @inheritParams toolHarmonizeFade
#' @inherit toolHarmonizeFade return
#' @author Pascal Sauer
toolHarmonizeFadeForest <- function(xInput, xTarget, harmonizationPeriod) {
  hp1 <- harmonizationPeriod[1]
  x <- toolHarmonizeFade(xInput, xTarget, harmonizationPeriod, level = 4)
  years <- getYears(x, as.integer = TRUE)
  psf <- c("primf", "secdf")
  forest <- dimSums(x[, years > hp1, psf], 3)
  shares <- xTarget[, hp1, psf] / dimSums(xTarget[, hp1, psf], 3)
  shares[is.nan(shares)] <- 1 / length(psf)
  primsecdf <- forest * collapseDim(shares)
  stopifnot(abs(forest - dimSums(primsecdf, 3)) < 10^-10)
  x[, years > hp1, psf] <- primsecdf
  x <- toolReplaceExpansion(x, "primf", "secdf", warnThreshold = 100, level = 3)
  return(x)
}
