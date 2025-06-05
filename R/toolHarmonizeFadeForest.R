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

  # lowest total forest stock per cluster/region
  low <- magpply(forest, min, DIM = 2)

  primf <- mpmin(mpmin(forest[, max(years), ] * collapseDim(shares)[, , "primf"],
                       xTarget[, hp1, "primf"]),
                 low)

  # interpolation is not linear :(
  primf <- time_interpolate(mbind(xTarget[, hp1, "primf"],
                                  primf),
                            years[years > hp1])
  # convergence(xTarget[, hp1, "primf"],
  #             primf,
  #             hp1,
  #             max(years),
  #             type = "linear")

  secdf <- setNames(forest - primf, "secdf")
  negativeSecdf <- secdf
  negativeSecdf[negativeSecdf > 0] <- 0
  primf <- primf + collapseDim(negativeSecdf)
  secdf[secdf < 0] <- 0

  primsecdf <- mbind(primf, secdf)
  stopifnot(abs(forest - dimSums(primsecdf, 3)) < 10^-10)
  x[, years > hp1, psf] <- primsecdf
  browser()
  # x <- toolReplaceExpansion(x, "primf", "secdf", warnThreshold = 100, level = 3)
  return(x)
}
