#' toolHarmonizeFadeForest
#'
#' Harmonize using \code{\link{toolHarmonizeFade}}, then sum up primf + secdf,
#' then disaggregate again in a way that minimizes primf to secdf conversion.
#'
#' @inheritParams toolHarmonizeFade
#' @inherit toolHarmonizeFade return
#' @author Pascal Sauer
toolHarmonizeFadeForest <- function(xInput, xTarget, harmonizationPeriod) {
  hp1 <- harmonizationPeriod[1]

  x <- toolHarmonizeFade(xInput, xTarget, harmonizationPeriod, level = 4)
  years <- getYears(x, as.integer = TRUE)
  psf <- c("primf", "secdf")
  if (!all(psf %in% getItems(x, 3))) {
    return(x)
  }
  forest <- dimSums(x[, years >= hp1, psf], 3)


  a <- xTarget[, getYears(xTarget, TRUE) <= hp1, "primf"]
  di <- -a[, -1, ] + setYears(a[, -nyears(a), ],
                              getYears(a[, -1, ]))
  meanPrimfDecline <- magpply(di, mean, DIM = 2)
  b <- xTarget[, hp1, "primf"]
  for (year in years[years > hp1]) {
    b <- mbind(b, setYears(b[, nyears(b), ] - meanPrimfDecline, year))
  }
  b[b < 0] <- 0

  primf <- xTarget[, hp1, "primf"]
  for (year in years[years > hp1]) {
    p <- mpmin(b[, year, ], forest[, year, ])
    p <- mpmin(p, primf[, nyears(primf), ])
    p <- setYears(p, year)
    primf <- mbind(primf, p)
  }
  primsecdf <- mbind(primf,
                     magclass::setNames(forest - primf, "secdf"))

  stopifnot(primsecdf >= 0,
            abs(forest - dimSums(primsecdf, 3)) < 10^-10)
  x[, years >= hp1, psf] <- primsecdf
  return(x)
}
