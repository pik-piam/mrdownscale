#' toolExtrapolate
#'
#' Extrapolate a dataset into the future. A linear model is fitted for
#' each combination of spatial entity and category, and then used to predict
#' the value of the last requested extrapolation year. If the linear model
#' is not significant (p > 0.05) the historical mean is used instead.
#' A spline-based interpolation is used to create a smooth transition from
#' historical values to the predicted value.
#'
#' @param x A magpie object with "year" as the temporal dimension and
#' without any NAs
#' @param years A vector of years to extrapolate to
#' @return A magpie object like x but with the extrapolated years only
#'
#' @author Pascal Sauer
toolExtrapolate <- function(x, years) {
  stopifnot(names(dimnames(x))[2] == "year",
            !is.na(x),
            !years %in% getYears(x, as.integer = TRUE))

  spatial <- strsplit(names(dimnames(x))[1], "\\.")[[1]]

  return(do.call(mbind, lapply(getItems(x, 1), function(location) {
    return(do.call(mbind, lapply(getItems(x, 3), function(category) {
      d <- as.data.frame(x[location, , category], rev = 3)
      fallbackValue <- mean(d$.value)
      prediction <- d[rep(1, length(years)), ]
      prediction$year <- years
      prediction$.value <- fallbackValue

      if (!all(d$.value == d$.value[1])) {
        model <- stats::lm(.value ~ year, data = d)

        # predict last year using linear model if significant, fallback otherwise
        lastYear <- data.frame(year = years[length(years)])
        lastYear <- rbind(lastYear, lastYear + 1) # add extra year to bend spline
        if (summary(model)$coefficients["year", "Pr(>|t|)"] <= 0.05) {
          y <- c(d$.value, stats::predict(model, newdata = lastYear))
        } else {
          y <- c(d$.value, fallbackValue, fallbackValue)
        }
        # spline interpolation from last historical year to predicted year
        prediction$.value <- stats::spline(x = c(d$year, lastYear$year),
                                           y = y,
                                           xout = prediction$year)$y
      }

      return(as.magpie(prediction, spatial = spatial, temporal = "year"))
    })))
  })))
}
