#' toolGetHarmonizer
#'
#' Get a harmonizer function by name.
#'
#' @param harmonizerName name of a harmonizer function, currently offset, fade, none
#' @return harmonizer function
#' @author Pascal Sauer
toolGetHarmonizer <- function(harmonizerName) {
  harmonizers <- list(offset = toolHarmonizeOffset,
                      fade = toolHarmonizeFade,
                      none = toolHarmonizeNone)
  stopifnot(harmonizerName %in% names(harmonizers))
  return(harmonizers[[harmonizerName]])
}
