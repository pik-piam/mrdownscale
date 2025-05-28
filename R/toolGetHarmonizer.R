#' toolGetHarmonizer
#'
#' Get a harmonizer function by name.
#'
#' @param harmonizerName name of a harmonizer function, currently offset, fade
#' @return harmonizer function
#' @author Pascal Sauer
toolGetHarmonizer <- function(harmonizerName) {
  # function(...) toolHarmonizeOffset(...) instead of passing
  # toolHarmonizeOffset directly so madrat recognizes it as dependency
  harmonizers <- list(offset = function(...) toolHarmonizeOffset(...),
                      fade = function(...) toolHarmonizeFade(...))
  stopifnot(harmonizerName %in% names(harmonizers))
  return(harmonizers[[harmonizerName]])
}
