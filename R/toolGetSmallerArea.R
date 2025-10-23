#' toolGetSmallerArea
#'
#' The function creates a transition data set based on a state dataset which
#' contains for every possible connection (land type A <-> land type B) the
#' area of the smaller of these land types.
#' This information is relevant if transitional shares should be calculated
#' based on the smaller area of the two (assuming that the smaller area
#' is determining how much transitions there will be).
#'
#' @param states magpie dataset containing states information
#' @return A land type x land type data set containing for each possible combination
#' the smaller area of the two land types
#' @author Jan Philipp Dietrich
#' @export

toolGetSmallerArea <- function(states) {
  i <- getItems(states, dim = 3)
  ii <- paste(i, i, sep = ".")
  withr::local_options(list(magclass_setMatching = TRUE))
  states2 <- states
  getSets(states, fulldim = FALSE)[3] <- "type1"
  getSets(states2, fulldim = FALSE)[3] <- "type2"
  states <- magpie_expand(states, states2)
  states2 <- magpie_expand(states2, states)
  states <- states[, , ii, invert = TRUE]
  states2 <- states2[, , ii, invert = TRUE]
  out <- pmin(states, states2)
  return(out)
}
