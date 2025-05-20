# pmin does not care about the order of items in a magclass object's dim
# mpmin reoders items in each dim so they are in the same order
# this function should eventually be moved into the magclass package
mpmin <- function(a, b) {
  stopifnot(is.magpie(a), is.magpie(b))

  if (identical(dimnames(a), dimnames(b))) {
    return(pmin(a, b))
  }

  stopifnot(identical(dim(a), dim(b)))

  if (dim(a)[1] > 1) {
    stopifnot(setequal(getItems(a, 1), getItems(b, 1)))
    b <- b[getItems(a, 1), , ]
  }
  if (dim(a)[2] > 1) {
    stopifnot(setequal(getItems(a, 2), getItems(b, 2)))
    b <- b[, getItems(a, 2), ]
  }
  if (dim(a)[3] > 1) {
    stopifnot(setequal(getItems(a, 3), getItems(b, 3)))
    b <- b[, , getItems(a, 3)]
  }

  return(pmin(a, b))
}
