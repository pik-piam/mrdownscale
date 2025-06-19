setequalDims <- function(a, b) {
  return(setequal(getItems(a, 1), getItems(b, 1)) &&
           setequal(getItems(a, 2), getItems(b, 2)) &&
           setequal(getItems(a, 3), getItems(b, 3)))
}
