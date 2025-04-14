#' toolLandCategoriesMapping
#'
#' Computes a land category mapping between input and target data set via
#' retrieving mappings from these sources to an internally defined
#' reference categorization and merging them to a direct input-to-target
#' mapping.
#' Mappings to reference categories are stored internally in the package and
#' have to be added to it if new input and/or target data should be supported.
#'
#' @param input name of the land input source to be used
#' @param target name of the land target source to be used
#' @author Jan Philipp Dietrich
#'
toolLandCategoriesMapping <- function(input, target) {
  input2ref  <- toolGetMapping(paste0("referenceMappings/", input, ".csv"), where = "mrdownscale")
  target2ref <- toolGetMapping(paste0("referenceMappings/", target, ".csv"), where = "mrdownscale")

  if (!setequal(input2ref$reference, target2ref$reference)) {
    warning("Input map and output map contain inconsistent reference information")
  }
  map <- merge(input2ref, target2ref, by = "reference", suffixes = c("Input", "Output"))
  map$merge <- paste0(map$dataInput, "__", map$dataOutput)
  if (anyDuplicated(map$reference)) {
    warning("Insuficient granularity of reference categories, as a reference category is mapped more than once (\"",
            paste(unique(map$reference[duplicated(map$reference)]), collapse = "\", \""), "\").")
  }
  return(map)
}
