#' downscaleRunESM
#'
#' Downscale MAgPIE results from the given folder to 0.25 degree resolution in
#' LUH2 format for ESMs. The resulting tgz file will be generated in the given folder.
#'
#' @param outputdir path to a folder containing fulldata.gdx and clustermap_*.rds,
#' resulting tgz will be written here
#' @param revision passed on to retrieveData, default: current date
#' @param scenario passed on to retrieveData, default: slightly modified folder name
#' @param ... additional arguments passed on to retrieveData
#' @return Invisibly, the path to the newly created tgz archive.
#'
#' @author Pascal Sauer
#' @export
downscaleRunESM <- function(outputdir, revision = NULL, scenario = NULL, ...) {
  if (is.null(revision)) {
    revision <- format(Sys.time(), "%Y-%m-%d")
  }

  if (is.null(scenario)) {
    scenario <- gsub("_", "-", sub("-mag-[0-9]+$", "", basename(outputdir)))
  }

  downscaleRun(outputdir, model = "ESM", rev = revision, scenario = scenario, progress = FALSE, ...)
}
