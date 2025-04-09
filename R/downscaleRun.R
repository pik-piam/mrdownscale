#' downscaleRun
#'
#' Downscale MAgPIE results from the given folder. The resulting tgz file will
#' be generated in the given folder.
#'
#' @param outputfolder path to a folder containing fulldata.gdx and clustermap_*.rds,
#' resulting tgz will be written here
#' @param ... arguments passed on to retrieveData,
#' e.g. model ("DOWNSCALEDMAGPIE" or "ESM"), rev, harmonizationPeriod
#' @return Invisibly, the path to the newly created tgz archive.
#'
#' @author Pascal Sauer
#' @export
downscaleRun <- function(outputfolder, ...) {
  outputfolder <- normalizePath(outputfolder)

  clustermap <- Sys.glob(file.path(outputfolder, "clustermap_*.rds"))
  gdx <- file.path(outputfolder, "fulldata.gdx")
  stopifnot(file.exists(gdx), length(clustermap) == 1)

  redirectSource("MagpieFulldataGdx", c(clustermap, gdx), linkOthers = FALSE)
  stopifnot(length(getConfig("redirections")) >= 1)
  retrieveData(..., outputfolder = outputfolder, puc = FALSE)
}
