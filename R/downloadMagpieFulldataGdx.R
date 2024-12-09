downloadMagpieFulldataGdx <- function() {
  filename <- "sample_fulldata.gdx.tgz"
  url <- paste0("https://rse.pik-potsdam.de/data/magpie/public/", filename)
  utils::download.file(url, filename, mode = "wb")
  utils::untar(filename)
  stopifnot(file.exists("fulldata.gdx", "readme.txt", Sys.glob("clustermap_*.rds")))
  unlink(filename)
  return(list(url          = url,
              doi          = NA,
              title        = "MAgPIE fulldata.gdx & clustermap",
              description  = "A sample fulldata.gdx file (raw MAgPIE output) and the corresponding clustermap.",
              author       = "MAgPIE team",
              unit         = NA,
              version      = NA,
              release_date = NA,
              license      = NA,
              reference    = NA))
}
