downloadLanduseInit <- function(subtype = "rev4.126") {
  if (subtype == "rev4.111") {
    tgzName <- "rev4.111_h12_fd712c0b_cellularmagpie_c200_MRI-ESM2-0-ssp370_lpjml-8e6c5eb1.tgz"
    releaseDate <- "2024-09-24"
  } else if (subtype == "rev4.126") {
    tgzName <- "rev4.126_h12_1b5c3817_cellularmagpie_c200_MRI-ESM2-0-ssp245_lpjml-8e6c5eb1.tgz"
    releaseDate <- "2025-10-23"
  }

  url <- paste0("https://rse.pik-potsdam.de/data/magpie/public/", tgzName)
  utils::download.file(url, "cellularmagpie.tgz", mode = "wb")
  utils::untar("cellularmagpie.tgz", "./avl_land_t_0.5.mz")
  unlink("cellularmagpie.tgz")
  return(list(url          = url,
              doi          = NA,
              title        = "LanduseInitialization",
              description  = "Land use initialization data for use in MAgPIE.",
              author       = "MAgPIE team",
              unit         = "Mha",
              version      = subtype,
              release_date = releaseDate,
              license      = NA,
              reference    = NA))
}
