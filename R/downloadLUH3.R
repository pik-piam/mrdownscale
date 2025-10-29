#' downloadLUH3
#'
#' Download the LUH3 dataset (states, management, transitions, static).
#'
#' @param subtype one of states, management, transitions, cellArea
#' @return metadata list with URL, title, description, author, unit, version, release date
#' @author Pascal Sauer
downloadLUH3 <- function(subtype) {
  baseUrl <- paste0("https://esgf-node.ornl.gov/thredds/fileServer/user_pub_work/input4MIPs/",
                    "CMIP7/CMIP/UofMD/UofMD-landState-3-1-1/land/")
  urls <- c(states = paste0(baseUrl, "yr/multiple-states/gn/v20250325/",
                            "multiple-states_input4MIPs_landState_CMIP_UofMD-landState-3-1-1_gn_0850-2024.nc"),
            management = paste0(baseUrl, "yr/multiple-management/gn/v20250325/multiple-management_",
                                "input4MIPs_landState_CMIP_UofMD-landState-3-1-1_gn_0850-2024.nc"),
            transitions = paste0(baseUrl, "yr/multiple-transitions/gn/v20250325/multiple-transitions_",
                                 "input4MIPs_landState_CMIP_UofMD-landState-3-1-1_gn_0850-2023.nc"),
            static = paste0(baseUrl, "fx/multiple-static/gn/v20250325/",
                            "multiple-static_input4MIPs_landState_CMIP_UofMD-landState-3-1-1_gn.nc"))

  # increase timeout drastically because the whole file needs to be downloaded in that time (16GB)
  withr::local_options(timeout = max(3e6, getOption("timeout")))
  for (datasetName in names(urls)) {
    if (datasetName %in% c(subtype, "static")) {
      utils::download.file(urls[[datasetName]], destfile = basename(urls[[datasetName]]), mode = "wb")
    }
  }

  return(list(url          = paste0("https://aims2.llnl.gov/search?project=input4MIPs&activeFacets=",
                                    "%7B%22institution_id%22%3A%22UofMD%22%2C%22mip_era%22%3A%22CMIP7%22%7D"),
              title        = paste0("LUH3 historical data ", subtype),
              description  = paste0("LUH3 historical data ", subtype),
              author       = "Hurtt, Chini et al.",
              unit         = "1",
              version      = "3.1.1",
              release_date = "2025-03-25",
              license      = NA,
              reference    = NA))
}
