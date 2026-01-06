# run this on PIK HPC in the LanduseInit madrat source folder
withr::with_package("mrlandcore", {
  x <- calcOutput("LanduseInitialisation",
                  nclasses = "seven", # should switch to nclasses = "nine"
                  cellular = TRUE, input_magpie = TRUE, aggregate = FALSE)
  x <- x[, seq(1995, 2020, 5), ]
  write.magpie(x, "avl_land_t_0.5.mz")
})
