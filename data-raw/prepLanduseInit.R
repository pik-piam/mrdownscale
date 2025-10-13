withr::with_package("mrmagpie", {
  x <- calcOutput("LanduseInitialisation", nclasses = "seven",
                  aggregate = FALSE, cellular = TRUE, cells = "lpjcell",
                  input_magpie = TRUE)
  x <- x[, seq(1995, 2020, 5), ]
  write.magpie(x, "avl_land_t_0.5.mz")
})
