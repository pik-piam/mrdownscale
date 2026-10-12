test_that("the input's primary and secondary split does not survive harmonization", {
  # why toolIAMCLandCategories can set an unusable forest split aside: from the
  # harmonization start on, primary forest comes from the target's trajectory
  # and the harmonized forest total, not from the input's split
  categories <- c("primf", "secdf")
  targetYears <- c(2015:2025, 2030, 2040)
  xTarget <- new.magpie("A.1", targetYears, categories, fill = 0)
  xTarget[, 2015:2025, "primf"] <- 100 - 2 * (0:10)
  xTarget[, c(2030, 2040), "primf"] <- c(70, 50)
  xTarget[, , "secdf"] <- 200

  input <- function(primf, secdf) {
    x <- new.magpie("A.1", c(2025, 2030, 2040, 2050), categories, fill = 0)
    x[, , "primf"] <- primf
    x[, , "secdf"] <- secdf
    x
  }
  # the same forest total, split two ways, including all of it as secondary
  split <- toolHarmonizeFadeForest(input(80, 200), xTarget, harmonizationPeriod = c(2025, 2050))
  whole <- toolHarmonizeFadeForest(input(0, 280), xTarget, harmonizationPeriod = c(2025, 2050))

  fromStart <- getYears(split, as.integer = TRUE) >= 2025
  expect_equal(split[, fromStart, ], whole[, fromStart, ])
})
