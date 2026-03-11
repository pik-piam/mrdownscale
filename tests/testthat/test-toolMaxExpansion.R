test_that("toolMaxExpansion works", {
  x <- maxample("pop")
  expect_identical(toolMaxExpansion(x),
                   as.vector(x["SAS", "y2025", "A2"] - x["SAS", "y2015", "A2"]))
  expect_identical(toolMaxExpansion(x["EUR", , "A2"]),
                   as.vector(x["EUR", "y2005", "A2"] - x["EUR", "y1995", "A2"]))
  expect_error(toolMaxExpansion(x[, 1, ]), "nyears(x) >= 2 is not TRUE", fixed = TRUE)
})
