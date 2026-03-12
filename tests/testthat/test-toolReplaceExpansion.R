test_that("toolReplaceExpansion works", {
  x <- maxample("pop")
  expect_warning({
    x2 <- toolReplaceExpansion(x, "A2", "B1")
  }, "A2 is expanding considerably")
  expect_silent({
    x2 <- toolReplaceExpansion(x, "A2", "B1", warnThreshold = 3000, noteThreshold = 3000)
  })

  expect_true(toolMaxExpansion(x[, , "A2"]) > 0)
  expect_identical(toolMaxExpansion(x2[, , "A2"]), 0)
  expect_identical(dimSums(x, 3), dimSums(x2, 3))
})
