test_that("the nonland report asks for the input it was given, magpie by default", {
  asked <- NULL
  local_mocked_bindings(
    calcOutput = function(type, ...) {
      if (type == "NonlandHighRes") {
        asked <<- list(...)$input
        stop("stop after recording the request")
      }
      stop("unexpected calcOutput call: ", type)
    },
    readSource = function(...) NULL
  )
  args <- list(outputFormat = "ScenarioMIP", harmonizationPeriod = c(2025, 2050),
               yearsSubset = 2020:2100, harmonization = "fadeForest", downscaling = "magpieClassic")

  expect_error(do.call(calcNonlandReport, c(args, input = "witch")), "stop after recording")
  expect_identical(asked, "witch")

  expect_error(do.call(calcNonlandReport, args), "stop after recording")
  expect_identical(asked, "magpie")
})
