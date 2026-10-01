items <- c("primf", "primn", "secdf", "secdn", "urban", "pltns",
           "pastr", "range", "c3ann_rainfed", "c4ann_rainfed")

# runs the expression and returns its value together with all messages and
# warnings emitted by toolStatusMessage (which uses warnings for "warn" status)
captureConditions <- function(expr) {
  conditions <- character()
  value <- withCallingHandlers(
    expr,
    message = function(m) {
      conditions <<- unique(c(conditions, conditionMessage(m)))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      conditions <<- unique(c(conditions, conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  return(list(value = value, conditions = conditions))
}

expectCondition <- function(conditions, regexp) {
  expect_true(any(grepl(regexp, conditions)))
}

test_that("toolHarmonizeAbsoluteChanges works", {
  xTarget <- new.magpie(c("reg.one", "reg.two"), years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.one", year, ] <- c(40, 10, 10, 5, 5, 0, 30, 0, 0, 0)
    xTarget["reg.two", year, ] <- c(30, 10, 20, 5, 5, 0, 30, 0, 0, 0)
  }

  xInput <- new.magpie(c("reg.one", "reg.two"), years = c(2020, 2025, 2030), names = items, fill = 0)
  xInput["reg.one", 2020, ] <- c(20, 10, 20, 5, 10, 0, 35, 0, 0, 0)
  xInput["reg.one", 2025, ] <- c(20, 10, 22, 5, 10, 0, 33, 0, 0, 0)
  xInput["reg.one", 2030, ] <- c(19, 10, 24, 5, 10, 0, 32, 0, 0, 0)
  xInput["reg.two", 2020, ] <- c(30, 8, 10, 7, 5, 0, 40, 0, 0, 0)
  xInput["reg.two", 2025, ] <- c(32, 8, 11, 7, 5, 0, 37, 0, 0, 0)
  xInput["reg.two", 2030, ] <- c(33, 8, 12, 7, 5, 0, 35, 0, 0, 0)

  run <- captureConditions(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2020, 2020)))
  out <- run$value

  # no negative absolute changes, so no value is reset to zero and no trend is altered
  # (except prim expansion which is checked below)
  expectCondition(run$conditions, "0 Mha per timestep")

  expect_equal(getYears(out, as.integer = TRUE), c(2010, 2020, 2025, 2030))

  # before and at the harmonization year target data is used
  expect_equal(as.vector(out["reg.one", 2010, ]), c(40, 10, 10, 5, 5, 0, 30, 0, 0, 0))
  expect_equal(as.vector(out["reg.two", 2010, ]), c(30, 10, 20, 5, 5, 0, 30, 0, 0, 0))
  expect_equal(as.vector(out["reg.one", 2020, ]), c(40, 10, 10, 5, 5, 0, 30, 0, 0, 0))

  # after the harmonization year absolute changes from input are applied:
  # secdf grows by 2 Mha from 2020 to 2025 -> target secdf 10 Mha + 2 = 12 Mha
  expect_equal(as.vector(out["reg.one", 2025, ]), c(40, 10, 12, 5, 5, 0, 28, 0, 0, 0))
  expect_equal(as.vector(out["reg.one", 2030, ]), c(39, 10, 14, 5, 5, 0, 27, 0, 0, 0))

  # primf expansion in the input data is replaced with secdf
  expect_equal(as.vector(out["reg.two", 2025, ]), c(30, 10, 23, 5, 5, 0, 27, 0, 0, 0))
  expect_equal(as.vector(out["reg.two", 2030, ]), c(30, 10, 25, 5, 5, 0, 25, 0, 0, 0))

  # total area is constant over time and unchanged
  expect_equal(as.vector(dimSums(out, dim = 3)), rep(100, 8))

  # invalid harmonizationPeriod
  expect_error(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2015, 2015)),
               "hy %in% inputYears is not TRUE")
  expect_error(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2010, 2020)))
  expect_error(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = 2020))
})

test_that("toolHarmonizeAbsoluteChanges reports zero deviation when no corrections are needed", {
  xTarget <- new.magpie("reg.rep", years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.rep", year, ] <- c(40, 10, 10, 5, 5, 0, 30, 0, 0, 0)
  }

  xInput <- new.magpie("reg.rep", years = c(2020, 2025), names = items, fill = 0)
  xInput["reg.rep", 2020, ] <- c(20, 10, 20, 5, 10, 0, 35, 0, 0, 0)
  # no negative absolute changes and no prim expansion, so the input trend is
  # reproduced exactly
  xInput["reg.rep", 2025, ] <- c(20, 10, 22, 5, 10, 0, 33, 0, 0, 0)

  run <- captureConditions(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2020, 2020)))
  out <- run$value

  expect_equal(as.vector(out["reg.rep", 2025, ]), c(40, 10, 12, 5, 5, 0, 28, 0, 0, 0))

  report <- grep("Correction of negative values", run$conditions, value = TRUE)
  expect_length(report, 1)
  expect_true(grepl("on average 0 Mha per timestep (mean over the 1 timestep in the input after 2020) were",
                    report, fixed = TRUE))
  expect_true(grepl("affected 0% of all cell/year/category values in 0% of all cells", report, fixed = TRUE))
  expect_true(grepl("Deviation from input trend in 2025: 0 Mha (input change 4 Mha, actual change 4 Mha, factor 1)",
                    report, fixed = TRUE))
  expect_true(grepl("Harmonization quality: 100%", report, fixed = TRUE))

  # one message per group, reporting the total group deviation followed by a
  # table of all categories in the group
  forestGroup <- grep("category group \"forest and other land\"", run$conditions, value = TRUE)
  expect_length(forestGroup, 1)
  # CSV table with total in the first row, sorted by abs(1 - factor), ties in
  # group order, n/a last
  expect_true(grepl(paste0("variable, factor, input, actual, diff\n",
                           "total   ,      1,     2,      2,    0\n",
                           "secdf   ,      1,     2,      2,    0\n",
                           "pltns   ,    n/a,     0,      0,    0\n",
                           "primf   ,    n/a,     0,      0,    0\n",
                           "primn   ,    n/a,     0,      0,    0\n",
                           "secdn   ,    n/a,     0,      0,    0"),
                    forestGroup, fixed = TRUE))
  cropGroup <- grep("category group \"cropland\"", run$conditions, value = TRUE)
  expect_length(cropGroup, 1)
  expect_true(grepl(paste0("variable     , factor, input, actual, diff\n",
                           "total        ,    n/a,     0,      0,    0\n",
                           "c3ann_rainfed,    n/a,     0,      0,    0\n",
                           "c4ann_rainfed,    n/a,     0,      0,    0"),
                    cropGroup, fixed = TRUE))
  pastureGroup <- grep("category group \"pasture and rangeland\"", run$conditions, value = TRUE)
  expect_length(pastureGroup, 1)
  # pastr loses the same 2 Mha secdf gains and its trend is reproduced exactly
  expect_true(grepl(paste0("variable, factor, input, actual, diff\n",
                           "total   ,      1,     2,      2,    0\n",
                           "pastr   ,      1,    -2,     -2,    0\n",
                           "range   ,    n/a,     0,      0,    0"),
                    pastureGroup, fixed = TRUE))
  # urban is never scaled by the corrections and is not reported separately,
  # and there is no ungrouped category anymore (all categories are in a group)
  expect_false(any(grepl("category group \"urban\"", run$conditions, fixed = TRUE)))
  expect_false(any(grepl("category group \"not in any group\"", run$conditions, fixed = TRUE)))
})

test_that("toolHarmonizeAbsoluteChanges avoids negative values", {
  xTarget <- new.magpie("reg.three", years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.three", year, ] <- c(5, 5, 10, 5, 5, 0, 70, 0, 0, 0)
  }

  xInput <- new.magpie("reg.three", years = c(2020, 2025), names = items, fill = 0)
  xInput["reg.three", 2020, ] <- c(50, 5, 10, 5, 5, 0, 25, 0, 0, 0)
  # input loses 10 Mha primf, but target only has 5 Mha primf in 2020
  xInput["reg.three", 2025, ] <- c(40, 5, 10, 5, 5, 0, 35, 0, 0, 0)

  run <- captureConditions(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2020, 2020)))
  out <- run$value

  # 5 Mha primf were clipped to zero (1 of 10 entries, in 100% of cells)
  expectCondition(run$conditions,
                  "5 Mha per timestep \\(mean over the 1 timestep in the input after 2020\\)")
  expectCondition(run$conditions,
                  "affected 10% of all cell/year/category values in 100% of all cells")
  expectCondition(run$conditions, "\\[!\\]")
  # half of the 20 Mha of input change (primf -10, pastr +10) was distorted by
  # the corrections (10 Mha of |out - raw|), so only 50% of the signal remains
  report <- grep("Correction of negative values", run$conditions, value = TRUE)
  expect_true(grepl("Harmonization quality: 50%", report, fixed = TRUE))

  # categories with sign-flipped trends (factor -Inf) are listed first in group
  # order, then the nearly intact trend (factor 0.5), n/a (pltns, no change at
  # all) last
  forestGroup <- grep("category group \"forest and other land\"", run$conditions, value = TRUE)
  expect_length(forestGroup, 1)
  # decimal separators are vertically aligned, total is the first row
  expect_true(grepl(paste0("variable, factor  , input, actual   , diff   \n",
                           "total   ,      1  ,    10,     10   ,   10   \n",
                           "secdf   ,   -Inf  ,     0,     -2.5 ,   -2.5 \n",
                           "primn   ,   -Inf  ,     0,     -1.25,   -1.25\n",
                           "secdn   ,   -Inf  ,     0,     -1.25,   -1.25\n",
                           "primf   ,      0.5,   -10,     -5   ,    5   \n",
                           "pltns   ,    n/a  ,     0,      0   ,    0"),
                    forestGroup, fixed = TRUE))

  expect_equal(as.vector(out["reg.three", 2025, "primf"]), 0)
  expect_true(all(out >= 0))
  expect_equal(as.vector(dimSums(out, dim = 3)), rep(100, 3))
  # target data before the harmonization year is untouched
  expect_equal(as.vector(out["reg.three", 2010, ]), c(5, 5, 10, 5, 5, 0, 70, 0, 0, 0))
  expect_equal(as.vector(out["reg.three", 2020, ]), c(5, 5, 10, 5, 5, 0, 70, 0, 0, 0))
})

test_that("toolHarmonizeAbsoluteChanges compensates negative forest area within the forest group", {
  xTarget <- new.magpie("reg.four", years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.four", year, ] <- c(40, 10, 10, 5, 5, 0, 30, 0, 0, 0)
  }

  xInput <- new.magpie("reg.four", years = c(2020, 2025), names = items, fill = 0)
  xInput["reg.four", 2020, ] <- c(20, 10, 20, 5, 10, 0, 35, 0, 0, 0)
  # input loses 15 Mha secdf, target only has 10 Mha secdf in 2020
  xInput["reg.four", 2025, ] <- c(20, 10, 5, 5, 10, 0, 50, 0, 0, 0)

  run <- captureConditions(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2020, 2020)))
  out <- run$value

  expectCondition(run$conditions, "5 Mha per timestep")

  # the secdf shortfall of 5 Mha is compensated by scaling down the other
  # categories of the forest group (primf, primn, secdf, secdn) proportionally,
  # secdf itself is set to 0
  expect_equal(as.vector(out["reg.four", 2025, c("primf", "primn", "secdf", "secdn", "urban", "pastr")]),
               c(400 / 11, 100 / 11, 0, 50 / 11, 5, 45))
  # the forest group keeps its total area
  expect_equal(as.vector(dimSums(out["reg.four", 2025, c("primf", "primn", "secdf", "secdn")], dim = 3)), 50)
  expect_true(all(out >= 0))
  expect_equal(as.vector(dimSums(out, dim = 3)), rep(100, 3))
  expect_equal(as.vector(out["reg.four", 2020, ]), c(40, 10, 10, 5, 5, 0, 30, 0, 0, 0))
})

test_that("toolHarmonizeAbsoluteChanges scales all categories except urban", {
  xTarget <- new.magpie("reg.five", years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.five", year, ] <- c(40, 4, 10, 1, 5, 0, 40, 0, 0, 0)
  }

  xInput <- new.magpie("reg.five", years = c(2020, 2025), names = items, fill = 0)
  xInput["reg.five", 2020, ] <- c(20, 4, 20, 1, 5, 0, 50, 0, 0, 0)
  # input loses 20 Mha secdf, more than secdf + primn + secdn can cover within
  # the forest group after clipping
  xInput["reg.five", 2025, ] <- c(20, 4, 0, 1, 5, 0, 70, 0, 0, 0)

  run <- captureConditions(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2020, 2020)))
  out <- run$value

  # 10 Mha secdf were clipped to zero
  expectCondition(run$conditions, "10 Mha per timestep")

  # the forest group is scaled down so that it keeps its total area of 35 Mha,
  # urban is untouched and pastr does not need any further scaling
  expect_equal(as.vector(out["reg.five", 2025, c("primf", "primn", "secdf", "secdn")]),
               c(280 / 9, 28 / 9, 0, 7 / 9))
  expect_equal(as.vector(out["reg.five", 2025, c("urban", "pastr")]), c(5, 60))
  expect_true(all(out >= 0))
  expect_equal(as.vector(dimSums(out, dim = 3)), rep(100, 3))
})

test_that("toolHarmonizeAbsoluteChanges scales down excess area and replaces prim expansion", {
  xTarget <- new.magpie("reg.seven", years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.seven", year, ] <- c(40, 4, 10, 1, 5, 0, 40, 0, 0, 0)
  }

  xInput <- new.magpie("reg.seven", years = c(2020, 2025), names = items, fill = 0)
  xInput["reg.seven", 2020, ] <- c(20, 4, 20, 1, 5, 0, 50, 0, 0, 0)
  # prim gains of 76 Mha make prim overshoot the total area once negative
  # categories are clipped to 0
  xInput["reg.seven", 2025, ] <- c(90, 10, 0, 0, 0, 0, 0, 0, 0, 0)

  run <- captureConditions(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2020, 2020)))
  out <- run$value

  # secdf (-10 Mha) and pastr (-10 Mha) were clipped to zero
  expectCondition(run$conditions, "20 Mha per timestep")

  # after clipping and scaling, toolReplaceExpansion moves the prim expansions
  # into secdf and secdn
  expect_equal(as.vector(out["reg.seven", 2025, ]), c(40, 4, 155 / 3, 13 / 3, 0, 0, 0, 0, 0, 0))
  expect_true(all(out >= 0))
  expect_equal(as.vector(dimSums(out, dim = 3)), rep(100, 3))
})

test_that("toolHarmonizeAbsoluteChanges refuses to scale urban", {
  xTarget <- new.magpie("reg.eight", years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.eight", year, ] <- c(0, 0, 10, 5, 80, 0, 5, 0, 0, 0)
  }

  xInput <- new.magpie("reg.eight", years = c(2020, 2025), names = items, fill = 0)
  xInput["reg.eight", 2020, ] <- c(0, 0, 10, 5, 10, 0, 75, 0, 0, 0)
  # urban gain of 90 Mha makes urban alone overshoot the total area once pastr
  # is clipped to 0, which cannot be fixed without scaling urban
  xInput["reg.eight", 2025, ] <- c(0, 0, 0, 0, 100, 0, 0, 0, 0, 0)

  expect_error(
    suppressWarnings(suppressMessages(
      toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2020, 2020))
    )),
    "all(nonUrbanTarget >= -10^-5) is not TRUE", fixed = TRUE
  )
})

test_that("toolHarmonizeAbsoluteChanges sets a group with a negative total area to zero", {
  xTarget <- new.magpie("reg.nine", years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.nine", year, ] <- c(50, 0, 0, 0, 0, 0, 50, 0, 0, 0)
  }

  xInput <- new.magpie("reg.nine", years = c(2020, 2025), names = items, fill = 0)
  xInput["reg.nine", 2020, ] <- c(90, 0, 0, 0, 0, 0, 10, 0, 0, 0)
  # primf loses 90 Mha, more than the 50 Mha of the target, so the total area of
  # the forest group becomes negative and the whole group is set to zero
  xInput["reg.nine", 2025, ] <- c(0, 0, 0, 0, 0, 0, 100, 0, 0, 0)

  run <- captureConditions(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2020, 2020)))
  out <- run$value

  expect_equal(as.vector(out["reg.nine", 2025, c("primf", "urban", "pastr")]), c(0, 0, 100))
  expect_true(all(out >= 0))
  expect_equal(as.vector(dimSums(out, dim = 3)), rep(100, 3))
})

test_that("toolHarmonizeAbsoluteChanges compensates negative cropland within the cropland group", {
  xTarget <- new.magpie("reg.crop", years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.crop", year, ] <- c(5, 0, 0, 0, 5, 0, 42, 1, 30, 17)
  }

  xInput <- new.magpie("reg.crop", years = c(2020, 2025), names = items, fill = 0)
  xInput["reg.crop", 2020, ] <- c(5, 0, 0, 0, 5, 0, 22, 1, 50, 17)
  # input loses 40 Mha of c3ann_rainfed, more than the 30 Mha in the target
  xInput["reg.crop", 2025, ] <- c(5, 0, 0, 0, 5, 0, 22, 1, 10, 57)

  run <- captureConditions(toolHarmonizeAbsoluteChanges(xInput, xTarget, harmonizationPeriod = c(2020, 2020)))
  out <- run$value

  # raw c3ann_rainfed drops 20 Mha below the 30 Mha of the target in the
  # harmonization year, 10 Mha were clipped to zero
  expectCondition(run$conditions, "10 Mha per timestep")

  # the c3ann_rainfed shortfall is covered by c4ann_rainfed, the cropland group
  # keeps its total area of 47 Mha
  expect_equal(as.vector(out["reg.crop", 2025, c("c3ann_rainfed", "c4ann_rainfed")]), c(0, 47))
  expect_true(all(out >= 0))
  expect_equal(as.vector(dimSums(out, dim = 3)), rep(100, 3))
})

test_that("toolHarmonizeAbsoluteChanges rejects inconsistent or invalid input data", {
  xTarget <- new.magpie("reg.six", years = c(2010, 2020), names = items, fill = 0)
  for (year in c(2010, 2020)) {
    xTarget["reg.six", year, ] <- c(40, 10, 10, 5, 5, 0, 30, 0, 0, 0)
  }

  xInput <- new.magpie("reg.six", years = c(2020, 2025), names = items, fill = 0)
  xInput["reg.six", 2020, ] <- c(20, 10, 20, 5, 10, 0, 35, 0, 0, 0)
  xInput["reg.six", 2025, ] <- c(20, 10, 22, 5, 10, 0, 33, 0, 0, 0)

  # total area of input is not constant over time
  brokenInput <- xInput
  brokenInput["reg.six", 2025, "pastr"] <- 30
  expect_error(toolHarmonizeAbsoluteChanges(brokenInput, xTarget, c(2020, 2020)))

  # total area of target is not constant over time
  brokenTarget <- xTarget
  brokenTarget["reg.six", 2010, "pastr"] <- 25
  expect_error(toolHarmonizeAbsoluteChanges(xInput, brokenTarget, c(2020, 2020)))

  # input data contains NA values
  naInput <- xInput
  naInput["reg.six", 2025, "urban"] <- NA_real_
  expect_error(toolHarmonizeAbsoluteChanges(naInput, xTarget, c(2020, 2020)))
})

test_that("toolGetHarmonizer returns the absoluteChanges harmonizer", {
  harmonizer <- toolGetHarmonizer("absoluteChanges")
  expect_true(is.function(harmonizer))
  expect_error(toolGetHarmonizer("nonexistent"))
})
