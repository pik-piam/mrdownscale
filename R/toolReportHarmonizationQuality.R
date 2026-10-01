#' toolReportHarmonizationQuality
#'
#' Reports how much the corrections applied during harmonization (see
#' toolHarmonizeAbsoluteChanges) deviate from the absolute changes of the input
#' data: the negative area that was reset to zero, a single harmonization
#' quality number (the share of the input trend after the harmonization year
#' which the output reproduces, 100% if the corrections never deviated from the
#' input trend), and the deviation from the input trend in 2050, first overall
#' and then per category group a table
#'
#' @param xRaw harmonized data before the corrections, as magpie object, with
#'   years after the harmonization year
#' @param xOut harmonized data after the corrections, as magpie object,
#'   including the harmonization year
#' @param harmonizationPeriod Two identical integer values, the harmonization
#'   year
#' @param inputYears integer vector of all years present in the input data
#' @param groups Named list of character vectors with the category groups to be
#'   reported separately; every category must be in one of the groups or be
#'   "urban"
#'
#' @author Pascal Sauer
toolReportHarmonizationQuality <- function(xRaw, xOut, harmonizationPeriod, inputYears, groups) {
  hy <- harmonizationPeriod[1]
  stopifnot(setequal(c(unlist(groups), "urban"), getItems(xOut, 3)))
  yearsAfter <- inputYears[inputYears > hy]

  negativeArea <- -sum(pmin(xRaw, 0))
  negativeShare <- 100 * sum(xRaw < 0) / length(xRaw)
  negativeCells <- 100 * sum(dimSums(xRaw < 0, c(2, 3)) > 0) / nrow(xRaw)
  # report the deviation from the input trend in 2050
  reportYear <- if (any(yearsAfter >= 2050)) min(yearsAfter[yearsAfter >= 2050]) else max(yearsAfter)
  deviation <- as.numeric(dimSums(xOut[, reportYear, ] - xRaw[, reportYear, ], c(1, 2)))
  deviation[abs(deviation) < 10^-5] <- 0
  names(deviation) <- getItems(xOut, 3)
  inputChange <- as.numeric(dimSums(xRaw[, reportYear, ] - xOut[, hy, ], c(1, 2)))
  inputChange[abs(inputChange) < 10^-5] <- 0
  names(inputChange) <- getItems(xOut, 3)
  change <- as.numeric(dimSums(xOut[, reportYear, ] - xOut[, hy, ], c(1, 2)))
  change[abs(change) < 10^-5] <- 0
  names(change) <- getItems(xOut, 3)

  .sumAbs <- function(x, group) sum(abs(x[group]))
  .formatNumber <- function(x) as.character(signif(x, 3))
  # factor < 1 means part of the input trend was lost, negative means the
  # actual trend has the opposite sign of the input trend
  .formatFactor <- function(actual, input) {
    factor <- signif(actual / input, 3)
    return(ifelse(is.nan(factor), "n/a", as.character(factor)))
  }
  # align a column of number-like strings vertically at the decimal separator
  .alignDecimal <- function(x) {
    intPart <- sub("\\..*$", "", x)
    fracPart <- ifelse(grepl(".", x, fixed = TRUE), sub(".*\\.", "", x), NA_character_)
    fracWidth <- suppressWarnings(max(nchar(fracPart), na.rm = TRUE)) # suppress because all NA would warn
    intText <- format(intPart, width = max(nchar(intPart)), justify = "right")
    if (is.finite(fracWidth) && fracWidth > 0) {
      fracText <- ifelse(is.na(fracPart), "", fracPart)
      return(paste0(intText,
                    ifelse(is.na(fracPart), " ", "."),
                    fracText,
                    strrep(" ", fracWidth - nchar(fracText))))
    }
    return(intText)
  }

  overallDeviation <- .sumAbs(deviation, getItems(xOut, 3))
  overallInputChange <- .sumAbs(inputChange, getItems(xOut, 3))
  overallChange <- .sumAbs(change, getItems(xOut, 3))
  # single number over all timesteps: the share of the input trend which the
  # corrections retained, 100 if they never deviated from the input trend, 0 if
  # they distorted more area than the entire input change covered
  totalDeviation <- as.numeric(sum(abs(xOut[, yearsAfter, ] - xRaw)))
  totalInputChange <- as.numeric(sum(abs(xRaw - setYears(xOut[, hy, ], NULL))))
  quality <- if (totalInputChange == 0) 100 else 100 * max(0, 1 - totalDeviation / totalInputChange)
  timestepWord <- if (length(yearsAfter) == 1) "timestep" else "timesteps"
  toolStatusMessage("note",
                    paste0("Correction of negative values: on average ",
                           signif(negativeArea / length(yearsAfter), 3),
                           " Mha per timestep (mean over the ", length(yearsAfter),
                           " ", timestepWord, " in the input after ", hy,
                           ") were below 0 and reset to 0, the excess area taken from ",
                           "other categories; affected ", round(negativeShare, 1),
                           "% of all cell/year/category values in ", round(negativeCells, 1),
                           "% of all cells.\n",
                           "Deviation from input trend in ", reportYear, ": ",
                           signif(overallDeviation, 3), " Mha (input change ",
                           signif(overallInputChange, 3), " Mha, actual change ",
                           signif(overallChange, 3), " Mha, factor ",
                           .formatFactor(overallChange, overallInputChange), ")\n",
                           "Harmonization quality: ", signif(quality, 3),
                           "% (retained share of the input trend after ", hy,
                           " across all timesteps)"))

  # urban is not reported separately, it is never scaled by the corrections
  reportGroups <- list(
    "forest and other land" = groups$forestOther,
    cropland = groups$cropland,
    "pasture and rangeland" = groups$pastureRangeland
  )
  for (groupName in names(reportGroups)) {
    groupItems <- reportGroups[[groupName]]
    if (length(groupItems) == 0) {
      next
    }
    groupDeviation <- deviation[groupItems]
    groupInputChange <- inputChange[groupItems]
    groupChange <- change[groupItems]
    groupFactor <- signif(groupChange / groupInputChange, 3)
    # sort by how far the factor is from 1 (= trend reproduced), n/a factors last
    sortKey <- abs(1 - groupFactor)
    sortKey[is.na(sortKey)] <- -Inf
    orderIndex <- order(sortKey, decreasing = TRUE)
    groupDeviation <- groupDeviation[orderIndex]
    groupInputChange <- groupInputChange[orderIndex]
    groupChange <- groupChange[orderIndex]
    groupFactor <- groupFactor[orderIndex]
    groupTotalDeviation <- sum(abs(groupDeviation))
    groupTotalInputChange <- .sumAbs(inputChange, groupItems)
    groupTotalChange <- .sumAbs(change, groupItems)
    tableRows <- rbind(
      c("variable", "factor", "input", "actual", "diff"),
      c("total", .formatFactor(groupTotalChange, groupTotalInputChange),
        .formatNumber(groupTotalInputChange),
        .formatNumber(groupTotalChange),
        .formatNumber(groupTotalDeviation)),
      cbind(names(groupDeviation),
            ifelse(is.na(groupFactor), "n/a", as.character(groupFactor)),
            .formatNumber(groupInputChange), .formatNumber(groupChange),
            .formatNumber(groupDeviation))
    )
    tableText <- apply(cbind(format(tableRows[, 1], width = max(nchar(tableRows[, 1]))),
                             .alignDecimal(tableRows[, 2]),
                             .alignDecimal(tableRows[, 3]),
                             .alignDecimal(tableRows[, 4]),
                             .alignDecimal(tableRows[, 5])),
                       1, paste, collapse = ", ")
    toolStatusMessage("note",
                      paste0("Deviation from input trend in category group \"", groupName,
                             "\" in ", reportYear, ":\n",
                             paste(tableText, collapse = "\n")))
  }
  return(invisible(NULL))
}
