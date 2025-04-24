library(terra) # nolint: undesirable_function_linter.
library(magclass) # nolint: undesirable_function_linter.


extractTransitions <- function(file = "transitions.nc", years = 2014:2015, filterPrimary = TRUE) {
  # correct years to "years since 850"
  years <- years - 850
  # extract years from transitions.nc and return them in magpie format
  a <- rast(file)

  # only select transitions for the selected years
  filter <- grep(paste0("_to_.*_(", paste(years, collapse = "|"), ")$"), names(a))

  if (filterPrimary) {
    # remove primf and primn from the selection as they do not have
    # any bidirectional transitions (primary types cannot expand)
    filter2 <- grep("^(primf|primn)", names(a), invert = TRUE)
    filter <- intersect(filter, filter2)
  }
  b <- subset(a, filter)
  names(b) <- sub("^(.*)_to_(.*)_(.*)$", "\\3..\\1.\\2", names(b))
  m <- as.magpie(b)
  getYears(m) <- paste0("y", getYears(m, as.integer = TRUE) + 850)
  getSets(m, fulldim = FALSE)[3] <- "from.to"
  return(m)
}

extractStatesMean <- function(file = "states.nc", years = 1995:2016, filterPrimary = TRUE) {
  # correct years to "years since 850"
  years <- years - 850
  # extract years from transitions.nc and return them in magpie format
  a <- rast(file)

  # only select transitions for the selected years
  filter <- grep(paste0("_(", paste(years, collapse = "|"), ")$"), names(a))
  filter2 <- grep("^(secmb|secma)", names(a), invert = TRUE)
  filter <- intersect(filter, filter2)

  if (filterPrimary) {
    # remove primf and primn from the selection as they do not have
    # any bidirectional transitions (primary types cannot expand)
    filter2 <- grep("^(primf|primn)", names(a), invert = TRUE)
    filter <- intersect(filter, filter2)
  }
  b <- subset(a, filter)

  ty <- unique(sub("_.*$", "", names(b)))

  out <- NULL
  for (t in ty) {
    tmp <- as.magpie(mean(subset(b, grep(t, names(b)))))
    getItems(tmp, dim = 3) <- t
    out <- mbind(out, tmp)
  }
  return(out)
}

getBidirectionalTransitions <- function(b) {
  # get transition pairs
  pairs <- data.frame(from = getItems(b, dim = 3))
  pairs$to <- sub("^(.*)\\.(.*)$", "\\2.\\1", pairs$from)
  # create order independent id to filter for duplicate
  # entries (a.b-b.a and b.a-a.b)
  split <- strsplit(pairs$from, split = "\\.")
  split <- lapply(split, sort)
  pairs$id <- sapply(split, paste, collapse = ".") # nolint: undesirable_function_linter
  pairs <- pairs[!duplicated(pairs$id), ]
  # now filter for entries which actually exist in both directions
  pairs <- pairs[(pairs$to %in% getItems(b, dim = 3)) & (pairs$from %in% getItems(b, dim = 3)), ]
  pairs$id <- NULL

  # compute bidirectional transitions
  bmin <- pmin(b[, , pairs$from], setItems(b[, , pairs$to], dim = 3, pairs$from, raw = TRUE))
  return(bmin)
}


ncFolder <- "../../mrdownscale_data/LUH2v2/"
computationFolder <- "../../mrdownscale_data/"

# 20 year time horizon 1995 - 2016
# 4 year packages (to stay in memory limits)
range <- seq(1995, 2011, 4)
tSteps <- 4

# extract transition information in digestable blocks
for (i in range) {
  file <- file.path(computationFolder, paste0("transitions_", i, "to", i + tSteps - 1, ".mz"))
  write.magpie(extractTransitions(file.path(ncFolder, "transitions.nc"), years = (i:(i + 3))),
               file)
  cat(basename(file), "written.")
}

# read in blocks and generate single 20 year block
out <- NULL
for (i in range) {
  file <- file.path(computationFolder, paste0("transitions_", i, "to", i + tSteps - 1, ".mz"))
  m <- read.magpie(file)
  m[m < 0] <- 0 # fix data errors (negative values due to rounding issues)
  out <- mbind(out, m)
}
write.magpie(out, file.path(computationFolder, "transitions_1995to2015.mz"))

# compute bidirectional transitions (transitions which happen in a cell in the
# same size in both directions or in other words: transitions, which don't lead
# to net transitions)

a <- read.magpie(file.path(computationFolder, "transitions_1995to2015.mz"))
abi <- getBidirectionalTransitions(a)
write.magpie(abi, file.path(computationFolder, "bidirectionalTransitions_1995to2015.mz"))
# compute mean bilateral transitions over the selected time period
meanBi <- dimSums(abi, dim = 2) / dim(abi)[2]
write.magpie(meanBi, file.path(computationFolder, "meanBidirectionalTransitions_1995to2015.mz"))

# calculate corresponding mean state levels for the corresponding period
states <- extractStatesMean(file.path(ncFolder, "states.nc"), years = 1995:2016)
write.magpie(states, file.path(computationFolder, "meanStates_1995to2016.mz"))

### New bilateral share calculation approach     ###
### (provide shares to both involved land types) ###

computeMeanBiShares <- function(meanBi, states) {
  # remove very small values
  meanBi[meanBi < 10^-6] <- 0

  # expand meanBi to have every transition for both directions
  direction2 <- sub("^(.*)\\.(.*)$", "\\2.\\1", getItems(meanBi, dim = 3))
  meanBi <- mbind(meanBi, setItems(meanBi, direction2, raw = TRUE, dim = 3))

  getSets(states)[4] <- "from"
  withr::with_options(list(magclass_setMatching = TRUE, magclass_sizeLimit = 10e+10), {
    x <- meanBi / (states + 10^-10)
  })

  # make sure that sum of shares for all categories the land comes from are not
  # above 0.95 (not more land is distributed than available + some safety
  # threshold of 5%)
  test <- dimSums(x, dim = "to")
  if (any(test > 0.95)) {
    corr <- test
    corr[test > 0.95] <- 0.95 / test[test > 0.95]
    corr[test <= 0.95] <- 1
    withr::with_options(list(magclass_setMatching = TRUE, magclass_sizeLimit = 10e+10), {
      x <- x * corr
    })
    test2 <- dimSums(x, dim = "to")
    if (any(test2 > 0.951)) stop("Something went wrong in the share correction!")
  }
  return(x)
}

computationFolder <- "../../mrdownscale_data/"
meanBi <- read.magpie(file.path(computationFolder, "meanBidirectionalTransitions_1995to2015.mz"))
states <- read.magpie(file.path(computationFolder, "meanStates_1995to2016.mz"))
x <- computeMeanBiShares(meanBi, states)
write.magpie(x, file.path(computationFolder, "meanBidirectionalTransitionsShares_1995to2015.mz"))
