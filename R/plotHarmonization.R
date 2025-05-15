plotHarmonization <- function(input, target, harmonizationPeriod) {
  lir <- calcOutput("LandInputRecategorized", input = input, target = target, aggregate = FALSE)
  ltex <- calcOutput("LandTargetExtrapolated", input = input, target = target,
                     harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)
  lh <- calcOutput("LandHarmonized", aggregate = FALSE, input = input,
                   target = target, harmonizationPeriod = harmonizationPeriod)

  crops <- grep("^c[34](ann|per|nfx)", getItems(lir, 3), value = TRUE)
  years <- getYears(lh, TRUE)
  years <- years[years >= harmonizationPeriod[[1]] & years < harmonizationPeriod[[2]]]

  lir <- dimSums(lir[, years, crops], c(1, 3))
  ltex <- dimSums(ltex[, years, crops], c(1, 3))
  lh <- dimSums(lh[, years, crops], c(1, 3))
  x <- mbind(magclass::setNames(lir, "model"),
             magclass::setNames(ltex, "historical extrapolated"),
             magclass::setNames(lh, "harmonized"))
  x <- as.data.frame(x)
  names(x)[4:5] <- c("Data", "CroplandMha")
  library(ggplot2)
  fontsize <- 25
  ggplot(data = x, aes(x = Year, y = CroplandMha, group = Data, color = Data)) +
    ggplot2::geom_line(size = 1.5) +
    theme(text = element_text(size = fontsize),
          axis.text = element_text(size = fontsize),
          axis.title = element_text(size = fontsize),
          plot.title = element_text(size = fontsize),
          legend.text = element_text(size = fontsize),
          legend.title = element_text(size = fontsize))
}
