% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcManagementNC.R
\name{calcManagementNC}
\alias{calcManagementNC}
\title{calcManagementNC}
\usage{
calcManagementNC(outputFormat, harmonizationPeriod, yearsSubset)
}
\arguments{
\item{outputFormat}{options: ESM, ScenarioMIP}

\item{harmonizationPeriod}{Two integer values, before the first given
year the target dataset is used, after the second given year the input
dataset is used, in between harmonize between the two datasets}

\item{yearsSubset}{remove years from the returned data which are not in yearsSubset}
}
\value{
data prepared to be written as a LUH-style management.nc file
}
\description{
Prepare data to be written as a LUH-style management.nc file.  Call this via calcOutput
in a full function, and set calcOutput's file argument to a .nc file path.
}
\author{
Pascal Sauer, Jan Philipp Dietrich
}
