% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createDat.R
\name{createDat}
\alias{createDat}
\title{Dummy Dataset for Record Swapping}
\usage{
createDat(N = 10000)
}
\arguments{
\item{N}{integer, number of household to generate}
}
\value{
`data.table` containing dummy data
}
\description{
createDat() returns dummy data to illustrate
targeted record swapping. The generated data contain
household ids (`hid`), geographic variables
(`nuts1`, `nuts2`, `nuts3`, `lau2`) as well as some
other household or personal variables.
}
\seealso{
recordSwap
}
