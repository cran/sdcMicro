\name{rankSwap}
\alias{rankSwap}
\title{
Rank Swapping
}
\description{
Each ranked value is then swapped with another
ranked value that has been chosen randomly within a restricted range.
}
\usage{
rankSwap(data, variables, TopPercent = 5, BottomPercent = 5, K0 = -1, R0 = 0.95, P = 0, missing = -999, seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
matrix or data frame
}
  \item{variables}{
names or index of variables for that rank swapping is applied.
}
  \item{TopPercent}{
Percentage of largest values that are group together before rank swapping is applied.
}
  \item{BottomPercent}{
Percentage of lowest values that are group together before rank swapping is applied.
}
  \item{K0}{
Subset-mean preservation factor.
}
  \item{R0}{
%%     ~~Describe \code{R0} here~~
}
  \item{P}{
%%     ~~Describe \code{P} here~~
}
  \item{missing}{
%%     ~~Describe \code{missing} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
}
\details{
Rank swapping sorts the values of one numeric variable by their
numerical values (ranking).  
The restricted range is determined by the rank of two swapped values, which
cannot differ, by definition, by more than $p$ percent of the total number of
observations.


}
\value{
v
}
\references{
r
}
\author{
a
}

\examples{
data(testdata)
data_swap <- rankSwap(testdata,variables=c("age","income","expend","savings"))
}
