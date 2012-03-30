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
Subset-mean preservation factor. Preserves the means before and after rank swapping within a range based on K0.
}
  \item{R0}{
Multivariate preservation factor. Preserves the correlation between variables within a certain range based on
the given constant R0.
}
  \item{P}{
Rank range as percentage of total sample size.
}
  \item{missing}{
missig value code.
}
  \item{seed}{
Seed.
}
}
\details{
Rank swapping sorts the values of one numeric variable by their
numerical values (ranking).  
The restricted range is determined by the rank of two swapped values, which
cannot differ, by definition, by more than p percent of the total number of
observations.


}
\value{
The rank-swapped data set.
}
\references{
Moore, Jr.R. (1996) 
    Controlled data-swapping techniques for masking public use microdata,
    U.S. Bureau of the Census
    \emph{Statistical Research Division Report Series},
    RR 96-04 .
}
\author{Alexander Kowarik for the interface.

For the underlying C++ code: This work is being supported by the International Household 
Survey Network (IHSN) and funded by a DGF Grant provided by the World Bank to the 
PARIS21 Secretariat at the Organisation for Economic Co-operation and Development (OECD). 
This work builds on previous work which is elsewhere acknowledged. 
}

\examples{
data(testdata)
data_swap <- rankSwap(testdata,variables=c("age","income","expend","savings"))
}
