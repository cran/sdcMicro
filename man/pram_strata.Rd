\name{pram_strata}
\alias{pram_strata}
\alias{print.pram_strata}
\title{
Post Randomization
}
\description{
To be used on categorical data. It randomly change the values of variables on selected records 
(normally the risky ones) according to an invariant probability transition matrix.
}
\usage{
pram_strata(data, variables = NULL, strata_variables = NULL, weights = NULL, seed = NULL, missing = -999)
\method{print}{pram_strata}(x, ...)
}
\arguments{
  \item{data}{
  Input data
}
  \item{variables}{
  Names of variables for Post Randomization
}
  \item{strata_variables}{
  Names of variables for stratification
}
  \item{weights}{
  a weight for each variable
}
  \item{seed}{
  Integer value for the random seed
}
  \item{missing}{
  A integer value to be used as missing value in the C++ routine
}
 \item{x}{
 Output of pram_strata
}
 \item{...}{
  currently unused
}
}
\value{
For each randomized variable a new variable with the suffix "_pram" will  be created.
}
\references{
\url{http://www.gnu.org/software/glpk}

\url{http://www.ccsr.ac.uk/sars/guide/2001/pram.pdf}
}
\author{
Alexander Kowarik, Bernd Prantner, IHSN C++ source
}
\note{
The GNU Linear Programming Kit(GLPK) is used in this program. The modified make files from the R package Rglpk are used.
}
\seealso{\code{pram}}
\examples{
\dontrun{
  data(testdata)
  res <- pram_strata(testdata,variables="roof",strata_variables=c("urbrur","sex"))
  print(res)
  res1 <- pram_strata(testdata,variables=c("roof","walls","water"),strata_variables=c("urbrur","sex"))
  print(res1)
}
}
\keyword{ manip }