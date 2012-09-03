\name{LocalRecProg}
\alias{LocalRecProg}
\title{
Local recoding via Edmond's maximum weighted matching algorithm
}
\description{
To be used on both categorical and numeric input variables, although usage on categorical variables is the focus of the
development of this software.

Each record in the data represents a category of the original data, and hence all records in the input
data should be unique by the N Input Variables. To achieve bigger category sizes (k-anoymity), one can
form new categories based on the recoding result and repeatedly apply this algorithm. 
}
\usage{
LocalRecProg(data,categorical,numerical=NULL,ancestors=NULL,ancestor_setting=NULL,
    k_level=2, FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA)
}
\arguments{
  \item{data}{
  Input data
}
  \item{categorical}{
  Names of categorical variables
}
  \item{numerical}{
  Names of numerical variables
}
 \item{ancestors}{
  Names of ancestors of the cateoorical variables
}
  \item{ancestor_setting}{
  For each ancestor the corresponding categorical variable
}
 \item{k_level}{
 Level for k-anonymity
}
  \item{FindLowestK}{
  requests the program to look for the smallest k that results in complete matches of the data.
  }
   \item{weight}{
   A weight for each variable (Default=1)
  }
 \item{lowMemory}{
  Slower algorithm with less memory consumption
}
 \item{missingValue}{
  The output value for a suppressed value.
}
}
\value{
dataframe with original variables and the supressed variables (suffix _lr).
}
\references{
\url{http://www.stat.t.u-tokyo.ac.jp/~takemura/papers/localrec.pdf}
}
\author{
Alexander Kowarik, Bernd Prantner, IHSN C++ source, Akimichi Takemura
}
\examples{
  # LocalRecProg
  data(testdata2)
  r1=LocalRecProg(testdata2,categorical=c("urbrur", "roof", "walls", "water", "sex", "relat"),missingValue=-99)
  r2=LocalRecProg(testdata2,categorical=c("urbrur", "roof", "walls", "water", "sex", "relat"),
    ancestor=c("water2", "water3", "relat2"),ancestor_setting=c("water","water","relat"),missingValue=-99)
  r3=LocalRecProg(testdata2,categorical=c("urbrur", "roof", "walls", "water", "sex", "relat"),
    ancestor=c("water2", "water3", "relat2"),ancestor_setting=c("water","water","relat"),missingValue=-99,FindLowestK=FALSE)

}
\keyword{ manip }