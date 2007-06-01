\name{globalRecode}
\alias{globalRecode}
\title{ Global Recoding }
\description{
Global recoding
}
\usage{
globalRecode(x, breaks, labels)
}
\arguments{
  \item{x}{ vector for recoding }
  \item{breaks}{ either a numeric vector of cut points or number giving the number of intervals which x is to be cut into. }
  \item{labels}{ labels for the levels of the resulting category. By default, labels are constructed using "(a,b]" interval notation. 
                  If labels = FALSE, simple integer codes are returned instead of a factor. }
}
\details{
If a labels parameter is specified, its values are used to name the factor levels. 
If none is specified, the factor level labels are constructed. 
}
\value{
A factor is returned, unless labels = FALSE which results in the mere integer level codes.
}
\seealso{  \code{\link{cut}} }
\examples{
data(free1)
free1[, "AGE"] <- globalRecode(free1[,"AGE"], breaks=c(1,9,19,29,39,49,59,69,100), labels=1:8)
}
\keyword{ manip }
