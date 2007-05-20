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
  \item{breaks}{ breaks }
  \item{labels}{ labels }
}
\details{
}
\value{
 recoded vector
}
\references{  }
\author{ Matthias Templ }
\note{ 
}
\seealso{  \code{\link{cut}} }
\examples{
data(free1)
free1[, "AGE"] <- globalRecode(free1[,"AGE"], breaks=c(1,9,19,29,39,49,59,69,100), labels=1:8)
}
\keyword{ manip }
