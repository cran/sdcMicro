\name{plot.localSupp2}
\alias{plot.localSupp2}
\title{ plot method for localSupp2 objects }
\description{
 Special barplot for objects from class localSupp2.
}
\usage{
\method{plot}{localSupp2}(x, ...)
}
\arguments{
  \item{x}{ object of class \sQuote{localSupp2} }
  \item{\dots}{ }
}
\details{
  Just look at the resulting plot.
}
\author{ Matthias Templ }
\seealso{ \code{\link{localSupp2}}, \code{\link{localSupp2Wrapper}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
l1 <- localSupp2(francdat, keyVars=c(2,4,5,6), w=8)
l1
plot(l1)
}
\keyword{ aplot }
