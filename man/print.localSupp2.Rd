\name{print.localSupp2}
\alias{print.localSupp2}
\title{ Print method for objects from class localSupp2 }
\description{
Print method for objects from class localSupp2.
}
\usage{
\method{print}{localSupp2}(x, ...)
}
\arguments{
  \item{x}{ object from class localSupp2 }
  \item{\dots}{ Additional arguments passed through. }
}
\value{
information about the frequency counts for key variables for object of class \sQuote{localSupp2}.
}
\author{ Matthias Templ }
\seealso{ \code{\link{localSupp2}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
l1 <- localSupp2(francdat, keyVars=c(2,4,5,6), w=8)
l1
}
\keyword{ print }

