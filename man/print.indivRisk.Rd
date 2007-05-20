\name{print.indivRisk}
\alias{print.indivRisk}
\title{ Print method for objects from class indivRisk }
\description{
 Print method for objects from class indivRisk
}
\usage{
print.indivRisk(x, ...)
}
\arguments{
  \item{x}{ object from class indivRisk }
  \item{\dots}{ }
}
\details{
}
\value{
}
\references{ }
\author{ Matthias Templ }
\note{ 
}
\seealso{ \code{\link{indivRisk}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
f
f$fk
f$Fk
## individual risk calculation:
indivRisk(f) 
}
\keyword{ print }

