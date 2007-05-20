\name{localSupp}
\alias{localSupp}
\title{ Local Suppression }
\description{
A simple algorithm to perfom local suppression.
}
\usage{
localSupp(x, keyVar, indivRisk, threshold = 0.15)
}
\arguments{
  \item{x}{ object from class freqCalc }
  \item{keyVar}{ Variable on which some values might be suppressed }
  \item{indivRisk}{ object from class indivRisk }
  \item{threshold}{ threshold for individual risk }
}
\details{
Have a look at the link given below.
}
\value{
Manipulated data
}
\references{  }
\author{ Matthias Templ }
\note{ fix me: Implementation of a advanced algorithm for local suppression.
}
\seealso{  \code{\link{freqCalc}}, \code{\link{indivRisk}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
f
f$fk
f$Fk
## individual risk calculation:
indivf <- indivRisk(f)  
indivf$rk         
## Local Suppression            
localS <- localSupp(f, keyVar=2, indivRisk=indivf$rk, threshold=0.25)
f2 <- freqCalc(localS$freqCalc, keyVars=c(4,5,6), w=8)
indivf2 <- indivRisk(f2)
indivf2$rk
## select another keyVar and run localSupp once again, if you think the table is not fully protected
}
\keyword{ manip }
