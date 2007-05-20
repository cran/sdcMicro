\name{freqCalc}
\alias{freqCalc}
\title{ Frequencies calculation for risk estimation }
\description{
  Computation of frequencies for risk estimation
}
\usage{
freqCalc(x, keyVars = 1:3, w = 4)
}
\arguments{
  \item{x}{ data frame or matrix }
  \item{keyVars}{ key variables }
  \item{w}{ column index of the weight variable }
}
\details{
}
\value{
  Object from class freqCalc.
  \item{freqCalc}{data}
  \item{keyVars }{keyVars}
  \item{w}{index of weight vector}
    \item{indexG }{}
    \item{fk }{fk}
    \item{Fk}{Fk}
    \item{n1 }{}
    \item{n2 }{}    
}
\references{ \url{http://neon.vb.cbs.nl/casc/Deliv/12d1.pdf} }
\author{ Matthias Templ }
\note{ 
}
\seealso{ \code{\link{indivRisk}} }
\examples{
data(francdat)
f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
f
f$fk
f$Fk
}
\keyword{ manip }