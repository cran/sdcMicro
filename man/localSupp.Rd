\name{localSupp}
\alias{localSupp}
\title{ Local Suppression }
\description{
A simple method to perfom local suppression.
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
Values of high risk (above the threshold) of a certain variable (parameter keyVar) 
are suppressed.
}
\value{
Manipulated data with suppressions
}
\references{ 
Templ, M.  
               \emph{Statistical Disclosure Control for Microdata Using the R-Package sdcMicro}, 
               Transactions on Data Privacy, 
               vol. 1, number 2, pp. 67-85, 2008. 
  url = {http://www.tdp.cat/issues/abs.a004a08.php}
 }
\author{ Matthias Templ }
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
