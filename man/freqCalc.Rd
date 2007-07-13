\name{freqCalc}
\alias{freqCalc}
\title{ Frequencies calculation for risk estimation }
\description{
  Fast computation of frequencies (counts) with which is needed for risk estimation.
}
\usage{
freqCalc(x, keyVars = 1:3, w = 4)
}
\arguments{
  \item{x}{ data frame or matrix }
  \item{keyVars}{ key variables }
  \item{w}{ column index of the weight variable. Should be set to NULL if one deal with the whole population. }
}
\details{
The function considers the case of missing values in the data. A missing value stands for any of the possible categories 
of the variables considered.
}
\value{
  Object from class freqCalc.
  \item{freqCalc}{data}
  \item{keyVars }{keyVars}
  \item{w}{index of weight vector. NULL if you do not a sample.}
    \item{indexG }{}
    \item{fk }{the frequency of equal observations in the key variables subset sample given for each observation.}
    \item{Fk}{estimated frequency in the population}
    \item{n1 }{amount of observations with fk=1}
    \item{n2 }{amount of observations with fk=2}    
}
\references{ look e.g. in \url{http://neon.vb.cbs.nl/casc/Deliv/12d1.pdf} }
\author{ Bernhard Meindl and Matthias Templ }
\seealso{ \code{\link{indivRisk}} }
\examples{
data(francdat)
f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
f
f$freqCalc
f$fk
f$Fk
## with missings:
x <- francdat
x[3,5] <- NA
x[4,2] <- x[4,4] <- NA
x[5,6]  <- NA
x[6,2]  <- NA
f2 <- freqCalc(x,  keyVars=c(2,4,5,6),w=8)
f2$Fk
}
\keyword{ manip }