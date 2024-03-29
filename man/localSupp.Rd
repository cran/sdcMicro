% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/localSupp.R
\docType{methods}
\name{localSupp}
\alias{localSupp}
\title{Local Suppression}
\usage{
localSupp(obj, threshold = 0.15, keyVar)
}
\arguments{
\item{obj}{object of class \code{\link{freqCalc}} or \code{\link{sdcMicroObj-class}}.}

\item{threshold}{threshold for individual risk}

\item{keyVar}{Variable on which some values might be suppressed}
}
\value{
an updated object of class \code{\link{freqCalc}} or the \code{\link{sdcMicroObj-class}}
object with manipulated data.
}
\description{
A simple method to perfom local suppression.
}
\details{
Values of high risk (above the threshold) of a certain variable (parameter
keyVar) are suppressed.
}
\examples{
data(francdat)
keyVars <- paste0("Key",1:4)
\donttest{
f <- freqCalc(francdat, keyVars = keyVars, w = 8)
f
f$fk
f$Fk

## individual risk calculation:
indivf <- indivRisk(f)
indivf$rk

## Local Suppression
localS <- localSupp(f, keyVar = "Key4", threshold = 0.15)
f2 <- freqCalc(localS$freqCalc, keyVars = keyVars, w = 8)
indivf2 <- indivRisk(f2)
indivf2$rk
identical(indivf$rk, indivf2$rk)

## select another keyVar and run localSupp once again,
# if you think the table is not fully protected

## for objects of class sdcMicro:
data(testdata)
sdc <- createSdcObj(
  dat = testdata,
  keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
  w = "sampling_weight"
)
sdc <- localSupp(sdc, keyVar = "urbrur", threshold = 0.045)
print(sdc, type = "ls")
}
}
\references{
Templ, M. \emph{Statistical Disclosure Control for Microdata
Using the R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number
2, pp. 67-85, 2008. \url{http://www.tdp.cat/issues/abs.a004a08.php}

Templ, M. Statistical Disclosure Control for Microdata: Methods and Applications in R.
\emph{Springer International Publishing}, 287 pages, 2017. ISBN 978-3-319-50272-4. \doi{10.1007/978-3-319-50272-4}
\doi{10.1007/978-3-319-50272-4}
}
\seealso{
\code{\link{freqCalc}}, \code{\link{indivRisk}}
}
\author{
Matthias Templ and Bernhard Meindl
}
\keyword{manip}
