\name{localSupp2Wrapper}
\alias{localSupp2Wrapper}
\title{ Local Suppression 2 }
\description{
A wrapper function for function localSupp2 in order to guarantee k-anonymity.
}
\usage{
localSupp2Wrapper(x, keyVars, w, importance=rep(1, length(keyVars)), method="minimizeSupp", kAnon=2)
}
\arguments{
  \item{x}{ data frame or matrix }
  \item{keyVars}{ column index of key variables }
  \item{w}{ column index of sampling weights }
  \item{importance}{ weights for each key variable, see \sQuote{localSupp2()} }
  \item{method}{ \dQuote{minimizeSupp} (default), further methods will be included in future versions of the package }
  \item{kAnon}{ parameter for k-anonymity. }
}
\details{
This wrapper function guarantees k-anonymity. If function localSupp2() cannot be reach k-anonymity, localSupp2 must
be re-run on the previous results as long as k-anoymity is reached. If k-anoymity cannot be achieved (because the entries of parameter
importance includes too much zeros) the function breaks after a sub-optimal solution is obtained.
}
\value{
  Object from class localSupp2.
  \item{xAnon}{resulting data with suppressions}
  \item{supps }{number of suppressions in the key variables }
  \item{totalSupps}{total number of suppressions.}
    \item{anonymity }{ TRUE, if k-anonymity is achieved }
    \item{keyVars }{index of the key variables.}
    \item{importance}{weight vector for key variables}
    \item{kAnon }{k for k-anoymity}
}
\references{ 
Templ, M.  
               \emph{Statistical Disclosure Control for Microdata Using the R-Package sdcMicro}, 
               Transactions on Data Privacy, 
               vol. 1, number 2, pp. 67-85, 2008. 
  \url{http://www.tdp.cat/issues/abs.a004a08.php}
 }
\author{ Bernhard Meindl, Matthias Templ }
\note{ fix me: Implementation in C and interface to R.
}
\seealso{  \code{\link{freqCalc}}, \code{\link{localSupp}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
## same results as localSupp2
data(francdat)
localSupp2Wrapper(francdat, keyVars=c(2,4,5,6), w=8)
localSupp2Wrapper(francdat, keyVars=c(2,4,5,6), w=8, k=2)
localSupp2Wrapper(francdat, keyVars=c(2,4,5,6), w=8, k=4)

## we want to avoid missings in column 5:
l1 <- localSupp2Wrapper(francdat, keyVars=c(2,4,5,6), importance=c(1,1,0,1), w=8, kAnon=1)
l1$x
## we want to avoid missings in column 5 and allow missings in 1 only if
## is really necessary:
l1 <- localSupp2Wrapper(francdat, keyVars=c(2,4,5,6), importance=c(0.1,1,0,1), w=8, kAnon=1)
l1$x

## long computation time, wait some seconds to get an information
## about the estimated computing time.
## l = localSupp2(free1, keyVars=1:3, w=30, k=2, importance=c(0.1,1,0.8))
}
\keyword{ manip }
