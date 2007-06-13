\name{plot.indivRisk}
\alias{plot.indivRisk}
\title{ plot method for indivRisk objects }
\description{
 Plots an interactive histogramm or ecdf plot with various interactive sliders.
}
\usage{
\method{plot}{indivRisk}(x, ...)
}
\arguments{
  \item{x}{ object of class \sQuote{indivRisk} }
  \item{\dots}{ }
}
\details{
  With the sliders one can move the individual risk threshold. By this movement the threshold 
  will be moved on the plot and the slider with a re-idendification rate and the slider of the number of unsafe records (based on your
  chosen threshold) are also moved based on the individual risk threshold. This plot is very similar to the individual risk plot of
  the software mu-Argus.
}
\references{ look e.g. on the mu-Argus manuals available at \url{http://neon.vb.cbs.nl/casc/Software/MuManual4.1.pdf}}
\author{ Matthias Templ }
\seealso{ \code{\link{indivRisk}} }
\examples{
data(free1)
f <- freqCalc(free1, keyVars=1:3,w=30)
## individual risk calculation:
indivf <- indivRisk(f)  
## now use:
##plot(indivf)  
}
\keyword{ aplot }
