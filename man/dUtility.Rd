\name{dUtility}
\alias{dUtility}
\title{ data utility }
\description{
  IL1s data utility
}
\usage{
dUtility(x, xm)
}
\arguments{
  \item{x}{ original data }
  \item{xm}{ perturbed data }
}
\details{
Here we measure standardised distances of the perturbed data values to the original ones.
}
\value{
data utility
}
\references{ look e.g. in \url{http://vneumann.etse.urv.es/publications/sci/lncs3050Outlier.pdf} }
\author{ Matthias Templ }
\note{ 
Note, that this measure of information loss does not evaluate how well univariate or multivariate statistics are preserved.
}
\seealso{  \code{\link{dRisk}} }
\examples{
 data(free1)
m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
m2 <- microaggregation(free1[, 31:34], method="pca", aggr=3)
dRisk(x=free1[, 31:34], xm=m1$blowxm)
dRisk(x=free1[, 31:34], xm=m2$blowxm)
dUtility(x=free1[, 31:34], xm=m1$blowxm)
dUtility(x=free1[, 31:34], xm=m2$blowxm)
}
\keyword{ manip }