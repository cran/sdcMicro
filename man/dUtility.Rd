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
}
\value{
data utility
}
\references{ \url{http://vneumann.etse.urv.es/publications/sci/lncs3050Outlier.pdf} }
\author{ Matthias Templ }
\note{ 
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