\name{dRisk}
\alias{dRisk}
\title{ overal disclosure risk }
\description{
  Distance-based disclosure risk estimation via standard deviation-based intervals.
}
\usage{
dRisk(x, xm, k = 0.01)
}
\arguments{
  \item{x}{ original data  }
  \item{xm}{ perturbed data }
  \item{k}{ percentage of the standard deviation }
}
\details{
An interval is built around each value of the perturbed value with the help of the standard deviation. 
Then we look if the original values are in these intervals or not.
With parameter k one can enlarge or down scaling the interval.
}
\value{
The disclosure risk.
}
\references{ see method SDID in \url{http://vneumann.etse.urv.es/publications/sci/lncs3050Outlier.pdf} }
\author{Matthias Templ }
\seealso{ \code{\link{dUtility}}, \code{\link{dUtility}} }
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
