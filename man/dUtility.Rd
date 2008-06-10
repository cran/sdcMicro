\name{dUtility}
\alias{dUtility}
\title{ data utility }
\description{
  IL1s data utility
}
\usage{
dUtility(x, xm, method="IL1")
}
\arguments{
  \item{x}{ original data }
  \item{xm}{ perturbed data }
  \item{method}{ method IL1 or eigen. More methods are implemented in summary.micro() }
}
\details{
Here we measure standardised distances of the perturbed data values to the original ones. Measure IL1 measures the distances between 
the original values and the perturbed ones, scaled by the standard deviation. Method \sQuote{eigen} and \sQuote{robeigen} compares the 
eigenvalues and robust eigenvalues form the original data and the perturbed data.
}
\value{
data utility
}
\references{ look e.g. in \url{http://vneumann.etse.urv.es/publications/sci/lncs3050Outlier.pdf} }
\author{ Matthias Templ }
\seealso{  \code{\link{dRisk}}, \code{\link{dRiskRMD}} }
\examples{
data(free1)
m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
m2 <- microaggregation(free1[, 31:34], method="pca", aggr=3)
dRisk(x=free1[, 31:34], xm=m1$blowxm)
dRisk(x=free1[, 31:34], xm=m2$blowxm)
dUtility(x=free1[, 31:34], xm=m1$blowxm)
dUtility(x=free1[, 31:34], xm=m2$blowxm)
data(Tarragona)
x <- Tarragona[, 5:7]
y <- addNoise(x)$xm
dRiskRMD(x, xm=y)
dRisk(x, xm=y)
dUtility(x, xm=y)
dUtility(x, xm=y, method="eigen")
dUtility(x, xm=y, method="robeigen")
}
\keyword{ manip }