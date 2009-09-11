\name{dUtility}
\alias{dUtility}
\title{ data utility }
\description{
  IL1s data utility.
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
The standardised distances of the perturbed data values to the original ones are measured. 
Measure IL1 measures the distances between 
the original values and the perturbed ones, scaled by the standard deviation. 
Method \sQuote{eigen} and \sQuote{robeigen} compares the 
eigenvalues and robust eigenvalues form the original data and the perturbed data.
}
\value{
data utility
}
\references{for IL1s: see \url{http://vneumann.etse.urv.es/publications/sci/lncs3050Outlier.pdf},
 
 Templ, M. and Meindl, B., 
               \emph{Robust Statistics Meets {SDC}: New Disclosure Risk Measures for 
               Continuous Microdata Masking}, 
               Lecture Notes in Computer Science, Privacy in Statistical Databases, 
               vol. 5262, pp. 113-126, 2008.          
}
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