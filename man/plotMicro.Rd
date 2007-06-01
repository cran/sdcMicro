\name{plotMicro}
\alias{plotMicro}
\title{ Comparison plots }
\description{
  Plots for the comparison of the original data and perturbed data.
}
\usage{
plotMicro(x, p, which.plot = 1:3)
}
\arguments{
  \item{x}{ object from class micro }
  \item{p}{ necessary parameter for the box cox transformation (lambda) }
  \item{which.plot}{ which plot should be created? 1: density traces, 2: parallel boxplots, 3: differences in totals }
}
\details{
Univariate and multivariate comparison plots are implemented to detect differences between the perturbed and the original data, but also 
to compare perturbed data which are produced by different methods.
}
\references{ see e.g. in \url{http://www.springerlink.com/content/4t0h5123v1436342/?p=878486a25d04466c981f7ad24f085447&pi=28}  }
\author{ Matthias Templ }
\seealso{  \code{\link{microaggregation}} }
\examples{
data(free1)
m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
m2 <- microaggregation(free1[, 31:34], method="pca", aggr=3)
plotMicro(m1, 0.1, which.plot=1)
}
\keyword{ aplot }
