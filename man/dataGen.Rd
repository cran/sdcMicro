\name{dataGen}
\alias{dataGen}
\title{ Fast generation of synthetic data }
\description{
  Fast generation of synthetic data.
}
\usage{
dataGen(x, n = 200)
}
\arguments{
  \item{x}{ data.frame or matix }
  \item{n}{ amount of observations for the generated data }
}
\details{
Uses the cholesky decomposition to generate synthetic data. For details see at the reference.
}
\value{
 the generated synthetic data.
}
\references{ Have a look at \url{http://vneumann.etse.urv.es/publications/sci/lncs3050FastGen.pdf} }
\author{ Matthias Templ }
\note{ With this method only multivariate normal distributed data with approxiomately the same covariance as the original data can be generated
without reflecting the distribution of real complex data, which are, in general, not follows a multivariate normal distribution.
}
\examples{
data(mtcars)
cov(mtcars[,4:6])
cov(dataGen(mtcars[,4:6]))
pairs(mtcars[,4:6])
pairs(dataGen(mtcars[,4:6]))
}
\keyword{ manip }

