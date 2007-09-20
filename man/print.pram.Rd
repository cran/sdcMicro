\name{print.pram}
\alias{print.pram}
\title{ Print method for objects from class pram }
\description{
Print method for objects from class \sQuote{pram}.
}
\usage{
\method{print}{pram}(x, ...)
}
\arguments{
  \item{x}{ object from class \sQuote{pram} }
  \item{\dots}{  }
}
\value{
Short information about the method and the parameters used.
}
\author{ Matthias Templ }
\seealso{ \code{\link{pram}} }
\examples{
data(free1)
x <- free1[,"MARSTAT"]
x2 <- pram(x)
x2
}
\keyword{ print }

