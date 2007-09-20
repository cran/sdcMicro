\name{summary.pram}
\alias{summary.pram}
\title{ Summary method for objects from class pram }
\description{
Summary method for objects from class \sQuote{pram} to provide information about transitions.
}
\usage{
\method{summary}{pram}(object, ...)
}
\arguments{
  \item{object}{object from class \sQuote{pram} }
  \item{\dots}{  }
}
\details{
  Shows various information about the transitions.
}
\value{
  The summary of object from class \sQuote{pram}.
}
\author{Matthias Templ }
\seealso{ \code{\link{pram}} }
\examples{
data(free1)
x <- free1[,"MARSTAT"]
x2 <- pram(x)
x2
summary(x2)
}
\keyword{ print }

