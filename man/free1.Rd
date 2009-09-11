\name{free1}
\alias{free1}
\docType{data}
\title{ Demo data set from mu-Argus}
\description{
Demo data set from the mu-Argus software for SDC. 
}
\usage{data(free1)}
\format{
  The format is:
 num [1:4000, 1:34] 36 36 36 36 36 36 36 36 36 36 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:34] "REGION" "SEX" "AGE" "MARSTAT" ...
}
\details{
Please, see at the link given below. Please note, that the correlation structure of the data
is not realistic.
}
\source{
\url{http://neon.vb.cbs.nl/casc/}
}
\examples{
data(free1)
head(free1)
}
\keyword{datasets}
