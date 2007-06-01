\name{topBotCoding}
\alias{topBotCoding}
\title{ Top and Bottom Coding }
\description{
Function for Top and Bottom Coding.
}
\usage{
topBotCoding(x, value, replacement, kind = "top")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vector or one-dimensional matrix or data.frame }
  \item{value}{ limit, from where it should be top- or bottom-coded }
  \item{replacement}{ replacement value. }
  \item{kind}{ top or bottom }
}
\details{
Extreme values are replaced by one value to reduce the disclosure risk.
}
\value{
Top or bottom coded data.
}
\author{ Matthias Templ }
\seealso{ \code{\link{indivRisk}} }
\examples{
data(free1)
topBotCoding(free1[,"DEBTS"], value=9000, replacement=9100, kind="top")
}
\keyword{ manip }

