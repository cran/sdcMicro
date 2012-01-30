\name{suda2}
\alias{suda2}
\title{
Suda2: Detecting Special Uniques
}
\description{
SUDA risk measure for data from (stratified) simple random sampling.
}
\usage{
suda2(data,variables=NULL,missing=-999,DisFraction=0.01)
}
\arguments{
  \item{data}{
object of class \dQuote{data.frame}
}
  \item{variables}{
Categorical (key) variables. Either the column names or and index of the variables to be used for
risk measurement.
}
  \item{missing}{
Missing value coding in the given data set.
}
  \item{DisFraction}{
It is the sampling fraction for the simple random sampling, and the common 
sampling fraction for stratified sampling. By default, it's set to 0.01.
}
}
\details{
Suda 2 is a recursive algorithm for finding
Minimal Sample Uniques. The algorithm generates all possible
variable subsets of defined categorical key variables 
and scans them for unique patterns in the subsets of variables. The lower the
amount of variables needed to receive uniqueness, the higher the risk of the
corresponding observation.
}
\value{
\item{ContributionPercent}{The contribution of each key variable to the SUDA score, calculated for each row.}
\item{score }{The suda score.}
\item{disscore }{The dis suda score}
}
\references{
C. J. Skinner; M. J. Elliot (20xx)
A Measure of Disclosure Risk for Microdata. \emph{Journal of the Royal 
Statistical Society: Series B (Statistical Methodology)}, Vol. 64 (4), pp 855--867.
 
M. J. Elliot, A. Manning, K. Mayes, J. Gurd and M. Bane (20xx)
SUDA: A Program for Detecting Special Uniques, 
Using DIS to Modify the Classification of Special Uniques

Anna M. Manning, David J. Haglin, John A. Keane (2008)
A recursive search algorithm for statistical disclosure assessment.
\emph{Data Min Knowl Disc}  16:165 -- 196 
}
\author{
Alexander Kowarik based on the C++  code from the 
Organisation For Economic Co-Operation And Development.  
}
\examples{
data(testdata)
data_suda2 <- suda2(testdata,variables=c("urbrur","roof","walls","water","sex"))
data_suda2
summary(data_suda2)
}
\keyword{ manip }
