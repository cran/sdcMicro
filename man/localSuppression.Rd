\name{localSuppression}
\alias{localSuppression}
\title{ Local Suppression to obtain k-anonymity }
\description{
Algorithm to achieve k-anonymity by performing local suppression.
}
\usage{
localSuppression(x, keyVars, w=NULL, k=2, importance=NULL)
}
\arguments{
  \item{x}{ data frame or matrix }
  \item{keyVars}{ numeric vector specifying indices of (categorical) key-variables }
  \item{w}{ index specifying weight-variable in input data x }
  \item{k}{ threshold for k-anonymity }
  \item{importance}{ numeric vector of numbers between 1 and n (n=length of vector keyVars). 
  This vector represents the "importance" of variables that should be used for local suppression
  in order to obtain k-anonymity. key-variables with importance=1 will - if possible - not suppressed,
  key-variables with importance=n will be used whenever possible. }
}
\details{
The algorithm provides a k-anonymized data set by suppressing values in key 
variables. The algorithm tries to find an optimal solution to suppress as few
values as possible and considers the specified importance vector.
If not specified, the importance vector is constructed in a way such that 
key variables with a high number of characteristics are considered less 
important than key variables with a low number of characteristics. 
}
\value{
Manipulated data set with suppressions that has k-anonymity 
with respect to specified key-variables
}

\author{ Bernhard Meindl, Matthias Templ}
\examples{
data(francdat)
## Local Suppression            
localS <- localSuppression(francdat, keyVar=c(4,5,6))
localS
plot(localS)
}
\keyword{ manip }
