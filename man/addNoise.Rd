\name{addNoise}
\alias{addNoise}
\title{ Adding noise for the perturbation of data }
\description{
  Various adding noise methods for the perturbation of data can be used.
}
\usage{
addNoise(x, noise = 150, method = "additive", p = 0.001, delta=0.1)
}
\arguments{
  \item{x}{ data frame or matrix which should be perturbed }
  \item{noise}{ amount of noise }
  \item{method}{ choose between \sQuote{additive}, \sQuote{correlated}, \sQuote{correlated2}, \sQuote{restr}, \sQuote{ROMM}, \sQuote{outdect}  }
  \item{p}{ multiplication factor for method \sQuote{ROMM} }
  \item{delta}{ parmeter for method \sQuote{correlated2}, see in the reference below }  
}
\details{
  Method \sQuote{additive} adds noise completely random to each variable depending on there size and standard error. \sQuote{correlated}
   and method \sQuote{correlated2} adds noise and preserves the covariances as descriped in R. Brand (2001) or in the reference given below.
   \sQuote{restr} takes the 
   sample size into account when adding noise. Method \sQuote{ROMM} should be the implementation of the first algorithm of Fienberg (2004).
   Method \sQuote{outdect} adds noise only to outliers. This outliers are idedentified with univariate and robust multivariate procedures 
   based on a robust mahalanobis distancs calculated with the MCD estimator.
}
\value{
  An object of class \dQuote{micro} with following entities:
  \item{x }{ the original data }
  \item{xm }{ the modified (perturbed) data }
  \item{method }{method used for perturbation}
  \item{noise }{ amount of noise}
}
\references{  Domingo-Ferrer, J. and Sebe, F. and Castella, J., 
               \dQuote{On the security of noise aliition for privacy in statistical databases}, 
               Lecture Notes in Computer Science, vol. 3050, pp. 149-161, 2004. 
               ISSN 0302-9743. Vol. Privacy in Statistical Databases, 
               eds. J. Domingo-Ferrer and V. Torra, Berlin: Springer-Verlag. 
               \url{http://vneumann.etse.urv.es/publications/sci/lncs3050OntheSec.pdf},
               
              Ting and Fienberg, \dQuote{ROMM Methodology for Microdata Release} 
              Joint UNECE/Eurostat work session on statistical data confidentiality, Geneva, Switzerland, 2005, 
              \url{http://www.niss.org/dgii/TR/wp.11.e(ROMM).pdf} }
\author{ Matthias Templ }
\seealso{  \code{\link{summary.micro}} }
\examples{
data(Tarragona)
a1 <- addNoise(Tarragona)
a1
valTable(Tarragona[100:200,], method=c("addNoise: additive", "addNoise: correlated2"))
}
\keyword{ manip }
