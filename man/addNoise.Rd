\name{addNoise}
\alias{addNoise}
\title{ Adding noise for the perturbation of data }
\description{
  Various adding noise methods for the perturbation of continuous scaled variables can be used.
}
\usage{
addNoise(x, noise = 150, method = "additive", p = 0.001, delta=0.1)
}
\arguments{
  \item{x}{ data frame or matrix which should be perturbed }
  \item{noise}{ amount of noise (in percentages)}
  \item{method}{ choose between \sQuote{additive}, \sQuote{correlated}, \sQuote{correlated2}, \sQuote{restr}, \sQuote{ROMM}, \sQuote{outdect}  }
  \item{p}{ multiplication factor for method \sQuote{ROMM} }
  \item{delta}{ parameter for method \sQuote{correlated2}, details can be found
   in the reference below. }  
}
\details{
  Method \sQuote{additive} adds noise completely at random to each variable 
  depending on there size and standard deviation. \sQuote{correlated}
   and method \sQuote{correlated2} adds noise and preserves the covariances as descriped in 
   R. Brand (2001) or in the reference given below.
   Method \sQuote{restr} takes the 
   sample size into account when adding noise. 
   Method \sQuote{ROMM} is an implementation of the algorithm ROMM (Random Orthogonalized 
   Matrix Masking) (Fienberg, 2004).
   Method \sQuote{outdect} adds noise only to outliers. 
   The outliers are idedentified with univariate and robust multivariate procedures 
   based on a robust mahalanobis distancs calculated by the MCD estimator.
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
              \url{http://www.niss.org/dgii/TR/wp.11.e(ROMM).pdf} 
              
  Templ, M. and Meindl, B., 
               \emph{Robustification of Microdata Masking Methods and the Comparison 
               with Existing Methods}, 
               Lecture Notes in Computer Science, Privacy in Statistical Databases, 
               vol. 5262, pp. 177-189, 2008.  
               
  Templ, M. 
\emph{New Developments in Statistical Disclosure Control and Imputation:
Robust Statistics Applied to Official Statistics}, Suedwestdeutscher Verlag fuer Hochschulschriften, 
2009, ISBN: 3838108280, 264 pages. 
             
}
\author{ Matthias Templ }
\seealso{  \code{\link{summary.micro}} }
\examples{
data(Tarragona)
a1 <- addNoise(Tarragona)
a1
valTable(Tarragona[100:200,], method=c("addNoise: additive", "addNoise: correlated2"))
}
\keyword{ manip }
