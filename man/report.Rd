\name{report}
\docType{methods}
\alias{report-methods}
\alias{report}
\alias{report,sdcMicroObj-method}
\title{Generate a HTML output from an sdcMicroObj}
\description{
 Summary statistics of the original and the perturbed data set
}
\usage{
report(obj, outdir=getwd(),filename="SDC-Report",Title="SDC-Report",internal=TRUE)
}
\arguments{
\item{obj}{
  	an object of class 'sdcMicroObj'
  }

  \item{outdir}{
	output folder
  }
  \item{filename}{
   	output filename
  }
  \item{Title}{
   	Title for HTML
  }
  \item{internal}{
   	TRUE for the long internal version of the report. FALSE for a short version for externals.
  }
}
\details{
The application of this function provides you with a html report for your sdcMicro object. 
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "sdcMicroObj")}}{
%%  ~~describe this method here~~
}
}}
\author{
Matthias Templ
}
\examples{
\dontrun{
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
report(sdc)
}
}
\keyword{methods}
