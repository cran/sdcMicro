\name{LLmodGlobalRisk}
\docType{methods}
\alias{LLmodGlobalRisk-methods}
\alias{LLmodGlobalRisk,ANY-method}
\alias{LLmodGlobalRisk,data.frame-method}
\alias{LLmodGlobalRisk,matrix-method}
\alias{LLmodGlobalRisk,sdcMicroObj-method}
\alias{LLmodGlobalRisk}
\title{
Global risk using log-linear models.
}
\description{
The sample frequencies are assumed to be independent and following a 
Poisson distribution. The parameters of the corresponding parameters are
estimated by a log-linear model including the main effects and possible interactions.
}
\usage{
LLmodGlobalRisk(obj,...)
# LLmodGlobalRisk(x, method = "IPF", inclProb = NULL, form = as.formula(paste(" ~ ", paste(colnames(x), collapse = "+"))), modOutput = FALSE)
}
\arguments{
  \item{obj}{
An object of class sdcMicroObj or a numeric matrix or data frame containing the categorical key variables.
}
\item{...}{see possible arguments below}
  \item{method}{
At this time, only iterative proportional fitting  (\dQuote{IPF}) can be used.
}
  \item{x}{
data frame or matrix
}
  \item{inclProb}{
Inclusion probabilites (experimental)
}
\item{form}{
A formula specifying the model.
}
  \item{modOutput}{
if TRUE, additional output is given.
}
}
\details{
The iterative proportional fitting method is used to fit the parameters of 
the Poisson distribution related to the model specified. 
These parameters are used to estimate a global risk, defined in Skinner and Holmes (1998).
}
\value{
Two global risk measures or the modified risk in the \dQuote{sdcMicroObj} object.
}
\references{
CJ. Skinner and DJ. Holmes. Estimating the re-identification risk per record in microdata.
Journal of Official Statistics, 14:361-372, 1998.
}
\author{
Matthias Templ
}
\seealso{
\code{\link{loglm}}, \code{\link{measure_risk}}
}
\examples{
data(free1)
x <- data.frame(free1[,c(2,4:5)])
x["SEX"] <- as.factor(x[,"SEX"])
x["MARSTAT"] <- as.factor(x[,"MARSTAT"])
x["KINDPERS"] <- as.factor(x[,"KINDPERS"])
LLmodGlobalRisk(x, inclProb=1/mean(free1[,"WEIGHT"]))

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2, keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
           numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- LLmodGlobalRisk(sdc)

}
\keyword{ manip }
