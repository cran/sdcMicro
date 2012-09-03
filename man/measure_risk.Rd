\name{measure_risk}
\alias{measure_risk}
\alias{ldiversity}
\alias{print.measure_risk}
\alias{print.ldiversity}
%\alias{measure_hier}
%\alias{print.measure_hier}
%\alias{measure_thres}
%\alias{print.measure_thres}
\title{
Measure Risk for weighted or unweighted data / Individual risk to household risk / Compute a risk threshold based on a global risk value
}
\description{
measure risk like in mu-argus / To be used when risk of disclosure for individuals within a family is considered to be statistical independent.
This is the assumption made in mu-Argus risk model. / To be used when the risk measurement at individual level can be used to calculate the file
level risk by taking the average of the individual risk. 
}
\usage{
measure_risk(data,keyVars,w=NULL,missing=-999,hid=NULL,max_global_risk=.01,force_hier=FALSE)
ldiversity(data,keyVars,missing=-999,l_recurs_c=2,ldiv_index=NULL)
\method{print}{measure_risk}(x, ...)
\method{print}{ldiversity}(x, ...)
%measure_hier(data,variables,weight_variable=NULL,hid,missing=-999)
%\method{print}{measure_hier}(x, ...)
%measure_thres(data,variables,weight_variable=NULL,missing=-999)
%\method{print}{measure_thres}(x, ...)
}
\arguments{
  \item{data}{
  Input data
}
  \item{keyVars}{
  Names of key variables
}
  \item{w}{
  name of variable containing sample weights
}
 \item{hid}{
 Household ID
}
  \item{missing}{
  A integer value to be used as missing value in the C++ routine
}
 \item{l_recurs_c}{
 L-Diversity Constant
}
  \item{ldiv_index}{
  indices (or names) of the variables used for l-diversity
  }
   \item{x}{
   Output of measure_risk, measure_hier or measure_thres
  }
  \item{max_global_risk}{Maximal global risk for threshold computation}
  \item{force_hier}{If TRUE a computation of the hierachical risk will be performed for large households, which is very slow.}
 \item{...}{
  currently unused
}
}
\value{
list with the following elements:
global_risk_ER: expected number of re-identification
global_risk: global risk (sum of indivdual risks)
global_risk_pct: global risk in percent
Res: matrix with the group counts, the risk, the group size (and the hierachical risk) for each observation
global_threshold: for a given max_global_risk the threshold for the risk of observations
max_global_risk: the input max_global_risk of the function
hier_risk_ER: expected number of re-identification with household structure
hier_risk: global risk with household structure(sum of indivdual risks)
hier_risk_pct: global risk  with household structure in percent
ldiverstiy: Matrix with Distinct_Ldiversity, Entropy_Ldiversity andRecursive_Ldiversity for each sensitivity variable

}
\references{
\url{http://neon.vb.cbs.nl/casc/Software/MuManual4.1.pdf}
}
\author{
Alexander Kowarik, Bernd Prantner, IHSN C++ source
}
\examples{
  # measure_risk
  data(testdata)
  res <- measure_risk(testdata,c("urbrur","roof","walls","water","sex"))
  print(res)
  head(res$Res)
  resw <- measure_risk(testdata,c("urbrur","roof","walls","water","sex"),w="sampling_weight")
  print(resw)
  head(resw$Res)
  res1 <- ldiversity(testdata,c("urbrur","roof","walls","water","sex"),ldiv_index="electcon")
  print(res1)
  head(res1)
  res2 <- ldiversity(testdata,c("urbrur","roof","walls","water","sex"),ldiv_index=c("electcon","relat"))
  print(res2)
  head(res2)
  # measure risk with household risk
  resh <- measure_risk(testdata,c("urbrur","roof","walls","water","sex"),w="sampling_weight",hid="ori_hid")
  print(resh)
  # change max_global_risk
  rest <- measure_risk(testdata,c("urbrur","roof","walls","water","sex"),w="sampling_weight",max_global_risk=0.0001)
  print(rest)
}
\keyword{ manip }