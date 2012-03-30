\name{measure_risk}
\alias{measure_risk}
\alias{print.measure_risk}
\alias{measure_hier}
\alias{print.measure_hier}
\alias{measure_thres}
\alias{print.measure_thres}
\title{
Measure Risk for weighted or unweighted data / Individual risk to household risk / Compute a risk threshold based on a global risk value
}
\description{
measure risk like in mu-argus / To be used when risk of disclosure for individuals within a family is considered to be statistical independent.
This is the assumption made in mu-Argus risk model. / To be used when the risk measurement at individual level can be used to calculate the file
level risk by taking the average of the individual risk. 
}
\usage{
measure_risk(data,variables,weight_variable=NULL,missing=-999,l_recurs_c=2,ldiv_index=NULL)
\method{print}{measure_risk}(x, ...)
measure_hier(data,variables,weight_variable=NULL,hid,missing=-999)
\method{print}{measure_hier}(x, ...)
measure_thres(data,variables,weight_variable=NULL,missing=-999)
\method{print}{measure_thres}(x, ...)
}
\arguments{
  \item{data}{
  Input data
}
  \item{variables}{
  Names of key variables
}
  \item{weight_variable}{
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
  indices of the variables used for l-diversity
  }
   \item{x}{
   Output of measure_risk, measure_hier or measure_thres
  }
 \item{...}{
  currently unused
}
}
\value{
xx
}
\references{
\url{http://neon.vb.cbs.nl/casc/Software/MuManual4.1.pdf}
}
\author{
Alexander Kowarik, Bernd Prantner, IHSN C++ source
}
\seealso{\code{measure_hier}}
\examples{
  # measure_risk
  data(testdata)
  res <- measure_risk(testdata,c("urbrur","roof","walls","water","sex"))
  print(res)
  head(res$Res)
  resw <- measure_risk(testdata,c("urbrur","roof","walls","water","sex"),weight_variable="sampling_weight")
  print(resw)
  head(resw$Res)
  res1 <- measure_risk(testdata,c("urbrur","roof","walls","water","sex"),ldiv_index=c(1))
  print(res1)
  head(res1$Mat_Risk)
  res2 <- measure_risk(testdata,c("urbrur","roof","walls","water","sex"),ldiv_index=c(1,2))
  print(res2)
  head(res2$Mat_Risk)
  # measure_hier
  resh <- measure_hier(testdata,c("urbrur","roof","walls","water","sex"),weight_variable="sampling_weight",hid="ori_hid")
  print(resh)
  # measure_thres
  rest <- measure_thres(testdata,c("urbrur","roof","walls","water","sex"),weight_variable="sampling_weight")
  print(rest)
}
\keyword{ manip }