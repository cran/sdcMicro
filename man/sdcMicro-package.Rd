\name{sdcMicro-package}
\alias{sdcMicro-package}
\alias{sdcMicro}
\docType{package}
\title{
Statistical Disclosure Control (SDC) for the generation of protected microdata for researchers and for public use.
}
\description{
This package includes most of the methods of the popular software mu-Argus and includes additional methods which are not included in mu-Argus.
The first version of this package does not include the PRAM method nor Adding Noise methods nor synthetic data generation methods like LHS. 
Since the code is already written, Adding Noise methods (also ROMM) and LHS
will be included in the next version of the package.
In comparison with mu-Argus the advantages of this package are that the results are reproducible, that the package can be used in batch-mode 
from other software, that the functions can be used in a very flexible way, that everybody could look at the source code and that there are no time-consuming meta-data management is necessary.
However, the user should have a detailed knowledge about SDC when applying the methods on data.
Since, it is quite natural for R users to deal with a CLI, there is no GUI implemented till now. Nevertheless, via Rtcltk it will be
feasible to implement a GUI if there will be a large demand for a GUI like in mu-Argus.

Please note, that methods \dQuote{shuffling}, \dQuote{robShuffle} (robust shuffling), \dQuote{gadp} and \dQuote{robgadp} are not included 
in the package because method \dQuote{shuffling} is under a US-patent by other authors, even shuffling consits only of 8 lines of code \ldots
}
\details{
\tabular{ll}{
Package: \tab sdcMicro\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2007-06-08\cr
License: \tab GPL 2.0 \cr
}

}
\author{
Matthias Templ

Maintainer: Matthias Templ <templ@statistik.tuwien.ac.at>
}
\references{
http://www.springerlink.com/content/4t0h5123v1436342/?p=4437dbfb85844df8842f7a73d92b54b2&pi=28
}
\keyword{ package }
\seealso{
}
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
f
f$fk
f$Fk
## with missings:
x <- francdat
x[3,5] <- NA
x[4,2] <- x[4,4] <- NA
x[5,6]  <- NA
x[6,2]  <- NA
f2 <- freqCalc(x,  keyVars=c(2,4,5,6),w=8)
f2$Fk
## individual risk calculation:
indivf <- indivRisk(f)  
indivf$rk         
## Local Suppression            
localS <- localSupp(f, keyVar=2, indivRisk=indivf$rk, threshold=0.25)
f2 <- freqCalc(localS$freqCalc, keyVars=c(2,4,5,6), w=8)
indivf2 <- indivRisk(f2)
indivf2$rk

## select another keyVar and run localSupp once again, if you think the table is not fully protected
data(free1)
f <- freqCalc(free1, keyVars=1:3, w=30)
ind <- indivRisk(f)
## and now you can use the interactive plot for individual risk objects: 
## plot(ind)

## Local suppression with localSupp2 and localSupp2Wrapper is more effective:
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
l1 <- localSupp2(francdat, keyVars=c(2,4,5,6), w=8)
l1
l1$x
l2 <- localSupp2(francdat, keyVars=c(2,4,5,6), w=8, k=2)
l3 <- localSupp2(francdat, keyVars=c(2,4,5,6), w=8, k=4)
## long computation time:
## l = localSupp2(free1, keyVar=1:3, w=30, k=2, importance=c(0.1,1,0.8))

## we want to avoid missings in column 5:
l1 <- localSupp2Wrapper(francdat, keyVars=c(2,4,5,6), importance=c(1,1,0,1), w=8, kAnon=1)
l1$x
## we want to avoid missings in column 5 and allow missings in 1 only if
## is really necessary:
l1 <- localSupp2Wrapper(francdat, keyVars=c(2,4,5,6), importance=c(0.1,1,0,1), w=8, kAnon=1)
l1$x
plot(l1)

## Data from mu-Argus:
## Global recoding:
data(free1)
free1[, "AGE"] <- globalRecode(free1[,"AGE"], c(1,9,19,29,39,49,59,69,100), labels=1:8)

## Top coding:
topBotCoding(free1[,"DEBTS"], value=9000, replacement=9100, kind="top")

## Numerical Rank Swapping:
## do not use the mu-Argus test data set (free1) since the numerical variables are (probably) faked.
data(Tarragona)
Tarragona1 <- swappNum(Tarragona, p=10) 

## Microaggregation:
m1 <- microaggregation(Tarragona, method="onedims", aggr=3)
m2 <- microaggregation(Tarragona, method="pca", aggr=3)
# summary(m1)
# valTable(Tarragona, method=c("simple","onedims","pca")) ## approx. 1 minute computation time

data(microData)
m1 <- microaggregation(microData, method="mdav")
x <- m1$x  ### fix me
summary(m1)
plotMicro(m1, 0.1, which.plot=1)  # too less observations...
data(free1)
plotMicro(microaggregation(free1[,31:34], method="onedims"), 0.1, which.plot=1)


## disclosure risk (interval) and data utility:
data(free1)
m1 <- microaggregation(Tarragona, method="onedims", aggr=3)
dRisk(x=Tarragona, xm=m1$blowxm)
dRisk(x=Tarragona, xm=m2$blowxm)
dUtility(x=Tarragona, xm=m1$blowxm)
dUtility(x=Tarragona, xm=m2$blowxm)

## S4 class code for Adding Noise methods will be included in the next version of sdcMicro.

## Fast generation of synthetic data with aprox. the same covariance matrix as the original one.

data(mtcars)
cov(mtcars[,4:6])
cov(dataGen(mtcars[,4:6],n=200))
pairs(mtcars[,4:6])
pairs(dataGen(mtcars[,4:6],n=200))

## PRAM

set.seed(123)
x <- sample(1:4, 250, replace=TRUE)
pr1 <- pram(x)
length(which(pr1$x == x))
x2 <- sample(1:4, 250, replace=TRUE)
length(which(pram(x2)$x == x2))

data(free1)
marstatPramed <- pram(free1[,"MARSTAT"])  

}
