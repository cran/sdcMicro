### R code from vignette source 'sdc_guidelines.rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: sdc_guidelines.rnw:164-201
###################################################
#require(sdcMicro)
#load("../Daten/ses.RData")	
f1 <- function(x){
	truth <- weighted.mean(x$earningsMonth, x$GrossingUpFactor.x)
	SEQ <- seq(10,100,10)
	risk <- risk2 <- utility <- utility2 <- perturbed <- perturbed2 <- numeric(length(SEQ))
	j <- 0
	for(i in SEQ){
		j=j+1
		ad <- addNoise(x[,c("earnings","earningsMonth")], noise=i, method="restr")
		ad2 <- microaggregation(x[,c("earnings","earningsMonth")], aggr=j+1, method="pca")
		perturbed[j] <- weighted.mean(ad$xm[,2], x$GrossingUpFactor.x)
		perturbed2[j] <- weighted.mean(ad2$mx[,2], x$GrossingUpFactor.x)
		utility[j] <- dUtility(ad$x, ad$xm)		
		risk[j] <- dRisk(ad$x, ad$xm, k=0.01)
		utility2[j] <- dUtility(ad$x, ad2$mx)		
		risk2[j] <- dRisk(ad$x, ad2$mx, k=0.01)
	}	
	list(truth=truth, perturbed=perturbed, utility=utility, risk=risk, 
			perturbed2=perturbed2, utility2=utility2, risk2=risk2, SEQ=SEQ)
}
#set.seed(123)
#res <- f1(x)
#save(res, file="../Daten/res.RData")
load("res.RData")
par(cex.lab=1.5, mar=c(5,4.5,1,0.1))
plot(cbind(res$risk, res$utility), type="l", 
	xlab="disclosure risk", ylab="information loss",
	xlim=c(0.08,0.26), ylim=c(0.1,1.95))
lines(cbind(res$risk2, res$utility2), lty=2)
text(x=res$risk, y=res$utility, res$SEQ)
text(x=res$risk2, y=res$utility2, 2:11)
text(x=0.22,y=0.5, "disclosive", cex=1.5)
text(x=0.21,y=1.8, "disclosive and worst data", cex=1.5)
text(x=0.1,y=0.5, "good", cex=1.5)
text(x=0.11,y=1.8, "worst data", cex=1.5)
legend("right", legend=c("method1","method2"), lty=c(1,2))	


###################################################
### code chunk number 2: freq
###################################################
require(sdcMicro)
require(xtable)
data(francdat)   ## toy data set
sdc <- createSdcObj(francdat, keyVars=c('Key1','Key2','Key3','Key4'), numVars=c('Num1','Num2','Num3'), w='w')
df <- cbind(francdat[,c(2,4,5,6,8)], get.sdcMicroObj(sdc, "risk")$individual)	
#colnames(df)[ncol(df)] <- expression(hat(F)[k])
df <- xtable(df, digits=c(0,0,0,0,0,1,3,0,1), align = "|l|llll|l|l|ll|",
	caption="Example of sample and estimated population frequency counts.", 
	label="listingFreq")


###################################################
### code chunk number 3: freqprint
###################################################
print(df,include.rownames = getOption("xtable.include.rownames", TRUE), caption.placement="top")


###################################################
### code chunk number 4: suda
###################################################
data(francdat)
x <- francdat[,c(2,4,5,6,8)]
ff <- freqCalc(x, keyVars=1:4, w=5)
s <- suda2(francdat, variables=1:4)
df <- cbind(x[,1:4], fk=ff$fk, scores=s$score, disScores=s$disScore)
df <- xtable(df, digits=c(0,0,0,0,0,0,2,4), align = "|l|llll|l|ll|",
	caption="Example of SUDA scores (scores) and DIS SUDA scores (disScores).", 
	label="listingsuda")


###################################################
### code chunk number 5: freqprint
###################################################
print(df,include.rownames = getOption("xtable.include.rownames", TRUE), caption.placement="top")


###################################################
### code chunk number 6: all
###################################################
sdc <- createSdcObj(francdat, keyVars=c('Key1','Key2','Key3','Key4'), numVars=c('Num1','Num2','Num3'), w='w')
df <- francdat[,c(2,4,5,6,7,8)]
sdc <- ldiversity(sdc,ldiv_index="Num3")
sdc <- suda2(sdc)
df <- cbind(df, ldiv=sdc@risk$ldiversity[,1])
df <- cbind(df, suda=sdc@risk$suda$score)
df <- cbind(df, get.sdcMicroObj(sdc, "risk")$individual)
df <- df[,c(1:6, 10:11, 7,8,9)]
df <- xtable(df, digits=c(0,0,0,0,0,0,1,0,1,0,2,4), align = "|l|llll|l|l|lll|l|l|",
	caption="Display of frequency counts, l-diversity, SUDA and individual risk. The continuous variable (Num3) was chosen as sensitive variable for $l$-diversity.", 
	label="listingIndiv")


###################################################
### code chunk number 7: allprint
###################################################
print(df,include.rownames = getOption("xtable.include.rownames", TRUE), caption.placement="top")


###################################################
### code chunk number 8: sdc_guidelines.rnw:664-665
###################################################
print(sdc, "risk")


###################################################
### code chunk number 9: sdc_guidelines.rnw:908-911
###################################################
set.seed(1234)
A <- as.factor(rep(c("A1","A2","A3"), each=5))
A


###################################################
### code chunk number 10: sdc_guidelines.rnw:916-918
###################################################
Apramed <- pram(A)
Apramed	


###################################################
### code chunk number 11: sdc_guidelines.rnw:925-926
###################################################
summary(Apramed)	


###################################################
### code chunk number 12: microaggregation
###################################################
df <- francdat[,c(1,3,7)]	
df <- cbind(df, microaggregation(df, aggr=2)$mx)
colnames(df)[4:6] <- paste("Mic",1:3, sep="")
df <- xtable(df, digits=c(0,2,3,0,2,2,1), align = "|l|lll|lll|",
	caption="Example of micro-aggregation. Columns 1-3 contain the original variables, columns 4-6 the micro-aggregated values.", 
	label="listingMicroaggregation")


###################################################
### code chunk number 13: allprint
###################################################
print(df,include.rownames = getOption("xtable.include.rownames", TRUE), caption.placement="top")


