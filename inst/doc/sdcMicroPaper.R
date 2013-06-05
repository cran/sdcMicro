### R code from vignette source 'sdcMicroPaper.Rnw'

###################################################
### code chunk number 1: sdcMicroPaper.Rnw:110-125
###################################################
ind <- c(1:11,29)
argus <- c(4,   4.1,   4.2, 5,    NA,   NA,    NA,    NA,    NA,    NA,    NA,     NA,   NA)
csol <- c(0.253, 0.271,0.296,0.396,0.382,0.466,0.524, 0.562, 0.705, 0.708, 0.743, 0.817, 1.26) 
rsol <- c(0.523, 0.691,0.917,1.383,5.534,39.9993,NA,  NA,    NA,    NA,    NA,    NA,    NA)
plot(csol, xaxt="n", ylim=c(0,40), type="o", xlab="Number of key variables", ylab="User computation time [in sec]", main=expression(paste("Frequency calculation: User computation time for the ",mu,"-Argus test data set")), cex.lab=1.7, cex.main=1.6, lwd=2)
lines(rsol, type="o", col="blue", lwd=2)
lines(argus, type="o", col="red", lwd=2)
legend("topright", legend=c("mu-Argus (out of memory with \n5 or more key variables)\n", "advanced R code (without loops)\n out of memory with 7 or more key variables\n", "R/C++ interface"), lwd=c(1,1,1), pch=1, col=c("red", "blue", "black"),    
       bty="n", cex=1.3)
#legend("right", legend=c("mu-Argus: Out of memory with 5 or more key variables", "R: Out of memory till 7 key variables", "R/C++: no problems with memory"),   
#       col=c("red", "blue", "black"), lwd=c(1,1,1))
axis(1, at=1:13, labels=c(1:12,29))
axis(1, at=12.5, labels=c("..."))
segments(x0=12.4, x1=12.4, y0=-2, y1=3, col="lightgrey")
segments(x0=12.6, x1=12.6, y0=-2, y1=3, col="lightgrey")


