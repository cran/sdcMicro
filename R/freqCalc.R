`freqCalc` <-
function(x, keyVars=1:3 , w=4){
x <- as.matrix(x)
#if( any(apply(x[, keyVars], 2, class)) ) x[, keyVars] <- apply(x[, keyVars], 2, function(x) as.integer(as.factor(x)))
y <- x
#x <- ifelse(is.na(x), -999999, x)
x <- as.integer(apply(x[,keyVars], 1, rbind))
N <- dim(y)[1]
S <- dim(y[,keyVars,drop=FALSE])[2]
res <- .C(	"f2",
		as.integer(c(N,S)),
    as.integer(ifelse(is.na(x), -999999, x)),
		as.integer(rep(0,N)),
		as.numeric(rep(0.0, N)),
		as.numeric(if(length(w)==0) rep(1,N) else y[,w]),
		#as.numeric(y[,w]),
		PACKAGE="sdcMicro", NUOK=TRUE)

z <- list(freqCalc=y, keyVars=keyVars, w=w, indexG=NULL, fk=res[[3]], Fk=res[[4]], n1=length(which(res[[3]]==1)), n2=length(which(res[[3]]==2)))
class(z) <- "freqCalc"
invisible(z)

}

