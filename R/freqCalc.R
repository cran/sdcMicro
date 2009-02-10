`freqCalc` <-
function(x, keyVars=1:3 , w=4){
	x <- as.matrix(x)
	y <- x	
	
	x <- x[,keyVars]
	x <- apply(x, 2, function(x) { as.integer(as.factor(x))})
	x <- apply(x, 1, rbind)
	
	N <- dim(y)[1]
	S <- dim(y[,keyVars,drop=FALSE])[2]
	res <- .C(	"f2",
			as.integer(c(N,S)),
	    	as.integer(ifelse(is.na(x), -999999, x)),
			as.integer(rep(0,N)),
			as.numeric(rep(0.0, N)),
			as.numeric(if(length(w)==0) rep(1,N) else y[,w]),
			PACKAGE="sdcMicro", NUOK=TRUE)
	
	z <- list(freqCalc=y, keyVars=keyVars, w=w, indexG=NULL, fk=res[[3]], Fk=res[[4]], n1=length(which(res[[3]]==1)), n2=length(which(res[[3]]==2)))
	class(z) <- "freqCalc"
	invisible(z)
}

