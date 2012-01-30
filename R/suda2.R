suda2 <- function(data,variables=NULL,missing=-999,DisFraction=0.01){
  if(is.null(variables))
    variables <- colnames(data)
	dataX <- data[,variables]
	dataX <- as.matrix(dataX)
	for(i in 1:ncol(dataX)){
		if(!is.numeric(dataX[,i]))
			dataX[,i] <- as.numeric(dataX[,i])
	}    
	dataX[is.na(dataX)] <- missing
	dat <- .Call("Suda2",dataX,missing, ncol(data),DisFraction)$Res
	colnames(dat) <- c(paste(variables,"_contribution",sep=""),"suda_score","dis_suda_score")
	res <- list("contributionPercent"=dat[,1:length(variables)],
		 "score" = dat[,"suda_score"],
		 "disScore" = dat[,"dis_suda_score"]
	)
	class(res) <- "suda2"
	invisible(res)
}

print.suda2 <- function(x, ...){
	SEQ <- seq(0,0.7,0.1)+.Machine$double.eps
	DISSudaScore <- paste(">",seq(0.0,0.7,0.1))
	tab <- table(cut(x$disScore, breaks=c(-1,SEQ)))
	res <- data.frame("thresholds"=DISSudaScore,
			          "number"=as.numeric(tab))
    cat("\n Dis suda scores table: \n")
	cat("- - - - - - - - - - - \n")
	print(res)
	cat(" - - - - - - - - - - - \n")
}
