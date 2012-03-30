#require(sdcMicro)
#data(testdata)
#dat <- testdata[,c("urbrur","sex","roof")]
#dat$roof_pram <- 0

# handling of NA and NULL Values for weight-vector
## with strata
#x <- .Call("Pram",as.matrix(dat),-999,2,1,-1)
## only frequency
#x <- .Call("Pram",as.matrix(dat),-999,0,0,-1)

pram_strata <- function(data,variables=NULL,strata_variables=NULL,weights=NULL,seed=NULL,missing=-999){
  if(is.null(seed))
    seed <- floor(rnorm(1)*1000)
  if(is.null(variables)&&is.null(strata_variables))
    stop("Please define valid strata variables and variables to pram!")
  else if(is.null(variables)&&!is.null(strata_variables))
    variables <- colnames(data)[!colnames(data)%in%strata_variables]
  else if(is.null(strata_variables))
    stop("Please define strata variables!"            )
  if(is.null(weights))
    weights <- rep(1,length(variables))
  pvariables <- paste(variables,"_pram",sep="")
  dataX <- data[,c(strata_variables,variables),drop=FALSE]
  dataX[,pvariables] <- rep(0,nrow(dataX))
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i]))
      dataX[,i] <- as.numeric(dataX[,i])
  }
  dataX[is.na(dataX)] <- missing
  dataX <- as.matrix(dataX)
  res <- .Call("Pram",dataX,missing,length(strata_variables),weights,seed)$Mat
  class(res) <- "pram_strata"
  invisible(res)
}

print.pram_strata <- function(x, ...){
  pram_var <- colnames(x)[grep("pram",colnames(x))]
  var <- unlist(lapply(pram_var,function(x)substring(x,1,nchar(x)-5)))
  cat("Number of changed observations: \n")
  cat("- - - - - - - - - - - \n")
  for(i in 1:length(pram_var)){
    s <- sum(x[,var[i]]!=x[,pram_var[i]])
    p <- round(s/nrow(x)*100,2)
    cat(var[i]," != ",pram_var[i]," : ",s," (",p,"%)","\n",sep="")
  }
}
