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
  if(is.null(variables))
    stop("Please define valid strata variables and variables to pram!")
  if(is.null(strata_variables)){
    data$strata_variables <- 1
    strata_variables <- "strata_variables"
  }
  if(length(variables)==1){
    variables <- c(variables,"pram_dummy_var1","pram_dummy_var2")
    data$pram_dummy_var1 <- 1
    data$pram_dummy_var2 <- 1
  }
  
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
  if("pram_dummy_var1"%in%colnames(res)){
    res <- res[,-which(colnames(res)%in%c("pram_dummy_var1","pram_dummy_var2","pram_dummy_var1_pram","pram_dummy_var2_pram"))]
  }
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
