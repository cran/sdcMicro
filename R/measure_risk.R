measure_risk <- function(data,variables,weight_variable=NULL,missing=-999,l_recurs_c=2,ldiv_index=NULL){
  if(is.null(variables)||!variables%in%colnames(data))
    stop("Please define valid key variables")
  if(!is.null(weight_variable)){
    if(!weight_variable%in%colnames(data))
      stop("Weight variable not found!")
  }
    
  weighted <- 0
  if(!is.null(weight_variable))
    weighted <- 1
  n_key_vars <- length(variables)
  dataX <- data[,c(variables)]
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i]))
      dataX[,i] <- as.numeric(dataX[,i])
  }
  dataX <- as.matrix(dataX)
  ind <- do.call(order,data.frame(dataX))
  dataX <- dataX[ind,,drop=FALSE]
  ind <- order(c(1:nrow(dataX))[ind])
  if(weighted==1)
    dataX <-cbind(dataX,data[,weight_variable])
  if(is.null(ldiv_index))
    ldiv_index=-99
  if(ldiv_index[1]!=-99&&weighted==1){
      warning("L-diversity measures will only be computed for unweighted data!")
  }
  if(length(ldiv_index)>5)
    stop("Maximal number of sensitivity variables is 5")
  res <- .Call("measure_risk",dataX,weighted,n_key_vars,l_recurs_c,ldiv_index,missing)
  class(res) <- "measure_risk"
  res$Res <- res$Res[ind,]
  res$Mat_Risk <- res$Mat_Risk[ind,]
  invisible(res)
}

print.measure_risk <- function(x, ...){
  cat("global_risk_ER:",x$global_risk_ER,"\n")
  cat("global_risk:",x$global_risk,"\n")
  cat("global_risk_pct:",x$global_risk_pct,"\n")
  cat("- - - - - - - - - - - \n")
  x <- summary(as.factor(1/x$Res[,2]),maxsum=10000)
  cat("Frequencies:\n")
  print(x[1:10])
}
