measure_hier <- function(data,variables,weight_variable=NULL,hid,missing=-999){
  risk <- measure_risk(data=data,variables=variables,weight_variable=weight_variable,missing=missing)
  ind <- order(data[,hid])
  dataX <- cbind(data[,hid],risk$Res[,2])[ind,]
  ind <- order(c(1:nrow(dataX))[ind])
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i]))
      dataX[,i] <- as.numeric(dataX[,i])
  }
  dataX <- as.matrix(dataX)
  
  res <- .Call("measure_hierachical",dataX)
  class(res) <- "measure_hier"
  res$Res <- res$Res[ind]
  invisible(res)
}
print.measure_hier <- function(x, ...){
  cat("hier_risk_ER:",x$hier_risk_ER,"\n")
  cat("hier_risk:",x$hier_risk,"\n")
  cat("hier_risk_pct:",x$hier_risk_pct,"\n")
  cat("- - - - - - - - - - - \n")
  print(summary(x$Res))
}
