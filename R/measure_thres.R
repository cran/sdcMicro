measure_thres <- function(data,variables,weight_variable=NULL,missing=-999){
  risk <- measure_risk(data=data,variables=variables,weight_variable=weight_variable,missing=missing)
  ind <- order(risk$Res[,2])
#  dataX <- risk$Res[ind,2]
#  ind <- order(c(1:length(dataX))[ind])
  res <- .Call("measure_threshold",risk$Res[ind,2],risk$global_risk)
  class(res) <- "measure_thres"
  #res$Res <- res$Res[ind]
  invisible(res)
}
print.measure_thres <- function(x, ...){
  cat("global_threshold:",x$global_threshold,"\n")
}
