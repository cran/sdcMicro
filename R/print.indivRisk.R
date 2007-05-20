`print.indivRisk` <-
function(x, ...){
  cat("\n ----- individual risk ----- \n")
  cat(paste("  method = ", x$method, ", qual = ", x$qual,"\n", sep=""))
  cat(" --------------------------- \n")
}

