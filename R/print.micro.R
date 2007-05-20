`print.micro` <-
function(x, ...){
  cat(paste("\n Object created with method", x$method))
  cat("\n -------------------------")
  cat(paste("\n Aggregation level:", x$aggr))
  cat("\n")
}

