`print.indivRisk` <-
function(x, ...){
#  cat("\n ----- individual risk ----- \n")
  cat(paste("method=", x$method, ", qual=", x$qual, sep=""))
  cat("\n --------------------------- \n")
  s <- sum(x$rk > median(x$rk)+2*mad(x$rk))
  cat(paste(s,"obs. with much higher risk than the main part"))
}

