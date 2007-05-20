`print.freqCalc` <-
function(x, ...){
  P <- dim(x)[2]
  cat("\n --------------------------\n")
  cat(paste(x$n1, "observation with fk=1 \n"))
  cat(paste(x$n2, "observation with fk=2 \n"))
  cat(" --------------------------\n")  
}

