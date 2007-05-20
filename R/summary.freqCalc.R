`summary.freqCalc` <-
function(object, ...){
  a1 <- c(apply(object$freqCalc, 2, function(x){ length(which(is.na(x))) }))
  P <- dim(object$freqCalc)[1]
  cat("\n Suppressions: \n")
  for(i in 1:length(a1)){
    if( a1[i] != 0 ) cat(paste("\nLocal suppression in", colnames(object$freqCalc)[i], ":", a1[i], "/", P, "\n")) 
  } 
}

