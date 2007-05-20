`dUtility` <-
function(x,xm){
  a <- x
  for( i in 1:dim(x)[2] ){ a[,i] <- abs((x[,i] - xm[,i])/sd(x[,i])*sqrt(2))}
  infLoss1 <- 1/dim(x)[2] * sum(a)
  infLoss1
}

