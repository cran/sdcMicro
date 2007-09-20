pram <- function(x, pd=0.8, alpha=0.5){
  fac <- FALSE
  xpramed <- x
  if(class(x) == "factor"){ xpramed <- as.numeric(as.character(xpramed)); fac=TRUE}
  L <- length(table(xpramed))
  P <- matrix(, ncol=L, nrow=L)
  pds <- runif(L, min=pd, max=1)
  tri <- (1 - pds)/(L-1)
  for(i in seq(L)){
    P[i,] <- tri[i]
  }
  diag(P) <- pds
  p <- table(xpramed)/sum(xpramed)
  Qest <- P
  for(k in seq(L)){
    s <- sum(p*P[,k])
    for(j in seq(L)){
      Qest[k,j] <- P[j,k]*p[j]/s
    }
  }

  #Qest <-  sapply(seq(L), function(i) apply(P, 1, function(x) p[i]*x)[,i])/p*P

  R <- P %*% Qest
  EI <- matrix(0, ncol=L, nrow=L)
  diag(EI) <- 1
  Rs <- alpha * R + (1 - alpha) * EI
  
  for(i in 1:length(xpramed)){
    xpramed[i] <- sample(1:L, 1, prob=Rs[x[i],])
  }
  
  if( fac == TRUE ) xpramed <- as.factor(xpramed)
  res <- list(x=x, xpramed=xpramed, pd=pd, P=P, Rs=Rs, alpha=alpha)
  class(res) <- "pram"
  invisible(res)
}
