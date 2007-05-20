`dRisk` <-
function(x,xm,k=0.01){
  mi <-  t(t(xm) - k*sd(xm))
  ma <-  t(t(xm) + k*sd(xm))
  w <- x < mi | x > ma
  tab <- table(w)
  as.numeric(tab[1]/sum(tab))
}

