dataGen <- function(x, n=200){

##Generate a random $n^{'} \times m$ matrix $A$ in such way that the covariance matrix $\Sigma_aa = I$.
M <- matrix(rnorm(n*ncol(x)), ncol=ncol(x))
##Use the Cholesky decomposition on C to obtain $C = U^t U$, where $U$ is an upper triangular matrix.
C <- cov(x)
chC <- chol(C)
##Obtain the synthetic data set $X^{'} = A U$
Xn <- M %*% chC
cm <- colMeans(x)
Xn <- t(t(Xn) +  cm)
Xn

}

