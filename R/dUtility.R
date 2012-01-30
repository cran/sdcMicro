dUtility <- function (x, xm, method = "IL1")
{
    if (dim(x)[1] != dim(xm)[1]) {
        warning("dimension of perturbed data and original data are different")
        xm <- xm[1:dim(x)[1], ]
    }
    if (method == "IL1") {
        a <- x
        for (i in 1:dim(x)[2]) {
            a[, i] <- abs((x[, i] - xm[, i])/sd(x[, i]) * sqrt(2))
        }
        infLoss1 <- 1/(dim(x)[2]*dim(x)[1]) * sum(a)
        return(infLoss1)
    }
    if (method == "eigen") {
        e1 <- eigen(cov(x))$values
        e2 <- eigen(cov(xm))$values
        d <- sum(abs(e1 - e2))
        return(d)
    }
    if (method == "robeigen") {
        e1 <- eigen(covMcd(x)$cov)$values
        e2 <- eigen(covMcd(xm)$cov)$values
        d <- sum(abs(e1 - e2))
        return(d)
    }
}
