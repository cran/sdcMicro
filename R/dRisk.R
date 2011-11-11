dRisk <- function (x, xm, k = 0.01)
{
    if (dim(x)[1] != dim(xm)[1]) {
        warning("dimension of perturbed data and original data are different")
        xm <- xm[1:dim(x)[1], ]
    }
    mi <- t(t(xm) - k * apply(xm, 2, sd))
    ma <- t(t(xm) + k * apply(xm, 2, sd))
    w <- which(rowSums(x < mi | x > ma) %in% 0:1)
    as.numeric(length(w)/nrow(x))
}