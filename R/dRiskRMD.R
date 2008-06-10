dRiskRMD <-
function (x, xm, k = 0.01, k2 = 0.05)
{
    if (dim(x)[1] != dim(xm)[1]) {
        xm <- xm[1:dim(x)[1], ]
    }
    x <- scale(x)
    xm <- scale(xm)
    cent <- colMeans(x)
    covs <- covMcd(x)$cov
    rmd <- mahalanobis(x, center = cent, cov = covs)
    rmd <- sqrt(rmd)*0.05  ##rmd/max(rmd) * 2
    mi <- x - k * rmd
    ma <- x + k * rmd
    #w <- which(rowSums(xm < mi | xm > ma) %in% 0:1)
    w <- which(apply(xm < ma & xm > mi, 1, any) == TRUE)


    xd <- as.matrix(dist(xm))
    diag(xd) <- NA
    if (length(w) > 0) {
        ind <- apply(xd[w, , drop = FALSE], 1, function(x, k = k2) {
            min(x, na.rm = TRUE) > k2
        })
    }
    else {
        ind <- FALSE
    }
    riskvec1 <- riskvec2 <- rep(0, nrow(x))
    riskvec1[w] <- rmd[w]
    if (length(which(ind == TRUE)) > 0) {
        w2 <- as.integer(names(ind)[which(ind == TRUE)])
        riskvec2[w2] <- rmd[w2]
    }
    else {
        w2 <- NULL
    }
    w2 <- if (length(w) > 0) {
        w2
    }
    else {
        NULL
   }
    risk <- if (length(w) > 0) {
        length(w2)/dim(x)[1]
    }
    else 0
    wrisk <- if (length(w) > 0) {
        (length(w2) * sum((rmd[w2])))/dim(x)[1]
    }
    else 0
    list(risk1 = length(w)/dim(x)[1], risk2 = risk, wrisk1 = if (length(w) >
        0) (length(w) * sum((rmd[w])))/dim(x)[1] else 0, wrisk2 = wrisk,
        indexRisk1 = w, indexRisk2 = w2, riskvec1 = riskvec1,
        riskvec2 = riskvec2)
}

