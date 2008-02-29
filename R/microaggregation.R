`microaggregation` <-
function (x, method = "pca", aggr = 3, nc = 8, clustermethod = "clara", 
    opt = FALSE, measure = "mean", trim = 0, varsort = 1, transf = "log", 
    blow = TRUE, blowxm = 0) 
{
    stopifnot( method %in% c("simple", "single", "onedims", "pca", "mcdpca", "pppca", "clustmcdpca",
                             "clustpppca", "clustpca", "rmd", "mdav", "influence"  ))
    if( method == "rmd" ){
      blow = FALSE
      warning("object$blow have been set to TRUE and 
           object$xm == object$blowxm \n--------\n")
    }
    factorOfTotals <- function(x, aggr) {
        n <- dim(x)[1]
        abgerundet <- floor(n/aggr)
        fot <- solve(abgerundet, n)
        fot
    }
    "indexMicro" <- function(x, aggr) {
        n <- dim(x)[1]
        if (n < 2 * aggr) {
            stop(paste("Too less observations (", n, ") for aggregate = ", 
                aggr, sep = ""))
        }
        aa <- seq(1, n, aggr)
        j <- 1
        teiler <- n/aggr
        d1 <- 1:n
        index <- list()
        if (teiler %in% 1:n) {
            for (i in 1:length(aa)) {
                index[[i]] <- d1[j:(j + aggr - 1)]
                j <- j + aggr
            }
        }
        else {
            for (i in 1:(length(aa) - 2)) {
                index[[i]] <- d1[j:(j + aggr - 1)]
                j <- j + aggr
            }
            index[[i + 1]] <- d1[(j):n]
        }
        index
    }
    "means" <- function(x, index, measure, trim = 0) {
        m <- matrix(ncol = ncol(x), nrow = length(index))
        if (measure == "mean") {
            for (i in 1:length(index)) {
                m[i, ] <- colMeans(x[index[[i]], ])
            }
        }
        if (measure == "median") {
            for (i in 1:length(index)) {
                m[i, ] <- apply(x[index[[i]], ], 2, median)
            }
        }
        if (measure == "trim") {
            for (i in 1:length(index)) {
                for (j in 1:length(index[[i]])) {
                  m[i, ] <- apply(x[index[[i]], ], 2, mean, trim = trim)
                }
            }
        }
        if (measure == "onestep") {
            y <- x
            constant <- 3/1.486
            for (i in 1:length(index)) {
                m1 <- apply(x[index[[i]], ], 2, median)
                m2 <- apply(x[index[[i]], ], 2, mad)
                limit1 <- m1 + constant * m2
                limit2 <- m1 - constant * m2
                for (ii in 1:length(index[[i]])) {
                  if (any(x[index[[i]][ii], ] > limit1)) {
                    w <- which(x[index[[i]][ii], ] > limit1)
                    le <- length(w)
                    y[index[[i]][ii], w] <- limit1[w]
                  }
                  if (any(x[index[[i]][ii], ] < limit2)) {
                    w <- which(x[index[[i]][ii], ] < limit2)
                    le <- length(w)
                    y[index[[i]][ii], w] <- limit2[w]
                  }
                  m[i, ] <- colMeans(y[index[[i]], ])
                }
            }
        }
        colnames(m) <- colnames(x)
        return(m)
    }
    blowup <- function(x, mr, aggr) {
        n <- dim(x)[1]
        aa <- seq(1, n, aggr)
        j <- 1
        teiler <- n/aggr
        d1 <- 1:n
        xx <- matrix(0, ncol = ncol(x), nrow = nrow(x))
        if (teiler %in% 1:n) {
            for (i in 1:length(aa)) {
                for (s in j:(j + aggr - 1)) {
                  xx[s, ] <- as.matrix(mr[i, , drop = FALSE])
                }
                j <- j + aggr
            }
        }
        else {
            for (i in 1:(length(aa) - 2)) {
                for (s in j:(j + aggr - 1)) {
                  xx[s, ] <- as.matrix(mr[i, , drop = FALSE])
                }
                j <- j + aggr
            }
            for (s in j:n) {
                xx[s, ] <- mr[i + 1, ]
            }
        }
        rownames(xx) <- rownames(x)
        xx
    }
    "clust" <- function(x, nc, clustermethod = "Mclust", opt = FALSE, 
        transf = "log") {
        if (transf == "log") {
            y <- scale(log(x))
        }
        if (transf == "boxcox") {
            lambda <- box.cox.powers(x)$lambda
            y <- scale(box.cox(x, lambda))
        }
        if (clustermethod == "clara") {
            a <- clara(x, nc)
            clustresult <- a$clust
            centers <- a$med
            size <- a$clusinfo[, 1]
        }
        if (clustermethod == "pam") {
            a <- pam(x, nc)
            clustresult <- a$clust
            centers <- a$med
            size <- a$clusinfo[, 1]
        }
        if (clustermethod == "kmeans") {
            a <- kmeans(x, nc)
            centers <- a$centers
            clustresult <- a$cluster
            size <- a$size
        }
        if (clustermethod == "cmeans") {
            a <- cmeans(x, nc)
            centers <- a$centers
            clustresult <- a$cluster
            size <- a$size
            res@mem <- a$mem
        }
        if (clustermethod == "bclust") {
            a <- bclust(x, nc)
            centers <- a$centers
            groesse <- rep(0, nc)
            for (i in seq(nc)) {
                groesse[i] <- length(which(a$cluster == i))
            }
            size <- groesse
            clustresult <- a$cluster
        }
        if (clustermethod == "Mclust" && opt == FALSE) {
            a <- Mclust(x, nc, nc)
            centers <- t(a$mu)
            groesse <- rep(0, nc)
            for (i in seq(nc)) {
                groesse[i] <- length(which(a$classification == 
                  i))
            }
            size <- groesse
            clustresult <- a$classification
        }
        if (clustermethod == "Mclust" && opt == TRUE) {
            a <- Mclust(x, 2, nc)
            centers <- t(a$mu)
            nc <- a$G
            groesse <- rep(0, nc)
            for (i in seq(nc)) {
                groesse[i] <- length(which(a$classification == 
                  i))
            }
            size <- groesse
            clustresult <- a$classification
        }
        list(centers = centers, clustresult = clustresult, nc = nc)
    }
    "prcompRob" <- function(X, k = 0, sca = "mad", scores = TRUE) {
        n <- nrow(X)
        p <- ncol(X)
        if (k == 0) {
            p1 <- min(n, p)
        }
        else {
            p1 <- k
        }
        S <- rep(1, p1)
        V <- matrix(1:(p * p1), ncol = p1, nrow = p)
        P <- diag(p)
        m <- apply(X, 2, median)
        Xcentr <- scale(X, center = m, scale = FALSE)
        for (k in 1:p1) {
            B <- Xcentr %*% P
            Bnorm <- sqrt(apply(B^2, 1, sum))
            A <- diag(1/Bnorm) %*% B
            Y <- A %*% P %*% t(X)
            if (sca == "mad") 
                s <- apply(Y, 1, mad)
            if (sca == "tau") 
                s <- apply(Y, 1, scale.tau)
            if (sca == "A") 
                s <- apply(Y, 1, scale.a)
            j <- order(s)[n]
            S[k] <- s[j]
            V[, k] <- A[j, ]
            if (V[1, k] < 0) 
                V[, k] <- (-1) * V[, k]
            P <- P - (V[, k] %*% t(V[, k]))
        }
        if (scores) {
            list(scale = S, loadings = V, scores = Xcentr %*% 
                V)
        }
        else list(scale = S, loadings = V)
    }
    fot <- factorOfTotals(x, aggr)
    if (method == "simple" || method == "single" || method == 
        "pca" || method == "mcdpca" || method == "pppca") {
        clustering <- FALSE
    }
    else {
        clustering <- TRUE
    }
    if (method == "simple") {
        index <- indexMicro(x, aggr)
        m <- means(x = x, index = index, measure = measure, trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(x, m, aggr)#mr
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod, 
            measure = measure, trim = trim, varsort = varsort, 
            transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    }
    if (method == "single") {
        sortvec <- sort(x[, varsort], index.return = TRUE)$ix
        xx <- x[sortvec, ]
        index <- indexMicro(xx, aggr)
        m <- means(x = xx, index = index, measure = measure, 
            trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(x, m, aggr)#mr
            rownames(blowxm) <- rownames(xx)
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod, 
            measure = measure, trim = trim, varsort = varsort, 
            transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    }
    if (method == "onedims") {
        i <- dim(x)[2]
        xx <- sapply(1:i, function(i) {
            x[order(x[, i]), i]
        })
        xxx <- sapply(1:i, function(i) {
            rank(x[, i], ties = "min")
        })
        index <- indexMicro(xx, aggr)
        m <- means(x = xx, index = index, measure = measure, 
            trim = trim)
        mr <- round(m)
        blow = TRUE
        b <- blowup(x, m, aggr)#mr
        y <- x
        for (i in 1:dim(x)[2]) {
            y[, i] <- b[xxx[, i], i]
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod, 
            measure = measure, trim = trim, varsort = varsort, 
            transf = transf, blow = blow, blowxm = y, fot = fot)
    }
    if (method == "pca") {
        p <- princomp(scale(x))
        s1 <- sort(p$scores[, 1], index.return = TRUE)$ix
        xx <- x[s1, ]
        index <- indexMicro(xx, aggr)
        m <- means(x = xx, index = index, measure = measure, 
            trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(x, m, aggr) #mr, aggr)
            rownames(blowxm) <- rownames(xx)
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod, 
            measure = measure, trim = trim, varsort = varsort, 
            transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    }
    if (method == "mcdpca") {
        x.mcd <- cov.mcd(x, cor = TRUE)
        x.scale <- scale(x, x.mcd$center, sqrt(diag(x.mcd$cor)))
        p <- princomp(x.scale, covmat = x.mcd)
        s1 <- sort(p$scores[, 1], index.return = TRUE)$ix
        xx <- x[s1, ]
        index <- indexMicro(xx, aggr)
        m <- means(x = xx, index = index, measure = measure, 
            trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(x, m, aggr)#mr
            rownames(blowxm) <- rownames(xx)
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod, 
            measure = measure, trim = trim, varsort = varsort, 
            transf = transf, blowup = blowup, blowxm = blowxm, 
            fot = fot)
    }
    if (method == "pppca") {
        p <- prcompRob(x)
        s1 <- sort(p$scores[, 1], index.return = TRUE)$ix
        xx <- x[s1, ]
        index <- indexMicro(xx, aggr)
        m <- means(x = xx, index = index, measure = measure, 
            trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(x, m, aggr)#mr
            rownames(blowxm) <- rownames(xx)
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod, 
            measure = measure, trim = trim, varsort = varsort, 
            transf = transf, blowup = blowup, blowxm = blowxm, 
            fot = fot)
    }
    if (method == "influence") {
        ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod, 
            opt = FALSE, transf = "log")
        cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
        j <- matrix(ncol = 1, nrow = nc)
        vmax <- matrix(ncol = 1, nrow = nc)
        for (i in 1:nc) {
            j[i, ] <- max(cent[, i])
            vmax[i, ] <- which(cent[, i] == j[i, ])
        }
        ncols <- c(1:ncol(x))
        xx <- list()
        for (i in 1:nc) {
            w <- which(ac.scale$clustresult == i)
            s <- x[w, , drop = FALSE]
            xx[[i]] <- s[order(s[, vmax[i]]), ]
        }
        for (i in 1:nc) {
            if (i == 1) {
                yy <- matrix(unlist(xx[[i]]), ncol = ncol(x), 
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]])))
            }
            if (i > 1) {
                yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x), 
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]]))))
            }
        }
        xx <- yy
        index <- indexMicro(xx, aggr)
        m <- means(x = xx, index = index, measure = measure, 
            trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(x, m, aggr)#mr
            rownames(blowxm) <- rownames(yy)
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = ac.scale$nc, xm = m, roundxm = mr, 
            clustermethod = clustermethod, measure = measure, 
            trim = trim, varsort = varsort, transf = transf, 
            blowup = blowup, blowxm = blowxm, fot = fot)
    }
    if (method == "clustpca") {
        ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod, 
            opt = FALSE, transf = "log")
        cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
        xx <- list()
        for (i in 1:nc) {
            w <- which(ac.scale$clustresult == i)
            if (length(w) < dim(x)[2]) {
                y <- x[w, , drop = FALSE]
                xx[[i]] <- y[order(y[, varsort]), ]
            }
            else {
                p <- princomp(scale(x[w, , drop = FALSE]))$scores[, 
                  1]
                psortind <- sort(p, index.return = TRUE)$ix
                y <- x[w, , drop = FALSE]
                xx[[i]] <- y[psortind, ]
            }
        }
        for (i in 1:nc) {
            if (i == 1) {
                yy <- matrix(unlist(xx[[i]]), ncol = ncol(x), 
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]])))
            }
            if (i > 1) {
                yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x), 
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]]))))
            }
        }
        xx <- yy
        index <- indexMicro(xx, aggr)
        m <- means(x = xx, index = index, measure = measure, 
            trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(x, m, aggr)#mr
            rownames(blowxm) <- rownames(xx)
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = ac.scale$nc, xm = m, roundxm = mr, 
            clustermethod = clustermethod, measure = measure, 
            trim = trim, varsort = varsort, transf = transf, 
            blowup = blowup, blowxm = blowxm, fot = fot)
    }
    if (method == "clustmcdpca") {
        ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod, 
            opt = FALSE, transf = "log")
        cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
        xx <- list()
        for (i in 1:nc) {
            w <- which(ac.scale$clustresult == i)
            if (length(w) < dim(x)[2]) {
                y <- x[w, , drop = FALSE]
                xx[[i]] <- y[order(y[, varsort]), ]
            }
            else {
                x.mcd <- cov.mcd(x[w, ], cor = TRUE)
                x.scale <- scale(x[, w], x.mcd$center, sqrt(diag(x.mcd$cor)))
                p <- princomp(x.scale, covmat = x.mcd)$scores[, 
                  1]
                psortind <- sort(p, index.return = TRUE)$ix
                y <- x[w, , drop = FALSE]
                xx[[i]] <- y[psortind, ]
            }
        }
        for (i in 1:nc) {
            if (i == 1) {
                yy <- matrix(unlist(xx[[i]]), ncol = ncol(x), 
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]])))
            }
            if (i > 1) {
                yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x), 
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]]))))
            }
        }
        xx <- yy
        index <- indexMicro(xx, aggr)
        m <- means(x = xx, index = index, measure = measure, 
            trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(x, m, aggr)#mr
            rownames(blowxm) <- rownames(xx)
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = ac.scale$nc, xm = m, roundxm = mr, 
            clustermethod = clustermethod, measure = measure, 
            trim = trim, varsort = varsort, transf = transf, 
            blowup = blowup, blowxm = blowxm, fot = fot)
    }
    if (method == "clustpppca") {
        ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod, 
            opt = FALSE, transf = "log")
        cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
        xx <- list()
        for (i in 1:nc) {
            w <- which(ac.scale$clustresult == i)
            if (length(w) < dim(x)[2]) {
                y <- x[w, , drop = FALSE]
                xx[[i]] <- y[order(y[, varsort]), ]
            }
            else {
                p <- prcompRob(x[w, , drop = FALSE], 1)$scores
                psortind <- sort(p, index.return = TRUE)$ix
                y <- x[w, , drop = FALSE]
                xx[[i]] <- y[psortind, ]
            }
        }
        for (i in 1:nc) {
            if (i == 1) {
                yy <- matrix(unlist(xx[[i]]), ncol = ncol(x), 
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]])))
            }
            if (i > 1) {
                yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x), 
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]]))))
            }
        }
        xx <- yy
        index <- indexMicro(xx, aggr)
        m <- means(x = xx, index = index, measure = measure, 
            trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(x, m, aggr) #mr
            rownames(blowxm) <- rownames(xx)
        }
        res <- list(x = x, method = method, clustering = clustering, 
            aggr = aggr, nc = ac.scale$nc, xm = m, roundxm = mr, 
            clustermethod = clustermethod, measure = measure, 
            trim = trim, varsort = varsort, transf = transf, 
            blowup = blowup, blowxm = blowxm, fot = fot)
    }
    if( method == "rmd" ){
       ##x <- data.frame(x1 = round(rnorm(25,10,2),1), x2 <- round(runif(25,1,50)), x3 <- round(abs(rnorm(25,30,10))))
       ##x <- as.matrix(x)
       ##colnames(x)=c("x1","x2","x3")
       y <- x
       cm <- colMeans(x, na.rm=TRUE)  ## fuers Ruecktransf.
       csd <- sd(x, na.rm=TRUE)   ## fuers Ruecktransf.
       y <- apply(y, 2, function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
       d <- as.matrix(dist(y))
       rr <- covMcd(y)
       md <- mahalanobis(y, center=rr$center, cov=rr$cov)
       diag(d) <- NA
       #d[lower.tri(d)] <- NA
       kn <- function(d,s,ne){
         w <- rep(0,ne)
         for( i in 1:ne ){
           w[i] <- which.min(d[,s])
           d[w,s] <- NA
         }
         return(w)
       }
       for( i in 1:(floor(dim(x)[1]/aggr)-1) ){
         s <- which.max(md)
         md[s] <- NA
         w <- kn(d,s,3)
         d[w,] <- NA
         y[w,] <- rep(colMeans(y[w,]), each=aggr)
       }
         w <- which(!is.na(d[,1]))
         y[w,] <- rep(colMeans(y[w,]), each=length(w))
       ### Ruecktrans:
       for( i in 1: dim(x)[2] ){
         y[,i] <- (y[,i] * csd[i]) + cm[i]
       }
       res <- list(x = x, method = method, clustering = clustering,
       aggr = aggr, nc = nc, xm = y, roundxm = round(y), clustermethod = clustermethod,
       measure = measure, trim = trim, varsort = varsort,
       transf = transf, blow = TRUE, blowxm = y, fot = fot)
    }
    if (method == "mdav") {
        maxDistinct <- function(d, j) {
            return(which(d == max(d), arr.ind = TRUE)[1, j])
        }
        distToVec <- function(x) {
            b <- as.matrix(dist(rbind(x, maxD)))[1, -1]
            return(b)
        }
        d <- as.matrix(dist(x))
        maxD <- matrix(ncol = ncol(x), nrow = 1)
        distVecs <- matrix(ncol = dim(x)[1], nrow = 1)
        maxD <- x[maxDistinct(d, 1), ]
        findNearest <- function(x, d) {
            maxD <- x[maxDistinct(d, 1), ]
            distVecs <- apply(x, 1, FUN = distToVec)
            s <- sort(distVecs, index.return = TRUE)$ix[1:aggr]
            sk <- x[s, ]
            x <- x[-s, , drop = FALSE]
            d <- as.matrix(dist(x))
            maxD <- x[maxDistinct(d, 2), ]
            distVecs2 <- apply(x, 1, FUN = distToVec)
            s2 <- sort(distVecs2, index.return = TRUE)$ix[1:aggr]
            sk <- rbind(sk, x[s2, ])
            x <- x[-c(s2), , drop = FALSE]
            d <- d[-s2, -s2]
            list(sk = sk, x = x, d = d)
        }
        a <- findNearest(x, d)
        b <- a$sk
        while (dim(a$x)[1] >= 2 * aggr) {
            a <- findNearest(a$x, a$d)
            b <- rbind(b, a$sk)
        }
        if (dim(a$x)[1] >= aggr && dim(a$x)[1] < 2 * aggr) {
            b <- rbind(b, a$x)
        }
        if (dim(a$x)[1] < aggr && dim(a$x)[1] > 0) {
            b <- rbind(b, a$x)
        }
        index <- indexMicro(b, aggr)
        m <- means(x = b, index = index, measure = measure, trim = trim)
        mr <- round(m)
        if (blow == TRUE) {
            blowxm <- blowup(b, m, aggr) #mr, aggr)
        }
        res <- list(x = b, method = method, clustering = clustering, 
            aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod, 
            measure = measure, trim = trim, varsort = varsort, 
            transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    }
    class(res) <- "micro"
    res
}

