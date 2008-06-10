addNoise <- function (x, noise = 150, method = "additive", p = 0.001, delta = 0.1)
{
    N <- dim(x)[1]
    P <- dim(x)[2]
    y = x
    wnoise <- noise/100
    addN <- apply(x, 2, median) * wnoise
    eps <- matrix(0, ncol = P, nrow = N)
    if (method == "additive") {
        x <- apply(x, 2, function(x) {
            x + rnorm(N, 0, 1.96 * sd(x)/sqrt(N) * wnoise)
        })
    }
    if (method == "correlated") {
        if (dim(x)[2] < 2)
            stop("must have more than 2 variables")
        x <- x + mvrnorm(N, colMeans(x), Sigma = noise/100 *
            cov(x))
    }
    if (method == "correlated2") {
        d1 <- sqrt(1 - delta^2)
        x <- apply(x, 2, function(x) {
            x * d1 + delta * rnorm(N, mean = (1 - d1)/delta *
                mean(x), sd = sd(x))
        })
    }
    if (method == "restr") {
        if (N < 500) {
            cc <- sqrt((N - 1 - wnoise)/((N + 1) * (1 + wnoise)))
        }
        else {
            cc <- sqrt((N - 1)/(N + N * wnoise - 1))
        }
        d <- (1 - cc) * colMeans(x)
        x <- cc * x + d
    }
    if (method == "ROMM") {
        #require(far)
        print("please load package far")
        ROMM <- function(x, p1 = p) {
            M <- matrix(rnorm(N * N), ncol = N, nrow = N)
            I <- diag(1, N)
            P <- I + p1 * M
            Torthon <- orthonormalization(P)
            x <- Torthon %*% as.matrix(x)
        }
        cn1 <- colnames(x)
        rn1 <- rownames(x)
        x <- ROMM(x, p)
        colnames(x) <- cn1
        rownames(x) <- rn1
    }
    if (method == "outdect") {
        q1 <- apply(x, 2, quantile, 0.99)
        r <- list()
        for (i in 1:ncol(x)) {
            r[[i]] <- which(x[, i] > q1[i])
        }
        univOutlier <- unlist(r)
        limit <- sqrt(qchisq(0.975, dim(x)[2]))
        xMcd <- covMcd(x, alpha = 1/2)
        rd <- sqrt(mahalanobis(x, xMcd$center, xMcd$cov))
        rdOutlier <- which(rd > limit)
        outliers <- unique(sort(c(univOutlier, rdOutlier)))
        for (i in 1:P) {
            x[outliers, i] <- x[outliers, i] + rnorm(length(outliers),
                0, 1.96 * sd(x)/sqrt(N) * wnoise)
        }
    }
    colnames(x) <- colnames(y)
    res <- list(x = y, xm = x, method = paste("addNoise:", method),
        noise = noise)
    class(res) <- "micro"
    res
}