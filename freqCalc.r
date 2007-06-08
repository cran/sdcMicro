freqCalc <-
function (x, keyVars = 1:3, w = 4)
{
    y <- x
    cn2 <- colnames(x)
    cn <- colnames(x)[w]
    colnames(x)[w] <- "w"
    x <- as.data.frame(x[, c(keyVars), drop = FALSE])
    xm <- mapply(unique, x)
    xml <- mapply(length, xm)
    ft <- table(x)
    z1 <- merge(ft, y, by.x = 1:length(keyVars), by.y = keyVars,
        sort = FALSE)
    indexG <- as.numeric(factor(apply(z1[, 1:length(keyVars)],
        1, paste, collapse = ":")))
    xx <- cbind(z1, indexG)
    xx <- as.data.frame(xx[order(xx[, "indexG"]), ])
    z <- aggregate(xx[,cn], list(xx$indexG), sum)
    z <- merge(xx, z, by.x = "indexG", by.y = "Group.1")
    ##colnames(z)[ncol(z) - 1] <- cn
    colnames(z)[ncol(z)] <- cn
    colnames(z)[ncol(z)] <- "Fk"
    z <- cbind(z, z1$Freq)
    colnames(z)[ncol(z)] <- "fk"
    P <- dim(z)[2]
    n1 <- dim(z[z[, P] == 1, ])[1]
    n2 <- dim(z[z[, P] == 2, ])[1]
    z <- list(freqCalc = z[, -c(1, which(colnames(z) == "Freq"),
        ncol(z) - 1, ncol(z))][, c(cn2)], keyVars = keyVars,
        w = w, indexG = z$indexG, fk = z$fk, Fk = z$Fk, n1 = n1,
        n2 = n2)
    class(z) <- "freqCalc"
    invisible(z)
}
