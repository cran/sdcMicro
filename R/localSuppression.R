localSuppression <- function(x, keyVars, w=NULL, k=2, importance=NULL) {
	my.dist <- function(dat, keyVars, ind) {
		l <- lapply(1:length(keyVars), function(x) { dat[,keyVars[x]] != dat[ind,keyVars[x]] }) 
		for (i in seq_along(l) ) {
			xind <- which(is.na(l[[i]]))
			if ( length(xind) > 0 )  {
				l[[i]][xind] <- FALSE
			}
		}
		dists <- Reduce("+", l)
		dists[ind] <- 0
		dists
	}
	
	if ( is.null(importance) ) {
		xx <- apply(x[,keyVars,drop=FALSE], 2, function(x) { length(table(x)) } )
		importance <- match(xx, sort(xx, decreasing=FALSE))
	} else {
		if ( length(setdiff(sort(importance), 1:length(keyVars))) > 0 ) {
			stop("importance vector needs to be discrete numbers between 1 and the number of key-variables!\n")
		} 	
	}
	
	# nr supps before doing anything
	totalNABefore <- length(which(is.na(x)))
	NAinKey <- apply(x[, keyVars], 2, function(x) length(which(is.na(x))))
		
	ff <- freqCalc(x, keyVars=keyVars, w=w)
	rk <- indivRisk(ff)
	runInd <- TRUE
	
	importanceI <- (length(importance)+1)-importance
	
	while ( runInd ) {
		ind.problem <- which(ff$fk < k)
		ind.problem  <- ind.problem[order(rk$rk[ind.problem],decreasing=TRUE)]
		for ( i in 1:length(ind.problem ) ) {
			dists <- my.dist(x, keyVars, ind.problem[i])
			if ( length(which(dists==0)) >= k ) {
				break
			}
			
			minDist <- sort(unique(dists))[2]
			ind <- which(dists==minDist)
			if ( length(ind) > 0 ) {
				colInd <- NULL
				colIndsSorted <- keyVars[order(importanceI)]
				while( is.null(colInd) ) {
					for ( cc in colIndsSorted ) {
						z <- which(x[ind.problem[i],cc]!=x[ind,cc] & !is.na(x[ind,cc]))
						if ( length(z) > 0 ) {
							colInd <- cc
							break
						}							
					}
				}
				x[ind.problem[i],colInd] <- NA
			} else {
				stop("Error\n")
			}
		}
		ff <- freqCalc(x, keyVars=keyVars, w=w)
		rk <- indivRisk(ff)
		if ( all(ff$fk >= k) ) {
			runInd <- FALSE
		} 
	}
		
	## preparing the output:
	totalNA <- length(which(is.na(x)))
	supps <- apply(x[, keyVars], 2, function(x) length(which(is.na(x)))) - NAinKey
	names(supps) <- colnames(x)[keyVars]
	
	res <- list(xAnon=x, supps=supps, totalSupps=totalNABefore-totalNA, 
			anonymity=TRUE, keyVars=keyVars, importance=importance, k=k)
	
	class(res) <- "localSuppression"
	invisible(res)	
}
