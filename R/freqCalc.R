`freqCalc` <-
function(x, keyVars=1:3 , w=4){
  ## x       ... data
  ## keyVars ... index of the key variables
  ## w       ... index of the weigth vector
  ## fixme: Preparing the function according to missing values
  y <- x
  cn2 <- colnames(x)
  if( length(w) > 0 ){
    cn <- colnames(x)[w]
    colnames(x)[w] <- "w"  
  }
  x <- as.data.frame(x[, c(keyVars),drop=FALSE]) 
  #xm <- mapply(unique, x)
  #xml <- mapply(length, xm)
  #if( any( xml > c(rep(500, ncol(x))) ) ){
	#  stop("too many different discrete values or some values are not of class integer")}
  ft <- table(x) 
  #ft2 <- ftable(x[, keyVars])
  #indexG <- as.numeric(factor(apply(x[,keyVars,drop=FALSE], 1, paste, collapse=":"))) 
  #indexG <- as.numeric(factor(apply(x, 1, paste, collapse=":")))  # was gehoert zusammen
  #Fk <- aggregate(x$WEIGHT, list(indexG), sum)
  #z1 <- merge(ft, y[, keyVars, drop=FALSE], by=keyVars, sort=FALSE)
  z1 <- merge(ft, y, by.x=1:length(keyVars),by.y=keyVars, sort=FALSE)
  indexG <- as.numeric(factor(apply(z1[,1:length(keyVars)], 1, paste, collapse=":")))  # was gehoert zusammen
  #z <- cbind(z,indexG,x[,w])
  #xx <- cbind(y[,keyVars], indexG, y[, ncol(x)])
  xx <- cbind(z1, indexG)  
  xx <- as.data.frame(xx[order(xx[,"indexG"]),])
  if( length(w) > 0 ){
    z <- aggregate(xx[,cn], list(xx$indexG), sum)
  } else {
     z <- aggregate(rep(1,nrow(xx)), list(xx$indexG), sum)
  }
  z <- merge(xx, z, by.x="indexG", by.y="Group.1")
  #colnames(z)[ncol(z)-1] <- cn
  if( length(w) > 0 ){
    colnames(z)[ncol(z)] <- cn
  }
  colnames(z)[ncol(z)] <- "Fk"
  z <- cbind(z, z$Freq)
  colnames(z)[ncol(z)] <- "fk"
  P <- dim(z)[2]  
  n1 <- dim(z[z[,P]==1,])[1]
  n2 <- dim(z[z[,P]==2,])[1]
  if( length(w) > 0 ){
    z <- list(freqCalc=z[, -c(1,which(colnames(z)=="Freq"),ncol(z)-1,ncol(z))][,c(cn2)], 
            keyVars=keyVars, w=w, indexG=z$indexG, fk=z$fk, Fk=z$Fk, n1=n1, n2=n2)
  } else{ z <- list(freqCalc=z[, -c(1,which(colnames(z)=="Freq"),ncol(z)-1,ncol(z))][,c(cn2)], 
            keyVars=keyVars, w=NULL, indexG=z$indexG, fk=z$fk, Fk=z$Fk, n1=n1, n2=n2)
  }
  class(z) <- "freqCalc"
  invisible(z)
}

    