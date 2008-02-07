`globalRecode` <-
function(x, breaks, labels=NULL, method="equidistant"){
   ## aequidistant   
   equidistant <- function(x, b=breaks){
     b1 <- seq(min(x), max(x), length.out=b+1)
     b1 <- round(b1)
     b1
   }   
   ## aequidistant, log(data)
  logEqui <- function(x, b=breaks){
    b1 <- log(x)
    b1 <- seq(min(b1), max(b1), length.out=b+1)
    b1 <- round(exp(b1))
    b1
  }
  ## überall gleich viele
  equalAmount <- function(x, b=breaks){
    SEQ <- c(0, (1:b)/b)
    b1 <- quantile(x, SEQ)
    b1 <- round(b1)
    b1
  }
  if( class(x) == "factor" ){
    x <- as.numeric(as.character(x))
    if( length(breaks) == 1){
       gr <- cut(x, breaks=get(method)(x), labels=labels, dig.lab=8)
    } else gr <- cut(x, breaks=breaks, labels=labels, dig.lab=8)
  } else{
    if( length(breaks) == 1){
      gr <- cut(x, breaks=get(method)(x), labels=labels, dig.lab=8)
    } else  gr <- cut(x, breaks=breaks, labels=labels, dig.lab=8)
    if(length(labels) != 0){ 
       gr <- as.numeric(as.character(gr))
    } else warning("not converted to a vector of class numeric")
  }    
  invisible(gr)
}

