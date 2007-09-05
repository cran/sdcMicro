`globalRecode` <-
function(x, breaks, labels){
  if( class(x) == "factor" ){
    x <- as.numeric(as.character(x))
    gr <- cut(x, breaks=breaks, labels=labels)
  } else{
    gr <- cut(x, breaks=breaks, labels=labels)
    gr <- as.numeric(as.character(gr))
  }    
  invisible(gr)
}

