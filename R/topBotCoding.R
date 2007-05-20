`topBotCoding` <-
function(x, value, replacement, kind="top"){
  if( class(x) == "data.frame" ){
  if( kind == "top"){
    x[x > value, ] <- replacement
  } else{
    x[x < value, ] <- replacement
  }
  }
  if( class(x) == "numeric" ){
  if( kind == "top"){
    x[x > value ] <- replacement
  } else{
    x[x < value ] <- replacement
  }
  }
  if( class(x) == "factor"){
    x <- as.numeric(as.character(x))
  if( kind == "top"){
    x[x > value ] <- replacement
  } else{
    x[x < value ] <- replacement
  }
  x <- as.factor(x)
  }
  invisible(x)
}

