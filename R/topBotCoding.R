setGeneric('topBotCoding', function(obj, ...) {standardGeneric('topBotCoding')})
setMethod(f='topBotCoding', signature=c('sdcMicroObj'),
    definition=function(obj, column,...) { 
      manipNumVars <- get.sdcMicroObj(obj, type="manipNumVars")
      x <- manipNumVars[,column]
      
      manipNumVars[,column] <- topBotCodingWORK(x, ...)
      
      obj <- nextSdcObj(obj)
      
      obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(manipNumVars))
      
      obj <- dRisk(obj)
      obj <- dUtility(obj)
      
      obj
    })
setMethod(f='topBotCoding', signature=c("ANY"),
    definition=function(obj, ...) { 
      topBotCodingWORK(x=obj,...)
    })



topBotCodingWORK <- function(x, value, replacement, kind="top", column=NULL){ #COLUMN FOR COMPATIBILITY WITH V4
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


