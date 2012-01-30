rankSwap <- function(data,variables=NULL,TopPercent=5,BottomPercent=5,K0=-1,R0=.95,P=0,missing=-999,seed=NULL){
  if(is.null(variables)){
		if(is.matrix(data))
			variables <- 1:ncol(data)
		else
			variables <- colnames(data)
  }
  dataX <- data[,variables]
  dataX <- as.matrix(dataX)
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i]))
      dataX[,i] <- as.numeric(dataX[,i])
  }
  data2 <- dataX                 
  dataX[is.na(dataX)] <- missing
  data2[,] <- NA                                                                   
  if(is.null(seed))
    seed <- -1
  seed <- as.integer(seed)
  dat <- .Call("RankSwap",dataX,data2,missing,TopPercent,BottomPercent,K0,R0,P,seed)$Res                                                                                                             
  data[,variables] <- dat
  invisible(data)
}
#dyn.load("D:\\Users/kowa$/Desktop/IHSN-SDC/Yichun\ 2\ February\ 2009/anonymization/src/RankSwapping/RankSwapping.dll")                                                                                       
#testdata <- read.table("D:\\Users/kowa$/Desktop/IHSN-SDC/Yichun\ 2\ February\ 2009/anonymization/test/win/Test_data.csv",sep=";",header=TRUE)                                                                    
#data <- as.matrix(data[,c("age","income","expend","savings")])                                                                                                                                               
#data2 <- data                                                                                                                                                                                                
#data2[,] <- NA                                                                                                                                                                                               
#dat <- .Call("RankSwap",data,data2,-999,5,5,-1,.95,0)$Res                                                                                                                                                    
#any(dat!=data)                                                                                                                                                                                               
#dyn.unload("D:\\Users/kowa$/Desktop/IHSN-SDC/Yichun\ 2\ February\ 2009/anonymization/src/RankSwapping/RankSwapping.dll")                                                                                     