measure_risk <- function(data,keyVars,w=NULL,missing=-999,hid=NULL,max_global_risk=.01,force_hier=FALSE){
  variables <- keyVars
  weight_variable <- w
  if((is.null(variables)||!variables%in%colnames(data))&&is.character(variables))
    stop("Please define valid key variables")
  else if(is.numeric(variables)){
    if(all(variables%in%c(1:ncol(data))))
      variables <- colnames(data)[variables]  
    else
      stop("Please define valid key variables")
  }
  if(!is.null(weight_variable)){
    if(!weight_variable%in%colnames(data)&&is.character(weight_variable))
      stop("Weight variable not found!")
    else if(is.numeric(weight_variable)){
      if(weight_variable%in%c(1:ncol(data)))
        weight_variable <- colnames(data)[weight_variable]
      else
        stop("Weight variable not found!")
    }
  }  
  
  weighted <- 0
  if(!is.null(weight_variable))
    weighted <- 1
  n_key_vars <- length(variables)
  dataX <- data[,c(variables),drop=FALSE]
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i]))
      dataX[,i] <- as.numeric(dataX[,i])
  }
  if(weighted==1)
    dataX <-cbind(dataX,data[,weight_variable])
  dataX <- as.matrix(dataX)
  ind <- do.call(order,data.frame(dataX))
  dataX <- dataX[ind,,drop=FALSE]
  ind <- order(c(1:nrow(dataX))[ind])
  res <- .Call("measure_risk",dataX,weighted,n_key_vars,2,-99,missing)
  res$Res <- res$Res[ind,]
  res <- res[names(res)!="Mat_Risk"]
  colnames(res$Res) <- c("group_count","risk","group_size")
  ind <- order(res$Res[,2],decreasing =TRUE)
  if(max_global_risk>=1 || max_global_risk<=0){
    stop("max_global_risk argument must be between 0 and 1!")
  }
  resth <- .Call("measure_threshold",res$Res[ind,2],max_global_risk)
  if(is.na(resth$global_threshold_unsafe)||is.na(resth$global_threshold_safe)||is.na(resth$global_threshold)){
    res[["global_threshold"]] <- NA
  }else if(resth$global_threshold_unsafe==resth$global_threshold&&resth$global_threshold_safe==resth$global_threshold)
    res[["global_threshold"]] <- NA
  else
    res[["global_threshold"]] <- resth$global_threshold
  res[["max_global_risk"]] <- max_global_risk
  class(res) <- "measure_risk"
  if(!is.null(hid)){
    ind <- order(data[,hid])
    dataX <- cbind(data[,hid],res$Res[,2])[ind,]
    ind <- order(c(1:nrow(dataX))[ind])
    for(i in 1:ncol(dataX)){
      if(!is.numeric(dataX[,i]))
        dataX[,i] <- as.numeric(dataX[,i])
    }
    dataX <- as.matrix(dataX)
    maxHH <- max(table(dataX[,1]))
    if(maxHH>10&&!force_hier){
      warning("The households are to large for a fast computation of the hierachical risk.\n
      (Use the parameter forceHier to perform the computation anyway)")
      res$Res <- cbind(res$Res,rep(NA,nrow(res$Res)))
      res[["hier_risk_ER"]] <- NA
      res[["hier_risk"]] <- NA
      res[["hier_risk_pct"]] <- NA
    }else{
      resh <- .Call("measure_hierachical",dataX)
      resh$Res <- resh$Res[ind]
      res$Res <- cbind(res$Res,resh$Res)
      res[["hier_risk_ER"]] <- resh[["hier_risk_ER"]]
      res[["hier_risk"]] <- resh[["hier_risk"]]
      res[["hier_risk_pct"]] <- resh[["hier_risk_pct"]]  
    }
    
    colnames(res$Res) <- c("group_count","risk","group_size","hier_risk");
  }
  invisible(res)
}
ldiversity <- function(data,keyVars,missing=-999,l_recurs_c=2,ldiv_index=NULL){
  variables <- keyVars
  if((is.null(variables)||!variables%in%colnames(data))&&is.character(variables))
    stop("Please define valid key variables")
  else if(is.numeric(variables)){
    if(all(variables%in%c(1:ncol(data))))
      variables <- colnames(data)[variables]  
    else
      stop("Please define valid key variables")
  }
  if(!is.null(ldiv_index)){
    if(is.numeric(ldiv_index)){
      ldiv_var <- colnames(data)[ldiv_index]
      ldiv_index <- length(variables)+1:length(ldiv_index)
    }else if(is.character(ldiv_index)){
      ldiv_var <- ldiv_index
      ldiv_index <- length(variables)+1:length(ldiv_index)
    }
    if(any(ldiv_var%in%variables))
      stop("Sensitivity variable should not be a keyVariable")
  }else
    ldiv_var <- character(0)
  
  n_key_vars <- length(variables)
  dataX <- data[,c(variables,ldiv_var),drop=FALSE]
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i]))
      dataX[,i] <- as.numeric(dataX[,i])
  }
  dataX <- as.matrix(dataX)
  ind <- do.call(order,data.frame(dataX))
  dataX <- dataX[ind,,drop=FALSE]
  ind <- order(c(1:nrow(dataX))[ind])
  if(is.null(ldiv_index))
    ldiv_index=-99
  if(length(ldiv_index)>5)
    stop("Maximal number of sensitivity variables is 5")
  res <- .Call("measure_risk",dataX,0,n_key_vars,l_recurs_c,ldiv_index,missing)
  res$Res <- res$Res[ind,]
  if(all(ldiv_index!=-99)){
    res$Mat_Risk <- res$Mat_Risk[ind,]
    names(res)[names(res)=="Mat_Risk"] <- "ldiversity"
    colnames(res$ldiversity) <- c(paste(rep(ldiv_var,each=3),rep(c("Distinct_Ldiversity","Entropy_Ldiversity","Recursive_Ldiversity"),length(ldiv_index)),sep="_"),
        "MultiEntropy_Ldiversity","MultiRecursive_Ldiversity")
  }else{
    res <- res[names(res)!="Mat_Risk"]
  }
  colnames(res$Res) <- c("group_count","risk","group_size")
  ind <- order(res$Res[,2],decreasing =TRUE)
  res <- res$ldiversity
  class(res) <- "ldiversity"
  invisible(res)
}
print.measure_risk <- function(x,...){
  cat("\n")
  cat("--------------------------\n")
  s <- sum(x$Res[,2] > median(x$Res[,2])+2*mad(x$Res[,2]))
  cat(paste(s,"obs. with much higher risk than the main part\n"))
  cat("Expected no. of re-identifications:\n",round(x$global_risk_ER,2),"")
  cat("(",round(x$global_risk_pct,2),"%)\n")
  if(is.na(x$global_threshold))
    x$global_threshold <- Inf
  cat("Threshold:",round(x$global_threshold,2),"\n (for maximal global risk",round(x$max_global_risk,2),")\n")
  cat("--------------------------\n")
  if("hier_risk_ER"%in%names(x)){
    if(!is.na(x$hier_risk_ER)){
      cat("--------------------------\n")
      cat("Hierarchical risk \n")
      cat("--------------------------\n")
      cat("Expected no. of re-identifications:\n",x$hier_risk_ER,"")
      cat("(",x$hier_risk_pct,"% )\n")
      print(round(summary(x$Res[,4]),2))
    }else{
      cat("--------------------------\n")
      cat("Hierarchical risk not available\n")
      cat("--------------------------\n")
    }
  }
}
print.ldiversity <- function(x,...){
  cat("--------------------------\n")
  cat("L-Diversity Measures \n")
  cat("--------------------------\n")
  print(summary(x[,-c(ncol(x)-1,ncol(x))]))
}
