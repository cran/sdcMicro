setGeneric('report', function(obj, outdir=getwd(),filename="SDC-Report",Title="SDC-Report",internal=TRUE) {
      standardGeneric('report')})
setMethod(f='report', signature=c('sdcMicroObj'),
		definition=function(obj, outdir=getwd(),filename="SDC-Report",Title="SDC-Report",internal=TRUE) { 
			x <- get.sdcMicroObj(obj, type="origData")
			y1 <- get.sdcMicroObj(obj, type="manipKeyVars")
			y2 <- get.sdcMicroObj(obj, type="manipNumVars")
			optionss <- get.sdcMicroObj(obj, type="options")
			
			y1cn <- get.sdcMicroObj(obj, type="keyVars")
			y1cn <- colnames(x)[y1cn]

			y2cn <- get.sdcMicroObj(obj, type="numVars")	
			y2cn <- colnames(x)[y2cn]
			
      if(length(get.sdcMicroObj(obj, type="hhId"))>0)
        hhIdcn <- colnames(x)[get.sdcMicroObj(obj, type="hhId")]
      else
        hhIdcn <- "not defined"
      if(length(get.sdcMicroObj(obj, type="strataVar"))>0)
        stratacn <- colnames(x)[get.sdcMicroObj(obj, type="strataVar")]
      else
        stratacn <- "not defined"
	
	  n <- nrow(obj@origData)
	  pCat <- length(obj@keyVars)
      wind <- get.sdcMicroObj(obj, type="weightVar")
      if(length(wind)>0)
  			weightcn <- colnames(x)[wind]
			else
        weightcn <- "not defined"
			#########################
			###   HTML START   ###### 
			HTMLStart(outdir=outdir, filename=filename, Title=Title)
			if(internal) HTML.title("SDC Report by sdcMicroGUI", HR=1) else HTML.title("Short SDC Report by sdcMicroGUI", HR=1)
#			HTML.title("Path (files):", HR=3)
#			PATHIMPORTEDDATA <- "D:/TODOINPUTPATHANDFILENAME"
      if("filename"%in%names(optionss)){
        HTML(paste("Imported data file:", optionss$filename))  
      }
#			HTML(paste("Exported data file:", PATHIMPORTEDDATA))
			
			
			HTMLhr()
			
			#############################################
			###   Begin Report on Cat Key Variables   ###
			#############################################
			HTML(paste("The data set consits of", nrow(x), "observations"))
			HTML.title("Selected (Key) Variables:", HR=3)
			maxcols <- max(c(length(y1cn), length(y2cn)))
			df <- data.frame(matrix(, ncol=maxcols, nrow=5))
			colnames(df) <- paste(1:maxcols)
			extend <- function(x, maxcols){
				if(length(x) == maxcols) return(x) else return(c(x, rep("", maxcols-length(x))))
			}
			df[1,] <- extend(y1cn, maxcols)
			df[2,] <- extend(y2cn, maxcols)
			df[3,] <- extend(weightcn, maxcols)
			df[4,] <- extend(hhIdcn, maxcols)
			df[5,] <- extend(stratacn, maxcols)
			rownames(df) <- c("Categorical", "Continuous", "weight", "hhID", "strata")
			HTML(df)

			sensiblecn <- get.sdcMicroObj(obj, "sensibleVar")
			if(!is.null(sensiblecn)){
				HTML(paste("for ldiversity the following sensible variables have been selected", 
								colnames(x)[sensiblecn]))
			}			
#			HTMLhr()
#			HTMLhr()
			
			###########################################
			###   Report on Anonymisation Methods   ###
			###########################################
#			HTML.title("Modifications:", HR=3)
			modCat <- sum(!(obj@origData[,obj@keyVars] == obj@manipKeyVars), na.rm=TRUE) > 0
			#isTRUE(sdc@origData[,sdc@keyVars] == sdc@manipKeyVars)
			modNum <- sum(!(obj@origData[,obj@numVars] == obj@manipNumVars), na.rm=TRUE) > 0
			modPram <- !is.null(obj@pram)
			modLocSupp <- !is.null(obj@localSuppression)			
			HTML(paste("Modifications on categorical key variables:",modCat))
			HTML(paste("Modifications on continuous key variables:",modNum))	
			HTML(paste("Modifications using PRAM:",modPram))
			HTML(paste("Local suppressions:",modLocSupp))			
			HTMLhr()
			HTMLhr()
			if(internal){
			#############################################
			###   DISCLOSURE RISK                     ###
			#############################################
			HTML.title("Disclosure Risk:", HR=3)
			HTMLhr()
			pram <- get.sdcMicroObj(obj, "pram")
			if(!is.null(pram)){
				HTML.title("Changes with PRAM:", HR=4)
#				HTML("you applied PRAM")			
				HTML("Number of changed categories: \n")
				for(i in 1:length(obj@keyVars)){
					s <- sum(obj@origData[,obj@keyVars[i]] != obj@manipKeyVars[,i])
					p <- round(s/nrow(obj@origData)*100,2)
					HTML(paste(colnames(obj@origData)[obj@keyVars][i]," != ", colnames(obj@origData)[obj@keyVars][i],"_pramed"," : ",s," (",p,"%)","\n",sep=""))
				}
				HTML(print(paste("Total number of changes in the categorical key variables:",
										sum(obj@origData[,obj@keyVars] != obj@manipKeyVars),
										"(",round(100*sum(obj@origData[,obj@keyVars] != obj@manipKeyVars)/(n*pCat),4)," %)"
				)))
			} else{
			HTML.title("Frequency Analysis for Categorical Key Variables:", HR=4)	
		    ## k-anonymity
			HTML(print("Number of observations violating\n"))
			HTML(print("\n -  2-anonymity:  "))
			HTML(print(paste(sum(obj@risk$individual[,2]<2),
							"(unmodified data: ",sum(obj@originalRisk$individual[,2]<2),")\n")))
			HTML(print(" -  3-anonymity:  "))
			HTML(print(paste(sum(obj@risk$individual[,2]<3),
							"(unmodified data: ",sum(obj@originalRisk$individual[,2]<3),")")))
			HTML(print("\n--------------------------\n"))  
			n <- nrow(obj@origData)
			HTML(print("\nPercentage of observations violating\n"))
			HTML(print(" -  2-anonymity:  "))
			HTML(print(paste(round(sum(obj@risk$individual[,2]<2)/n*100,2),"% ",
							"(unmodified data: ",round(sum(obj@originalRisk$individual[,2]<2)/n*100,2),"%",")\n")))
			HTML(print(" -  3-anonymity:  "))
			HTML(print(paste(round(sum(obj@risk$individual[,2]<3)/n*100,2),"% ",
							"(unmodified data: ",round(sum(obj@originalRisk$individual[,2]<3)/n*100,2),"%",")")))
			
			## individual risk:
			HTML(print("\n--------------------------\n"))  
			HTML.title("Disclosure Risk Categorical Variables:", HR=4)
			risk <- get.sdcMicroObj(obj, type="risk")
			HTML("Expected Percentage of Reidentifications:")
			HTML(paste(round(risk$global$risk_pct, 4), 
							"%  ( ~ ", round(risk$global$risk*nrow(x)), "observations )" ))
			## TODO: save risk of original data and not renewly calculate it:
     		if(weightcn=="not defined") weightcn <- NULL
			ro <- measure_risk(x, keyVars=y1cn, w=weightcn)$global_risk_pct
			HTML(paste("( unmodified data:", round(ro,4)," % )" ))
			
#			HTML(paste("Information on k-anonymity:"))
#			y1w <- cbind(y1, w=x[,wind])
#			fr <- freqCalc(y1w, keyVars=1:(ncol(y1w)-1), w=ncol(y1w))
#	        HTML(fr)
			
			HTML(paste("10 combinations of categories with highest risk:"))
#			rownames(risk$individual) <- 1:nrow(x)
#			so <- sort(risk$individual[,"risk"], index.return=TRUE)$ix[1:10]
#			HTML(cbind(y1[so,], risk$individual[so,]))
			or <- cbind(y1, risk$individual)
			index <- apply(y1, 1, paste, collapse="")
      		or <- or[!duplicated(index),]
			or <- or[order(or$risk,decreasing = TRUE),]
			HTML(or[1:10,])
			}
			
			if("hier_risk_ER"%in%names(obj@risk$global)){
				if(!is.na(obj@risk$global$hier_risk_ER)){
					HTML(print("--------------------------\n"))
					HTML(print("Hierarchical risk \n"))
#					HTML(print("--------------------------\n"))
					HTML(print(paste("Expected no. of re-identifications:\n",
							round(obj@risk$global$hier_risk_ER,2),"")))
					HTML(print(paste("[",round(obj@risk$global$hier_risk_pct,2),"%]  (orig:", 
							round(obj@originalRisk$global$hier_risk_ER,2), 
							"[",round(obj@originalRisk$global$hier_risk_pct,2),"%])\n")))
				}else{
					HTML(print("--------------------------\n"))
					HTML(print("Hierarchical risk not available\n"))
					HTML(print("--------------------------\n"))
				}
			}
			
			HTMLhr()
			
#			if(!is.null(pram)){
#				HTML("Information on Pramed Variables:")
#				HTML(get.sdcMicroObj(obj, "pram"))
#			}
			
			
			#			HTML("Allowing 1 % risky observations")
	
#			HTMLhr()
			
			## disclosure risk on continuous key variables
			HTML.title("Disclosure Risk Continuous Scaled Variables:", HR=4)
			risknum <- get.sdcMicroObj(obj, "risk")$numeric
			if(!is.null(get.sdcMicroObj(obj, "manipNumVars"))){
				HTML(paste("Distance-based Disclosure Risk for Continuous Key Variables:"))
#				HTML(paste("Percent of Observations with high risk:",round(risknum*100,3),"% ( ~", 
#								round(risknum*nrow(x)), " observations )" ))
				HTML(paste("Disclosure Risk is between: \n [0% ; ", 
						round(100*obj@risk$numeric,2), "%] (current)\n 
								(original data: ~", 100, "%) \n",sep=""))
			}
			HTMLhr()
			HTMLhr()
			
			##################
			HTML.title("Data Utility:", HR=3)
			HTMLhr()
			HTML.title("Frequencies Categorical Key Variables:", HR=4)
			ind <- 1
			for(i in y1cn){
				maxcols <- length(unique(x[,i])) #max(apply(x[, y1cn], 2, function(x) length(unique(x))))
				df <- data.frame(matrix(, ncol=maxcols, nrow=4))#length(y1cn)))
				colnames(df) <- paste(1:maxcols)
				df[1,] <- levels(factor(x[,i]))
				df[2,] <- table(factor(x[,i]))
				df[4,] <- extend(table(factor(y1[,ind])), maxcols)
				df[3,] <- extend(levels(factor(y1[,ind])), maxcols)
#				colnames(df) <- levels(factor(x[,i]))
				rownames(df) <- c("categories1","orig","categories2","recoded")
				ww <- which(colnames(x) %in% i)
				HTML(colnames(x)[ww])
				HTML(df)
				ind <- ind +1
			}
				
			##################
			HTML.title("Local Suppressions", HR=5)
			HTML("Number of local suppressions:")
			locsupps <- as.numeric(unlist(get.sdcMicroObj(obj, "localSuppression")))
			if(length(locsupps) == 0){ locsupps <- rep(0,length(y1cn))}
			df <- data.frame(matrix(,ncol=length(y1cn), nrow=2))
			colnames(df) <- y1cn
			rownames(df) <- c("absolute","relative (in %)")
			df[1,] <- paste(as.integer(locsupps))
			df[2,] <- round(locsupps/nrow(x)*100,2)
			HTML(df)
			
			if(ncol(y2)>0){
        	HTMLhr()
			  HTML.title("Data Utility of Continuous Scaled Key Variables:", HR=4)
			  HTML("Univariate Summary:")
			  s <- apply(x[,y2cn], 2, summary, na.rm=TRUE) 
#			  HTML(s) 	
#			  HTML("Univariate Summary for Perturbed Variables:")
			  ss <- apply(y2, 2, function(x) round(summary(x, na.rm=TRUE),1)) 
			  colnames(ss) <- paste(colnames(ss),"_m")
#			  HTML(ss)
			  pNum <- length(obj@numVars)
			  xx <- cbind(s,ss)
			  if(pNum > 1){
			  	SEQ <- SEQinit <- c(1,pNum+1)
				for(i in 1:(pNum-1)){
				  SEQ <- c(SEQ, SEQinit+i)
				}
		  	  }
			  xx <- xx[, SEQ]
              HTML(xx)
			  HTML(paste("- Information Loss:\n    IL1: ", 
					  round(obj@utility$il1,2))) 
	          HTML(paste("Difference Eigenvalues: ",round(obj@utility$eigen*100,2)," %",
					  "\n\n (orig: Information Loss: 0) \n",sep=""))
#	HTML("Relative Differences (in %):")
#	s <- s %/% ss
#	options(scipen=999)
#	HTML(round(s*100,1)) 
#	options(scipen=0)
			  mi <- min(x[,y2cn],y2)
			  ma <- max(x[,y2cn],y2)
			  b <- boxplot(x[,y2cn], boxwex=0.1, main="univariate comparison original vs. perturbed data", 
					  ylim=c(mi,ma))
			  boxplot(y2, add=TRUE, at=1:ncol(y2)+0.2, boxwex=0.1, col="lightgrey", xaxt="n", xlab="")	
			  legend("topright", legend=c("orig","pert"), pch=15, col=c("white","lightgrey"))	
			  legend("topright", legend=c("orig","pert"), pch=22)
			  HTMLplot(Height=400)	
			}
      if("cmd"%in%names(optionss)){
        HTMLhr()
        HTML.title("R-Code:", HR=4)
        for(i in 1:length(obj@options$cmd))
        HTML(obj@options$cmd[[i]])
      }
	  }
      HTMLhr()
      HTML.title("Session Info:", HR=3)
      HTML(sessionInfo())
      HTML("sdcMicro and sdcMicroGUI package developed and maintained by")
      HTML("<a href=http://www.data-analysis.at>http://www.data-analysis.at</a>") 
      HTML("(data-analysis OG, <a href=mailto:office@data-analysis.at>office@data-analysis.at</a>)")
      
			HTMLStop()	
#			res <- reportWORK(x, y, y1cn, y2cn, weightcn, hhIdcn,
#					stratacn)

})


#setMethod(f='report', signature=c("data.frame"),
#		definition=function(obj, ...) { 
#			reportWORK(obj,...)
#		})
#setMethod(f='report', signature=c("matrix"),
#		definition=function(obj, ...) { 
#			reportWORK(obj,...)
#		})
#
#reportWORK <- function(x, y, y1cn, y2cn, 
#		  weightcn, hhIdcn, stratacn,
#					outdir=".", filename="SDC-Report", version="short"){
#	HTMLStart(outdir=outdir, filename=filename, Title="SDC-Report")
##	HTMLChangeCSS("gridR2HTML")
#	
#	HTML.title("SDC Report by sdcMicroGUI", HR=1)
#	
#	HTML.title("Path (files):", HR=3)
#	PATHIMPORTEDDATA <- "D:/TODO"
#	HTML(paste("Imported data file:", PATHIMPORTEDDATA))
#	HTML(paste("Exported data file:", PATHIMPORTEDDATA))
#
#	HTML.title("Selected (Key) Variables:", HR=3)
#	maxcols <- max(c(length(y1cn), length(y2cn)))
#	df <- data.frame(matrix(, ncol=maxcols, nrow=5))
#	colnames(df) <- paste(1:maxcols)
#	extend <- function(x, maxcols){
#		if(length(x) == maxcols) return(x) else return(c(x, rep("", maxcols-length(x))))
#	}
#	df[1,] <- extend(y1cn, maxcols)
#	df[2,] <- extend(y2cn, maxcols)
#	df[3,] <- extend(weightcn, maxcols)
#	df[4,] <- extend(hhIdcn, maxcols)
#	df[5,] <- extend(stratacn, maxcols)
#	rownames(df) <- c("Categorical", "Continuous", "weight", "hhID", "strata")
#	HTML(df)
#	
#	HTML.title("Applied Anonymization Methods:", HR=3)
#	HTML("TODO - List Methods that have been applied, from R code that is generated.")
#	
#	HTMLhr()
#	
#	HTML.title("Disclosure Risk:", HR=3)
#	HTML.title("Disclosure Risk Categorical Variables: (not applicable for PRAM)", HR=4)
#	HTML("TODO - Calculate DR")
#	
#	HTML.title("Disclosure Risk Continuous Scaled Variables:", HR=4)
#	HTML("TODO - Calculate DR")
#	
#	HTMLhr()
#	
#	HTML.title("Data Utility:", HR=3)
#	HTML.title("Data Utility Categorical Variables:", HR=4)
#	HTML("TODO - Calculate DR")
#	HTML.title("Data Utility Continuous Scaled Variables:", HR=4)
#	HTML("Univariate Summary for original variables:")
#	s <- apply(x[,y2cn], 2, summary, na.rm=TRUE) 
#	HTML(s) 	
#	HTML("Univariate Summary for Perturbed Variables:")
#	ss <- apply(y, 2, function(x) round(summary(x, na.rm=TRUE),1)) 
#	HTML(ss) 
##	HTML("Relative Differences (in %):")
##	s <- s %/% ss
##	options(scipen=999)
##	HTML(round(s*100,1)) 
##	options(scipen=0)
#    mi <- min(x[,y2cn],y)
#	ma <- max(x[,y2cn],y)
#    b <- boxplot(x[,y2cn], boxwex=0.1, main="univariate comparison original vs. perturbed data", 
#			ylim=c(mi,ma))
#	boxplot(y, add=TRUE, at=1:ncol(y)+0.2, boxwex=0.1, col="lightgrey", xaxt="n", xlab="")	
#	legend("topright", legend=c("orig","pert"), pch=15, col=c("white","lightgrey"))	
#	legend("topright", legend=c("orig","pert"), pch=22)
#	HTMLplot(Height=400)	
#
#	HTMLhr()
#	# classAgreement(table(a,b))
##	mosaicplot(~ Class + Survived, Titanic)
##	HTMLplot(Height=400)
#	HTMLStop()	
#}
