#should be in zzz.R
#require(gWidgetsRGtk2)
#sdcGUIenv <- new.env() starts in gui function

# just for test case while not in sdcMicro package
#require(sdcMicro)

sdcGUI <- function() {
	
	sdcGUIenv <- new.env()
  ## utility functions
  # envionment with get and set functions
	# not used, cause it ignores new env ... to use, remove ...x
  sdcGUIenvx <- function() {
    pos <- match("sdcGUIenv", search())
    if(is.na(pos)) {
      sdcGUIenv <- list()
      attach(sdcGUIenv, pos=length(search())-1)
      rm(sdcGUIenv)
      pos <- match("sdcGUIenv", search())
    }
    return(pos.to.env(pos))
  }
	
  putd <- function(x, value) {
    assign(x, value, envir=sdcGUIenv) # add () to sdcGUIenv
  }
  
  getd <- function(x, mode="any") {
    get(x, envir=sdcGUIenv, mode=mode, inherits=FALSE) # add () to sdcGUIenv
  }
  
  existd <- function(x, mode="any") {
    exists(x, envir=sdcGUIenv, mode=mode, inherits=FALSE) # add () to sdcGUIenv
  }
  
  # ActiveDataSet
  ActiveDataSet <- function(name) {
    if( missing(name) ) {
      getd("activeDataSet")
    } else {
      if( is.matrix(get(name)) ) {
        putd("activeDataSet", data.frame(get(name), stringsAsFactors=FALSE))
      } else {
        putd("activeDataSet", get(name))
      }
      putd("dataSetName", name)
    }
  }
	
	# update ActiveDataSet
	updateActiveDataSet <- function(x, ...) {
		if( is.matrix(x) ) {
			factorizeVars = FALSE
			x <- data.frame(x, stringsAsFactors=FALSE)
			xtmp <- getd("numIndex")
			xtmp <- c(xtmp, getd("wIndex"))
			for( i in 1:length(xtmp) ) {
				try( x[,xtmp[i]] <- as.numeric(x[,xtmp[i]]), silent=TRUE )
			}
			if( factorizeVars ) {
				xtmp <- getd("keyIndex")
				for( i in 1:length(xtmp) ) {
					try( x[,xtmp[i]] <- as.factor(x[,xtmp[i]]), silent=TRUE )
				}
			}
		}
		putd("activeDataSet", x)
		freqCalcIndivRisk()
	}
	
	# Script
  #
	Script <- function(name, ...) {
		if( missing(name) ) {
			getd("activeScript")
		} else { 
			putd("activeScript", name)
		}
	}
	
	Script.new <- function(...) {
		xtmp <- list(cmd=c())
		putd("activeScript", xtmp)
	}
	
	Script.add <- function(cmd, ...) {
		xtmp <- Script()
		xtmp$cmd[length(xtmp$cmd)+1] = cmd
		Script(xtmp)
	}
	
	Script.run <- function(xscr, ...) {
		if( existd("activeDataSet") ) {
			if( missing(xscr) ) {
				xcmd <- Script()
				xcmd <- xcmd$cmd
			} else {
				xcmd <- xscr
			}
			xprogress = gwindow("please wait", width=180, height=40, parent=window)
			glabel("... script running ...", cont=xprogress)
			for( i in 1:length(xcmd) ) {
				ytmp <- xcmd[i]
				eval(parse(text=ytmp), envir=sdcGUIenv)
				#xtmp <- function() { eval(parse(text=ytmp)) }
				#do.call(xtmp, list(), envir=sdcGUIenv)
			}
			dispose(xprogress)
		} else {
			gmessage("Run not possible, because no active data set found.", title="Attention", icon="error", parent=window)
		}
	}
	
	parseVar <- function(x, ...) {
		s <- "c("
		for ( i in 1:length(x) ) {
			s <- paste(s, x[i])
			if (i < length(x)) {
				s <- paste(s, ",")
			}
		}
		s <- paste(s, ")")
		return(s)
	}
	
	parseVarStr <- function(x, ...) {
		s <- "c("
		for ( i in 1:length(x) ) {
			s <- paste(s, "'", x[i], "'", sep="")
			if (i < length(x)) {
				s <- paste(s, ",", sep="")
			}
		}
		s <- paste(s, ")", sep="")
		return(s)
	}
  
  # getIndex to get the col index of categorical, numerical and weight vars
  getIndex <- function(x, ...) {
    ads <- names(ActiveDataSet())
    ord <- c()
    for( i in 1:length(x) ) {
      for( j in 1:length(ads) ) {
        if( x[i]==ads[j] ) {
          ord <- c(ord, j)
        }
      }
    }
    return(ord)
  }

  # function for button ir_button (plotIndivRisk)
  # indivRiskGroup function
  # x ... object of class indivRisk
  # y ... object of class freqCalc
  plotIndivRisk <- function(y, x, ...) {
    method = "histogram"
    mu <- 0.0025
    sd <- 0.5
    s2 <- 0.5
    mu.old <- mu
    sd.old <- sd
    s2.old <- s2
    maxsd <- 1/length(x$rk) * (sum(x$fk * x$rk)) *100
    n1 <- x$knames[1]     ## next, the plot of column names of keys
    if( length(x$knames) > 1 ){
      for(i in 2:length(x$knames)){
        n1 <- paste(n1, "x", x$knames[i])
      }
    }
    norm.refresh <- function(...) {
        method = method
        mu <- as.numeric(evalq(svalue(smu)))
        sd <- as.numeric(evalq(svalue(ssd)))
        s2 <- as.numeric(evalq(svalue(ss2)))
        if (mu != mu.old) {
            s2 <- round(length(which(x$rk > mu)))
            sd <- 1/length(x$rk) * (sum(x$fk[x$rk < mu] * x$rk[x$rk < mu]) + mu*sum(x$fk[x$rk>mu])) * 100
            try(svalue(ssd)<-sd)
            try(svalue(ss2)<-s2)
            sd.old <<- sd
            s2.old <<- s2
        }
        if (sd != sd.old) {
            sd <- as.numeric(evalq(tclvalue(s2), env = slider.env))
            s2 <- length(which(x$rk > mu))
            try(svalue(ssd)<-sd)
            try(svalue(ss2)<-s2)
            sd.old <<- sd
            s2.old <<- s2
        }
        if (s2 != s2.old) {
            s2 <- as.numeric(evalq(tclvalue(s2), env = slider.env))
            sd <- 1/length(x$rk) * (sum(x$fk * x$rk) + 0.02*sum(x$fk))
            try(svalue(ssd)<-sd)
            sd.old <<- sd
            s2.old <<- length(which(x$rk > mu))
        }
        if( method == "histogram" ){
          hist(x$rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
          abline(v=mu, col="blue", lwd=2)
        }
        if( method == "ecdf" ){
          plot(ecdf(x$rk), main="ecdf of individual risk", xlab="individual risk")
                    abline(v=mu, col="blue", lwd=2)
        }
    }
    plot1 <- function(method){
      if( method == "histogram" ){
				hist(x$rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
        abline(v=mu, col="blue", lwd=2)
      }
      if( method == "ecdf" ){
        plot(ecdf(x$rk), main="ecdf of individual risk", xlab="individual risk")
        abline(v=as.numeric(evalq(svalue(smu))), col="blue", lwd=2)
      }
    }
    win = gwindow("Individual Risk Adjustments", parent=window)
    mainGroup1 = ggroup(cont=win, horizontal=FALSE)
    method = "histogram"
    sliderGroup = ggroup(cont=mainGroup1, horizontal=FALSE)
    tmp = gframe("Individual Risk Threshold", cont=sliderGroup)
    smu = gslider(from=0, to=max(x$rk), by=0.001, value=mu, handler=norm.refresh)
    add(tmp, smu, expand=TRUE)
    tmp = gframe("Re-identification Rate", cont=sliderGroup)
    ssd = gslider(from=0, to=maxsd, by=0.01, value=sd, handler=norm.refresh)
    add(tmp, ssd, expand=TRUE)
    tmp = gframe("Unsafe Records", cont=sliderGroup)
    ss2 = gslider(from=0, to=length(x$rk), by=1, value=s2, handler=norm.refresh)
    add(tmp, ss2, expand=TRUE)
    gbutton("Show ecdf", cont=mainGroup1, handler=function(x,...) plot1("ecdf"))
    add(mainGroup1, ggraphics())
    if( method == "histogram" ){
			try(hist(x$rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow"), silent=TRUE)
      try(abline(v=mu, col="blue", lwd=2), silent=TRUE)
    }
  }
  
  # FreqCalc and indivRisk calculation - freqCalc()
	#                                    - indivRisk()
	# TODO: not needed - save freqCalcIndivRisk for script/history
	freqCalcIndivRisk <- function(...) {
		xprogressFQ = gwindow("please wait", width=250, height=140, parent=window)
		glabel("... calculating ...", cont=xprogressFQ)
    # freqCalc
    f1 <- freqCalc(ActiveDataSet(), keyVars=getd("keyIndex"), w=getd("wIndex"))
		#-- Start - print.freqCalc
		tmp <- capture.output(print.freqCalc(f1))
		svalue(fc_print) <- tmp[1]
		if( length(tmp)> 1 ) {
			for( i in 2:length(tmp) ) {
				insert(fc_print, tmp[i])
			}
		}
    #-- End - print.freqCalc
		#-- Start - summary.freqCalc
		tmp <- capture.output(summary.freqCalc(f1))
		svalue(fc_summary) <- tmp[1]
		if( length(tmp)> 1 ) {
			for( i in 2:length(tmp) ) {
				if( !tmp[i] == "" ) {
					insert(fc_summary, tmp[i])
				}
			}
		}
    #-- End - summary.freqCalc
    putd("freqCalc", f1)
    # indivRisk
    i1 <- indivRisk(f1)
		#-- Start - print.indivRisk
		tmp <- capture.output(print.indivRisk(i1))
		svalue(ir_print) <- tmp[1]
		if( length(tmp)> 1 ) {
			for( i in 2:length(tmp) ) {
				insert(ir_print, tmp[i])
			}
		}
    #-- End - print.indivRisk
    putd("indivRisk", i1)
		dispose(xprogressFQ)
  }
	
  # localSupp1_tmp - localSupp()
	# TODO: done - save localSupp for script/history
  localSupp1_tmp <- function(keyVar, threshold, redo=FALSE) {
		if( !redo ) {
			Script.add(paste("localSupp1_tmp(", parseVarStr(keyVar), ", ", parseVar(threshold), ", redo=TRUE)", sep=""))
		}
    x <- getd("freqCalc")
    keyVar <- getIndex(keyVar)
    indivRisk <- getd("indivRisk")$rk
    threshold <- threshold
    l1 <- localSupp(x, keyVar, indivRisk, threshold)
    putd("l1", l1)
    updateActiveDataSet(l1$freqCalc)
    #freqCalcIndivRisk()
  }
  # localSupp2_tmp - localSupp2()
	# TODO: done - save localSupp2 for script/history
  localSupp2_tmp <- function(k, importance, redo=FALSE) {
		if( !redo ) {
			Script.add(paste("localSupp2_tmp(", parseVar(k), ", ", parseVar(importance), ", redo=TRUE)", sep=""))
		}
    x <- ActiveDataSet()
    keyVars <- getd("keyIndex")
    w <- getd("wIndex")
    k <- k
    importance <- importance
		l1 <- localSupp2(x=x, keyVars=keyVars, w=w, k=k, importance=importance)
    putd("l1", l1)
    updateActiveDataSet(l1$xAnon)
    #freqCalcIndivRisk()
  }
  # localSupp3_tmp - localSupp2Wrapper()
	# TODO: done - save localSupp2Wrapper for script/history
  localSupp3_tmp <- function(k, importance, redo=FALSE) {
		if( !redo ) {
			Script.add(paste("localSupp3_tmp(", parseVar(k), ", ", parseVar(importance), ", redo=TRUE)", sep=""))
			xprogress = gwindow("please wait", width=180, height=40)
			glabel("... script running ...", cont=xprogress)
		}
    x <- ActiveDataSet()
    keyVars <- getd("keyIndex")
    w <- getd("wIndex")
    k <- k
    importance <- importance
    l1 <- localSupp2Wrapper(x=x, keyVars=keyVars, w=w, kAnon=k, importance=importance)
    putd("l1", l1)
    updateActiveDataSet(l1$xAnon)
		if( !redo ) {
			dispose(xprogress)
		}
    #freqCalcIndivRisk()
  }

  # function for ls1_button (local Suppression)
  # localSuppGroup function
  ls1 <- function(...) {
    ls1_pars = ggroup(horizontal=FALSE)
    tmp = gframe("Choose keyVar", cont=ls1_pars)
    x = gdroplist(sort(getd("keyVars")))
    add(tmp, x, expand=TRUE)
    tmp = gframe("Threshold", cont=ls1_pars)
    y = gslider(from=0, to=1, by=0.01, value=0.15)
    add(tmp, y, expand=TRUE)
    gbasicdialog(title="Choose parameters", parent=window,
          widget=ls1_pars,
          handler=function(h,...) localSupp1_tmp(svalue(x), svalue(y)) )
  }
  
  # function for ls2_button (local Suppression 2)
  # localSuppGroup function
  ls2 <- function(...) {
    ls2_pars = ggroup(horizontal=FALSE)
    tmp = gframe("k-anonymity parameter", cont=ls2_pars)
    y_tmp <- names(ActiveDataSet())[sort(getd("keyIndex"))]
    x = gslider(2, 12, 2, by=2)
    add(tmp, x, expand=TRUE)
    y <- list()
    for( i in 1:length(y_tmp) ) {
      y[i] <- gslider(from=0, to=1, by=0.01, value=1)
    }
    tmp = gframe("importance of keyVars", cont=ls2_pars, horizontal=FALSE)
    for( i in 1:length(y_tmp) ) {
      tmpg = ggroup(cont=tmp)
      tmpt = glabel(y_tmp[i])
      add(tmpg, tmpt)
      add(tmpg, y[[i]], expand=TRUE)
    }
    add(tmp, x, expand=TRUE)
    ls3_window = gbasicdialog(title="Choose Parameters", parent=window,
              widget=ls2_pars,
              handler=function(h,...) {
                        importance <- c()
                        k <- svalue(x)
                        for(i in 1:length(y_tmp) ) {
                          importance <- c(importance, svalue(y[[i]]))
                        }
                        localSupp2_tmp(k, importance)
                      } )
  }
  
  # fuction for ls3_button (local Suppression 2 Wrapper)
  # localSuppGroup function
  ls3 <- function(...) {
    ls3_pars = ggroup(horizontal=FALSE)
    tmp = gframe("k-Anonymity parameter", cont=ls3_pars)
    y_tmp <- names(ActiveDataSet())[sort(getd("keyIndex"))]
    x = gslider(2, 12, 2, by=2)
    add(tmp, x, expand=TRUE)
    y <- list()
    for( i in 1:length(y_tmp) ) {
      y[i] <- gslider(from=0, to=1, by=0.01, value=1)
    }
    tmp = gframe("Importance of keyVars", cont=ls3_pars, horizontal=FALSE)
    for( i in 1:length(y_tmp) ) {
      tmpg = ggroup(cont=tmp)
      tmpt = glabel(y_tmp[i])
      add(tmpg, tmpt)
      add(tmpg, y[[i]], expand=TRUE)
    }
    add(tmp, x, expand=TRUE)
    gbasicdialog(title="Choose parameters", parent=window,
              widget=ls3_pars,
              handler=function(h,...) {
                        importance <- c()
                        k <- svalue(x)
                        for(i in 1:length(y_tmp) ) {
                          importance <- c(importance, svalue(y[[i]]))
                        }
                        localSupp3_tmp(k, importance)
                      } )
  }
	
	# microaggregation_tmp - microaggregation()
	# TODO: done - save microaggregation for script/history
	microaggregation_tmp <- function(aggr, method, vars, redo=FALSE) {
		if( !redo ) {
			Script.add(paste("microaggregation_tmp(", parseVar(aggr), ", ",
						parseVarStr(method), ", ", parseVarStr(vars), ",redo=TRUE)", sep=""))
			putd("oldCols", ActiveDataSet()[,vars])
		}
		xtmp <- ActiveDataSet()
		xtmp[,vars] <- microaggregation(xtmp[,vars], method=method, aggr=aggr)$blowxm
		updateActiveDataSet(xtmp)
		#freqCalcIndivRisk()
		if( !redo ) {
			putd("newCols", ActiveDataSet()[,vars])
			nm_risk_print_function()
		}
	}
	
	# function for nm_button2
	# globalRecodeGroup-numericalMethods function
	nm2 <- function(...) {
		lTOr <- function(h, ...) {
			if( length(h)>0 ) {
				if( length(selTab[])==1 ) {
					if( is.na(selTab[]) ) {
						selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
					} else {
						selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
					}
				} else {
					selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
				}
				if( length(h)==length(varTab[]) ) {
					varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
				} else {
					xtmp <- c()
					for( i in 1:length(varTab[]) ) {
						for( j in 1:length(h) ) {
							if( varTab[][i]==h[j] ) {
								xtmp <- c(xtmp, i)
							}
						}
					}
					varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
				}
			}
		}
		rTOl <- function(h, ...) {
			if( length(h)>0 ) {
				if( length(varTab[])==1 ) {
					if( is.na(varTab[]) ) {
						varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
					} else {
						varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
					}
				} else {
					varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
				}
				if( length(h)==length(selTab[]) ) {
					selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
				} else {
					xtmp <- c()
					for( i in 1:length(selTab[]) ) {
						for( j in 1:length(h) ) {
							if( selTab[][i]==h[j] ) {
								xtmp <- c(xtmp, i)
							}
						}
					}
					selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
				}
			}
		}
		nm2_window = gwindow("Microaggregation", width=230, parent=window)
		nm2_windowGroup = ggroup(cont=nm2_window, horizontal=FALSE)
		tmp = gframe("Aggregation level", cont=nm2_windowGroup, horizontal=FALSE)
		ntmp = ggroup(cont=tmp)
		aggrSel = gslider(from=2, to=20, by=1)
		add(ntmp, aggrSel, expand=TRUE)
		tmp = gframe("Method", cont=nm2_windowGroup, horizontal=FALSE)
		methodSel = gdroplist(c("rmd", "pca", "clustppca", "influence"))
		add(tmp, methodSel)
		tmp = gframe("Variable selection", cont=nm2_windowGroup)
		numVars <- c()
		xtmp <- ActiveDataSet()
		# just use all numerical vars
		#for( i in 1:dim(xtmp)[2] ) {
		#	if( is.numeric(xtmp[,i]) & names(xtmp)[i] != getd("wVars") ) {
		#		numVars <- c(numVars, names(xtmp)[i])
		#	}
		#}
		numVars <- getd("numVars")
		varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
		size(varTab) <- c(120,200)
		add(tmp, varTab)
		btmp = ggroup(cont=tmp, horizontal=FALSE)
		addSpring(btmp)
		gbutton(">>", cont=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
		gbutton("<<", cont=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
		addSpring(btmp)
		selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
		size(selTab) <- c(120,200)
		add(tmp, selTab)
		gseparator(cont=nm2_windowGroup)
		nm2_windowButtonGroup = ggroup(cont=nm2_windowGroup)
		addSpring(nm2_windowButtonGroup)
		gbutton("Ok", cont=nm2_windowButtonGroup,
				handler=function(h,...) {
					aggrVal <- as.numeric(svalue(aggrSel))
					if( length(selTab[])<2 | any(is.na(selTab[])) ) {
						gmessage("You need to select at least 2 variables!", title="Information", icon="info", parent=nm2_window)
					} else {
						microaggregation_tmp(aggrVal, svalue(methodSel), selTab[])
						dispose(nm2_window)
					}
				})
		gbutton("Cancel", cont=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
	}
	
	# addNoise_tmp - addNoise()
	# TODO: done - save addNoise for script/history
	addNoise_tmp <- function(noise, method, vars, redo=FALSE) {
		if( !redo ) {
			Script.add(paste("addNoise_tmp(", parseVar(noise), ", ",
						parseVarStr(method), ", ", parseVarStr(vars), ", redo=TRUE)", sep=""))
			putd("oldCols", ActiveDataSet()[,vars])
		}
		# with just 1 var, create fake-matrix, execute function and delete fake
		if( length(vars)==1 ) {
			xtmp <- ActiveDataSet()
			x1tmp <- cbind(0, xtmp[,vars])
			xtmp[, vars] <- addNoise(x1tmp, noise=noise, method=method)$xm[,2]
			updateActiveDataSet(xtmp)
			freqCalcIndivRisk()
		} else {
			xtmp <- ActiveDataSet()
			xtmp[, vars] <- addNoise(xtmp[,vars], noise=noise, method=method)$xm
			updateActiveDataSet(xtmp)
			#freqCalcIndivRisk()
		}
		if( !redo ) {
			putd("newCols", ActiveDataSet()[,vars])
			nm_risk_print_function()
		}
	}
	
	# function for nm_button1
	# globalRecodeGroup-numericalMethods function
	nm1 <- function(...) {
		lTOr <- function(h, ...) {
			if( length(h)>0 ) {
				if( length(selTab[])==1 ) {
					if( is.na(selTab[]) ) {
						selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
					} else {
						selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
					}
				} else {
					selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
				}
				if( length(h)==length(varTab[]) ) {
					varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
				} else {
					xtmp <- c()
					for( i in 1:length(varTab[]) ) {
						for( j in 1:length(h) ) {
							if( varTab[][i]==h[j] ) {
								xtmp <- c(xtmp, i)
							}
						}
					}
					varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
				}
			}
		}
		rTOl <- function(h, ...) {
			if( length(h)>0 ) {
				if( length(varTab[])==1 ) {
					if( is.na(varTab[]) ) {
						varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
					} else {
						varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
					}
				} else {
					varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
				}
				if( length(h)==length(selTab[]) ) {
					selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
				} else {
					xtmp <- c()
					for( i in 1:length(selTab[]) ) {
						for( j in 1:length(h) ) {
							if( selTab[][i]==h[j] ) {
								xtmp <- c(xtmp, i)
							}
						}
					}
					selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
				}
			}
		}
		nm1_window = gwindow("Add noise", width=230, parent=window)
		nm1_windowGroup = ggroup(cont=nm1_window, horizontal=FALSE)
		tmp = gframe("Noise", cont=nm1_windowGroup, horizontal=FALSE)
		ntmp = ggroup(cont=tmp)
		glabel("Value between 0 and 2000", cont=ntmp)
		noiseSel = gedit()
		add(ntmp, noiseSel)
		tmp = gframe("Method", cont=nm1_windowGroup, horizontal=FALSE)
		methodSel = gdroplist(c("additive", "correlated2"))
		add(tmp, methodSel)
		tmp = gframe("Variable selection", cont=nm1_windowGroup)
		numVars <- c()
		xtmp <- ActiveDataSet()
		# not all vars, just numerical vars
		#for( i in 1:dim(xtmp)[2] ) {
		#	if( class(xtmp[,i])=="numeric" & names(xtmp)[i] != getd("wVars") ) {
		#		numVars <- c(numVars, names(xtmp)[i])
		#	}
		#}
		numVars <- getd("numVars")
		varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
		size(varTab) <- c(120,200)
		add(tmp, varTab)
		btmp = ggroup(cont=tmp, horizontal=FALSE)
		addSpring(btmp)
		gbutton(">>", cont=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
		gbutton("<<", cont=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
		addSpring(btmp)
		selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
		size(selTab) <- c(120,200)
		add(tmp, selTab)
		gseparator(cont=nm1_windowGroup)
		nm1_windowButtonGroup = ggroup(cont=nm1_windowGroup)
		addSpring(nm1_windowButtonGroup)
		gbutton("Ok", cont=nm1_windowButtonGroup,
				handler=function(h,...) {
					noise <- as.numeric(svalue(noiseSel))
					if( !is.numeric(noise) | is.na(noise) ) {
						gmessage("Noise needs to be a numeric value!", title="Information", icon="info", parent=nm1_window)
					} else {
						if( length(selTab[])==0 | is.na(selTab[]) ) {
							gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=nm1_window)
						} else {
							addNoise_tmp(noise, svalue(methodSel), selTab[])
							dispose(nm1_window)
						}
					}
				})
		gbutton("Cancel", cont=nm1_windowButtonGroup, handler=function(h,...) { dispose(nm1_window) })
	}
	
	# function for gr_button3
	# needed sub functions
	# TODO: done - save rename for script/history
	renameVars_tmp <- function(v, h, newName, redo=FALSE) {
		if( !redo ) {
			Script.add(paste("renameVars_tmp(", parseVarStr(v), ", ",
						parseVarStr(h), ", ", parseVarStr(newName), ", redo=TRUE)", sep=""))
		}
		xtmp <- ActiveDataSet()
		levels(xtmp[,v]) <- ifelse(levels(xtmp[,v])==h, newName, levels(xtmp[,v]))
		updateActiveDataSet(xtmp)
		#freqCalcIndivRisk()
	}
	# TODO: done - save group for script/history
	groupVars_tmp <- function(v, h, newName, redo=FALSE) {
		if( !redo ) {
			Script.add(paste("groupVars_tmp(", parseVarStr(v), ", ",
						parseVarStr(h), ", ", parseVarStr(newName), ", redo=TRUE)", sep=""))
		}
		xtmp <- ActiveDataSet()
		for( i in 1:length(h) ) {
			levels(xtmp[,v]) <- ifelse(levels(xtmp[,v])==h[i], newName, levels(xtmp[,v]))
		}
		updateActiveDataSet(xtmp)
		#freqCalcIndivRisk()
	}
	# group and rename variables
	# globalRecodeGroup function
	gr3 <- function(...) {
		facVars <- c()
		anyFac <- FALSE
		xtmp <- ActiveDataSet()
		for( i in 1:dim(xtmp)[2] ) {
			if( class(xtmp[,i])=="factor" ) {
				facVars <- c(facVars, names(xtmp)[i])
				anyFac <- TRUE
			}
		}
		if( anyFac==FALSE ) {
			gmessage("No variables of type factor available.", title="Information",
					icon="info", parent=window)
		} else {
			renameFacVar <- function(h, v, ...) {
				if( length(h)< 1 ) {
					gmessage("You need to select at least 1 level.", title="Information", icon="warning")
				} else {
					if( length(h)> 1 ) {
						gmessage("To rename one, you just have to select 1.", title="Information",
								icon="warning", parent=gr3_window)
					} else {
						newName <- ginput("Please enter a new level name.", parent=gr3_window)
						if( !is.na(newName) & newName!="" ) {
							renameVars_tmp(v, h, newName)
							showLevels(v)
						}
					}
				}
			}
			groupFacVar <- function(h, v, ...) {
				if( length(h)< 2 ) {
					gmessage("You need to select at least 2 levels to group.", title="Information",
							icon="warning", parent=gr3_window)
				} else {
					levName <- h[1]
					for( i in 2:length(h) ) {
						levName <- paste(levName, "_and_", h[i], sep="")
					}
					newName <- ginput("Please enter a new level name.", text=levName, parent=gr3_window)
					if( !is.na(newName) ) {
						groupVars_tmp(v, h, newName)
						showLevels(v)
					}
				}
			}
			showLevels <- function(h, ...) {
				facTab[,] <- data.frame(levels=levels(ActiveDataSet()[,h]), stringsAsFactors=FALSE)
				enabled(gr3_windowButton1) <- TRUE
				enabled(gr3_windowButton2) <- TRUE
			}
			gr3_window = gwindow("Rename/group Variables", width=230, parent=window)
			gr3_windowGroup = ggroup(cont=gr3_window, horizontal=FALSE)
			tmp = gframe("Select variable", cont=gr3_windowGroup)
			selFacVar = gdroplist(sort(facVars))
			add(tmp, selFacVar, expand=TRUE)
			gbutton(" select ", cont=tmp, handler=function(h,...) { showLevels(svalue(selFacVar)) } )
			tmp = gframe("Levels", cont=gr3_windowGroup)
			facTab = gtable(data.frame(levels=character(0), stringsAsFactors=FALSE),
					multiple=TRUE)
			size(facTab) <- c(120,150)
			add(tmp, facTab)
			btmp = ggroup(cont=tmp, horizontal=FALSE, expand=TRUE)
			gr3_windowButton1 = gbutton("rename",
					handler=function(h,...) { renameFacVar(svalue(facTab), svalue(selFacVar)) })
			enabled(gr3_windowButton1) <- FALSE
			gr3_windowButton2 = gbutton("group",
					handler=function(h,...) { groupFacVar(svalue(facTab), svalue(selFacVar)) })
			enabled(gr3_windowButton2) <- FALSE
			add(btmp, gr3_windowButton1)
			add(btmp, gr3_windowButton2)
			gseparator(cont=gr3_windowGroup)
			gr3_windowButtonGroup = ggroup(cont=gr3_windowGroup)
			addSpring(gr3_windowButtonGroup)
			gbutton("Ok", cont=gr3_windowButtonGroup, handler=function(h,...) { 
						dispose(gr3_window)
						freqCalcIndivRisk()
					})
		}
	}
	
	# globalRecode_tmp - globalRecode()
	# TODO: replace cut with globalRecode as soon as it is corrected
	# TODO: done - save globalRecode for script/history
	globalRecode_tmp <- function(var, breaks, labels, redo=FALSE) {
		if( !redo ) {
			Script.add(paste("globalRecode_tmp(", parseVarStr(var), ", ",
						parseVar(breaks), ", ", parseVarStr(labels), ", redo=TRUE)", sep=""))
		}
		xtmp <- ActiveDataSet()
		res <- cut(xtmp[,getIndex(var)], breaks=breaks, labels=labels)
		xtmp[,getIndex(var)] <- res
		updateActiveDataSet( xtmp )
		#freqCalcIndivRisk()
	}
	
	# function for gr_button1
	# globalRecodeGroup function
	gr1 <- function(...) {
		# TODO: add summary and head display
		updateViews <- function(var, ...) {
			xtmp <- ActiveDataSet()
			svalue(gr1_head) <- capture.output(print(head(xtmp[,var])),append=FALSE)
			svalue(gr1_summary) <- capture.output(print(summary(xtmp[,var])),append=FALSE)
		}
		gr1_window = gwindow("Choose parameters for globalRecode", width=300, parent=window)
		gr1_all = ggroup(horizontal=FALSE, cont=gr1_window)
		gr1_main = ggroup(cont=gr1_all)
				
		gr1_pars = ggroup(horizontal=FALSE, cont=gr1_main)
		gr1_views = ggroup(horizontal=FALSE, cont=gr1_main)
		
		tmp = gframe("Head of selected Variable", horizontal=FALSE, cont=gr1_views)
		gr1_head = gtext("", cont=tmp, height=100, width=250)
		addSpring(gr1_views)
		tmp = gframe("Summary of selected Variable", horizontal=FALSE, cont=gr1_views)
		gr1_summary = gtext("", cont=tmp, height=100, width=250)
		
		tmp = gframe("choose Variable", cont=gr1_pars)
		xtmp <- ActiveDataSet()
		xname <- c()
		# would display all but weight var
		#for( i in 1:length(names(xtmp))) {
		#	if( names(xtmp)[i]!=getd("wVars") ) {
		#		xname <- c(xname, names(xtmp)[i])
		#	}
		#}
		xname <- getd("keyVars")
		xy <- c()
		for( i in 1:length(xname) ) {
			if( is.numeric(xtmp[,xname[i]]) ) {
				xy <- c(xy, xname[i])
			} 
		}
		x = gdroplist(sort(xy), handler=function(k,...) { updateViews(svalue(k$obj)) })
		add(tmp, x, expand=TRUE)
		gbutton("var to factor", cont=tmp, handler=function(k,...) {
					var = svalue(x)
					gconfirm("Converts variable to datatype factor.", title="Information", icon="info",
							parent=gr1_window, handler=function(h, ...) {
								try(xtmp[,var] <- as.factor(xtmp[,var]), silent=TRUE)
								updateActiveDataSet(xtmp)
								dispose(gr1_window)
							})
					
				})
		tmp = gframe("breaks", horizontal=FALSE, cont=gr1_pars)
		lab <-              "Example input: 1,3,5,9 splits var in 3 groups"
		lab <- paste(lab, "\n(1,3],(3,5] and (5,9]. If you just supply")
		lab <- paste(lab, "\n1 number, like 3, the var will be split in")
		lab <- paste(lab, "\n3 equal sized groups.")
		glabel(lab, cont=tmp)
		y = gedit(width=40)
		add(tmp, y, expand=TRUE)
		tmp = gframe("labels", horizontal=FALSE, cont=gr1_pars)
		lab <-              "Labels are depending on your breaks-input."
		lab <- paste(lab, "\nExample inupt with breaks=1,3,5,9 or breaks=3:")
		lab <- paste(lab, "\n- leave it blank: auto numbering from 1 to 3")
		lab <- paste(lab, "\n- a,b,c: the 3 groups are named a, b and c")
		glabel(lab, cont=tmp)
		z = gedit()
		add(tmp, z, expand=TRUE)
		gseparator(cont=gr1_all)
		okCancelGroup = ggroup(cont=gr1_all)
		addSpring(okCancelGroup)
		gbutton("Ok", cont=okCancelGroup,
				handler=function(h,...) {
						# check if input is usable
					var <- svalue(x)
					breaks <- svalue(y)
					labels <- svalue(z)
					breaks <- strsplit(breaks, ",")[[1]]
					labels <- strsplit(labels, ",")[[1]]
					allNumeric <- TRUE
					labelsNumeric <- TRUE
					gr_do <- TRUE
					if( length(breaks)==0 ) {
						allNumeric <- FALSE
					} else {
						try(breaks <- as.numeric(breaks), silent=TRUE)
						for( i in 1:length(breaks) ) {
							if( is.na(breaks[i]) ) {
								allNumeric <- FALSE
							}
						}
					}
					if( allNumeric==FALSE ) {
						gmessage("Breaks argument is not valid", title="Information", icon="info", parent=gr1_window)
						gr_do <- FALSE
					}
					if( allNumeric ) {
						if( length(labels)>0 ) {
							if( length(breaks)==1 ) {
								if( length(labels)!=breaks) {
									gmessage("Too many or few labels supplied", title="Information", icon="info", parent=gr1_window)
									gr_do <- FALSE
								}
							}
							if( length(breaks)>1 ) {
								if( length(labels)!=(length(breaks)-1) ) {
									gmessage("Too many or few labels supplied", title="Information", icon="info", parent=gr1_window)
									gr_do <- FALSE
								}
							}
							if( gr_do ) {
								try(tmp_labels <- as.numeric(labels), silent=TRUE)
								for( i in 1:length(tmp_labels) ) {
									if( is.na(tmp_labels[i]) ) {
										labelsNumeric <- FALSE
									}
								}
								if( labelsNumeric ) {
									labels <- as.numeric(labels)
								}
								if( !labelsNumeric ) {
									gr_do <- gconfirm("Variable will be of typ factor afterwards", title="Information",
											icon="warning", parent=gr1_window)
								}
							}
						} else {
							labels <- FALSE
						}
					}
					if( gr_do ) {
						#print(breaks)
						#print(class(breaks))
						#print(labels)
						#print(class(labels))
						globalRecode_tmp(var, breaks, labels)
						dispose(gr1_window)
					}
					} )
			gbutton("Cancel", cont=okCancelGroup, handler=function(h,...) dispose(gr1_window) )
			add(gr1_window, gr1_all)
			updateViews(xname[1])
	}
  
	# function for gr_button2
	# opens script window to execute R commands directly
	# globalRecodeGroup function
	scriptWindow <- function(...) {
		# TODO: auto scroll down needs to be implemented
		scriptEnv = new.env()
		assign("cmdhist", c(), envir=scriptEnv)
		sendCommand <- function(gin, gout, ...) {
			insert(gout, paste(">", svalue(gin)), font.attr=c(color="red", family="monospace"))
			res <- capture.output(err <- try(eval(parse(text=svalue(gin)), envir=scriptEnv), silent=TRUE), append=FALSE)
			if( class(err)=="try-error" ) {
				insert(gout, strsplit(err, " : ")[[1]][2], font.attr=c(family="monospace"))
			} else {
				if( length(err)>0 ){
					err <- capture.output(print(err))
					insert(gout, err[1], font.attr=c(family="monospace"))
					if( length(err)>1 ) {
						for( i in 2:length(err) ) {
							insert(gout, err[i], font.attr=c(family="monospace"))
						}
					}
				}
				if( length(strsplit(svalue(gin), "<-")[[1]])>1 || length(strsplit(svalue(gin), "=")[[1]])>1 ) {
					cmdhist <- get("cmdhist", envir=scriptEnv)
					cmdhist <- c(cmdhist, svalue(gin))
					assign("cmdhist", cmdhist, envir=scriptEnv)
				}
			}
			svalue(gin) <- ""
		}
		saveAds <- function(...) {
			updateActiveDataSet(get("ads", envir=scriptEnv))
			#freqCalcIndivRisk()
			#assign("x", get("ads", envir=scriptEnv), envir=.GlobalEnv)
			# TODO: done? - save commands for script/history
			cmdhist <- get("cmdhist", envir=scriptEnv)
			if( length(cmdhist) > 0 ) {
				Script.add("ads <- ActiveDataSet()")
				for( i in 1:length(cmdhist) ) {
					Script.add(cmdhist[i])
				}
				Script.add("updateActiveDataSet(ads)")
				Script.add("freqCalcIndivRisk()")
			}
			# end save
			quitScriptWindow()
		}
		removeWs <- function(...) {
			if( exists("scriptEnv", envir=.GlobalEnv) ) {
				try(rm(scriptEnv, envir=.GlobalEnv), silent=TRUE)
			}
		}
		sureQuit <- function(...) {
			gconfirm("You want to close the window without saving?", icon="question", parent=scriptWindow,
					handler=function(h,...) quitScriptWindow() )
		}
		quitScriptWindow <- function(...) {
			removeWs()
			dispose(scriptWindow)
		}
		loadAds <- function(...) {
			assign("ads", ActiveDataSet(), envir=scriptEnv)
			#assign("ads", francdat, envir=scriptEnv)
		}
		scriptWindow = gwindow("Script window", parent=window)
		scriptWidget = ggroup(horizontal=FALSE)
		scriptInfoGroup = ggroup(cont=scriptWidget)
		addSpring(scriptInfoGroup)
		glabel("ActiveDataSet available for modifications as variable: ads",
				cont=scriptInfoGroup)
		gbutton("Reload active data set to ads", cont=scriptInfoGroup,
				handler=function(h,...) loadAds() )
		addSpring(scriptInfoGroup)
		loadAds()
		xout = gtext(text="", width=700, height=400)
		add(scriptWidget, xout)
		scriptSubmit = ggroup(cont=scriptWidget)
		glabel(" >", cont=scriptSubmit)
		xcom = gedit("", cont=scriptSubmit, expand=TRUE, handler=function(h, ...) sendCommand(xcom, xout))
		gbutton("submit", cont=scriptSubmit, handler=function(h, ...) sendCommand(xcom, xout))
		gseparator(cont=scriptWidget)
		saveCancelGroup = ggroup(cont=scriptWidget)
		addSpring(saveCancelGroup)
		gbutton("Save", cont=saveCancelGroup, handler=function(h,...) saveAds() )
		gbutton("Cancel", cont=saveCancelGroup, handler=function(h,...) sureQuit() )
		
		add(scriptWindow, scriptWidget)
		focus(xcom)
	}
	
	# TODO: nm_risk_print_function
	# nm_risk_print output function
	nm_risk_print_function <- function(...) {
		xprogress = gwindow("please wait", width=180, height=40, parent=window)
		
		drisk <- dRiskRMD(getd("oldCols"), getd("newCols"), k=svalue(nm_risk_slider1), k2=svalue(nm_risk_slider2))
		dutil <- dUtility(getd("oldCols"), getd("newCols"))
		
		svalue(nm_risk_print) <- paste("Sensitive observations(percent):", round(drisk$risk2,3), "\n\n", 
																		"Standard distance of the perturbed data to\nthe original one\n    ", dutil)
	
		dispose(xprogress)
	}
	selVar <- function(...) {
		putd("keyLen", 0)
		putd("numLen", 0)
		putd("wLen", 0)
		ft <- function(f, t, h, var, pm, ...) {
			# pm: 1 for +, 0 for -
			count = getd(var)
			if( pm == 1 ) {
				count <- count + length(h);
			} else {
				count <- count - length(h);
			}
			putd(var, count)
			if( length(h)>0 ) {
				if( length(f[])==1 ) {
					if( is.na(f[]) ) {
						f[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
					} else {
						f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
					}
				} else {
					f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
				}
				if( length(h)==length(t[]) ) {
					t[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
				} else {
					xtmp <- c()
					for( i in 1:length(t[]) ) {
						for( j in 1:length(h) ) {
							if( t[][i]==h[j] ) {
								xtmp <- c(xtmp, i)
							}
						}
					}
					t[,] <- data.frame(vars=t[-xtmp], stringsAsFactors=FALSE)
				}
			}
		}
		selVar_window = gwindow("Select variables", width=230, parent=window)
		selVar_windowGroup = ggroup(cont=selVar_window, horizontal=FALSE)
		selVar_main = ggroup(cont=selVar_windowGroup)
		mtmp = ggroup(cont=selVar_main)
		numVars <- c()
		xtmp <- ActiveDataSet()
		for( i in 1:dim(xtmp)[2] ) {
			numVars <- c(numVars, names(xtmp)[i])
		}
		varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
		size(varTab) <- c(120,200)
		add(mtmp, varTab)
		
		rtmp = ggroup(cont=mtmp, horizontal=FALSE)
		
		tmp = gframe("categorical", cont=rtmp)
		btmp = ggroup(cont=tmp, horizontal=FALSE)
		addSpring(btmp)
		gbutton(">>", cont=btmp, handler=function(h,...) { ft(catTab, varTab, svalue(varTab), "keyLen", 1) })
		gbutton("<<", cont=btmp, handler=function(h,...) { ft(varTab, catTab, svalue(catTab), "keyLen", 0) })
		addSpring(btmp)
		catTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
		size(catTab) <- c(120,100)
		add(tmp, catTab)
		
		tmp = gframe("numerical", cont=rtmp)
		btmp = ggroup(cont=tmp, horizontal=FALSE)
		addSpring(btmp)
		gbutton(">>", cont=btmp, handler=function(h,...) { ft(numTab, varTab, svalue(varTab), "numLen", 1) })
		gbutton("<<", cont=btmp, handler=function(h,...) { ft(varTab, numTab, svalue(numTab), "numLen", 0) })
		addSpring(btmp)
		numTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
		size(numTab) <- c(120,100)
		add(tmp, numTab)
		
		tmp = gframe("weight", cont=rtmp)
		btmp = ggroup(cont=tmp, horizontal=FALSE)
		addSpring(btmp)
		gbutton(">>", cont=btmp, handler=function(h,...) { ft(wTab, varTab, svalue(varTab), "wLen", 1) })
		gbutton("<<", cont=btmp, handler=function(h,...) { ft(varTab, wTab, svalue(wTab), "wLen", 0) })
		addSpring(btmp)
		wTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
		size(wTab) <- c(120,50)
		add(tmp, wTab)
		
		gseparator(cont=selVar_windowGroup)
		selVar_windowButtonGroup = ggroup(cont=selVar_windowGroup)
		addSpring(selVar_windowButtonGroup)
		gbutton("Ok", cont=selVar_windowButtonGroup,
				handler=function(h,...) {
					# check if firstrun - if not reset script and dataset to original one
					cat(paste(getd("keyLen"), getd("numLen"), getd("wLen"), "\n"))
					fr_do <- TRUE
					if( !getd("firstRun") ) {
						fr_do <- gconfirm("If you reselect vars, script and dataset will reset.\nAre you sure?", title="Attention",
								icon="warning", parent=window)
						if( fr_do ) {
							Script.new()
							if( existd("oldDataSet") ) {
								putd("activeDataSet", getd("oldDataSet"))
								#xtmp <- getd("oldDataSet")
								#updateActiveDataSet(xtmp)
							}
						}
					} else {
						putd("firstRun", FALSE)
					}
					# check if enough is selected
					if( fr_do ) {
						# min selection must be 1 in each category
						if( getd("keyLen")>=2  & getd("numLen")>=1 &
								getd("wLen")==1 ) {
							confirmSelection_tmp(catTab[], numTab[], wTab[])
							dispose(selVar_window)
						} else {
							gmessage("You have to select at least 2 categoric variables and 1 of each other",
									title="Information", icon="warning", parent=window)
						}
					}
				})
		gbutton("Cancel", cont=selVar_windowButtonGroup, handler=function(h,...) { dispose(selVar_window) })
	}
	
	
  # function for gb1 (confirm selection)
	# needed sub functions
	# TODO: done - save selection for script/history
	confirmSelection_tmp <- function(t1, t2, t3, redo=FALSE) {
		if( !redo ) {
			Script.add(paste("confirmSelection_tmp(", parseVarStr(t1), ", ",
						parseVarStr(t2), ", ", parseVarStr(t3), ", redo=TRUE)", sep=""))
		}
		putd("keyIndex", getIndex(t1))
		putd("keyVars", t1)
		putd("numIndex", getIndex(t2))
		putd("numVars", t2)
		putd("wIndex", getIndex(t3))
		putd("wVars", t3)
		# TODO: Experimental - all key vars as factors, done in updateActiveDataSet()!!!
		updateActiveDataSet(ActiveDataSet())
		# End - Experimental
		# TODO: update main window variable display
		stmp <- "Categorical:"
		vtmp <- getd("keyVars")
		for( i in 1:length(vtmp) ) {
			stmp <- paste(stmp, vtmp[i])
			if( i < length(vtmp) )
				stmp <- paste(stmp, ",", sep="")
		}
		svalue(tab1) <- stmp
		stmp <- "Numerical:"
		vtmp <- getd("numVars")
		for( i in 1:length(vtmp) ) {
			stmp <- paste(stmp, vtmp[i])
			if( i < length(vtmp) )
				stmp <- paste(stmp, ",", sep="")
		}
		svalue(tab2) <- stmp
		stmp <- "Weight:"
		vtmp <- getd("wVars")
		for( i in 1:length(vtmp) ) {
			stmp <- paste(stmp, vtmp[i])
		}
		svalue(tab3) <- stmp
		# enable plot indivRisk button
		enabled(ir_button) <- TRUE
		enabled(ls_button1) <- TRUE
		#enabled(ls_button2) <- TRUE
		enabled(ls_button3) <- TRUE
		enabled(gr_button1) <- TRUE
		enabled(gr_button2) <- TRUE
		enabled(gr_button3) <- TRUE
		enabled(nm_button1) <- TRUE
		enabled(nm_button2) <- TRUE
		enabled(nm_button3) <- TRUE
		enabled(nm_risk_slider1) = TRUE
		enabled(nm_risk_slider2) = TRUE
	}
  # variableSelectionGroup function
	#       if re-clicked, prompt and ask if you want to reset all work and script done
	# 			this is to be used to set dataset to start format as well as reset script,
	#				because it is not needed to reselect the vars during the work process
  confirmSelection <- function(...) {
		# open selection window
		selVar()
  }
  
  ## Menubar Functions
  asdf <- function(...) {
    cat("do...\n")
  }
  # Data - Load Dataset
  loadDataSet <- function(...) {
		xname <- gfile("Select file to load", parent=window, type="open" )
		if( xname != '' ) {
			load(xname, envir=.GlobalEnv)
		}
  }
  # Data - Choose Dataset
  setDataSet <- function(...) {
    vardt <- ls(envir = .GlobalEnv, all.names=TRUE)
    vards <- names(which(sapply(vardt, function(.x) is.data.frame(get(.x)))))
    vards <- c(vards,names(which(sapply(vardt, function(.x) is.matrix(get(.x))))))
    if( length(vards)==0 ) {
      gmessage("No datasets loaded.", title="Information", icon="warning",
        parent=window)
    } else {
      gbasicdialog(title="Choose Dataset",
        x<-gdroplist(vards), parent=window,
        handler=function(x, ...) { ActiveDataSet(svalue(x$obj))
																	 putd("oldDataSet", ActiveDataSet()) })
				if( existd("activeDataSet") ) {
					if( dim(ActiveDataSet())[1] > 4000 ) {
						gmessage("Operations in this dataset may require some time, so please be patient.", title="Information",
								icon="info", parent=window)
					}
	        svalue(dslab) <- getd("dataSetName")
	        enabled(gb1) <- TRUE
				}
    }
  }
  # Data - Save Dataset To - File
  saveToFile <- function(...) {
    saveVar <- function(fileName, ...) {
			xtmp <- ActiveDataSet()
      save(xtmp, file=paste(fileName,".RData", sep=""))
    }
    if( existd("activeDataSet") ) {
      xname <- gfile("Choose a file to save the Dataset", type="save", parent=window)
			if( xname != "" ) {
				saveVar(xname)
			}
    } else {
      gmessage("No active Dataset found.", title="Information", icon="warning",
        parent=window)
    }
  }
  # Data - Save Dataset To - Variable
  saveToVariable <- function(...) {
    checkAndSave <- function(parent, varName, ...) {
      saveVar <- function(varName, ...) {
        assign(varName, ActiveDataSet(), envir=.GlobalEnv)
      }
      if( exists(varName, envir=.GlobalEnv) ) {
        gconfirm("Variable already exists, do you want to replace it?",
            title="Information", parent=parent,
            handler=function(h, ...) { saveVar(varName) } )
      } else {
        saveVar(varName)
      }
    }
    if( existd("activeDataSet") ) {
      xname = ginput("Please enter a Variable name",
        title="Choose Variable name", icon="question", parent=window,
        handler=function(h, ...) checkAndSave(h$obj, h$input) )
    } else {
      gmessage("No active Dataset found.", title="Information", icon="warning",
        parent=window)
    }
  }
	
	# Script - New Script
	newScript <- function(...) {
		ns_do <- gconfirm("A new script will be started.\nAre you sure?", title="Information",
				icon="warning", parent=window)
		if( ns_do ) {
			Script.new()
		}
	}
	
	# Script - Save Script
	saveScript <- function(...) {
		saveScriptToFile <- function(fileName, ...) {
			cmdtmp <- Script()
			save(cmdtmp, file=paste(fileName,".sdcMicroScript", sep=""))
		}
		if( existd("activeScript") ) {
			xname <- gfile("Select file to save Script", type="save", parent=window)
			if( xname != "" ) {
				saveScriptToFile(xname)
			}
		} else {
			gmessage("No active Script found.", title="Information", icon="warning",
					parent=window)
		}
	}
	
	# Script - Load Script
	loadScript <- function(...) {
		# open file browser and load the needed script
		xname <- gfile("Select script file to open.", parent=window, type="open", 
								filter=list("Script files" = list(patterns = c("*.sdcMicroScript"))) )
		if( xname != '' ) {
			load(xname, envir=sdcGUIenv)
			Script.new()
			putd("activeScript", get("cmdtmp", envir=sdcGUIenv))
			rm(cmdtmp, envir=sdcGUIenv)
		}
	}
	
	# Script - View Script
	# TODO: implement view script
	viewScript <- function(...) {
		cmdhist <- Script()$cmd
		if( is.null(cmdhist) ) {
			gmessage("No script present at the moment.", title="Attention", icon="warning", parent=window)
		} else {
			sureQuit <- function(...) {
				gconfirm("Do you want to close the window without saving?", icon="question", parent=scriptEditWindow,
						handler=function(h,...) quitEditScriptWindow() )
			}
			quitEditScriptWindow <- function(...) {
				xtmp <- list(cmd=c(xscript[]))
				Script(xtmp)
				dispose(scriptEditWindow)
			}
			runCMDhist <- function(...) {
				rto <- as.numeric(svalue(runTo))
				cmdhist <- xscript[]
				if( is.numeric(rto) & !is.na(rto) ) {
					if( rto>0 & rto<(length(cmdhist)+1) ) {
						cmdhisttmp <- cmdhist[c(1:rto)]
						Script.run(cmdhisttmp)
						quitEditScriptWindow()
					}
				} else {
					gmessage("Script step not valid.", title="Input not valid", icon="info", parent=scriptEditWindow)
				}
			}
			delCMDhist <- function(...) {
				dto <- as.numeric(svalue(delRow))
				cmdhist <- xscript[]
				if( is.numeric(dto) & !is.na(dto) ) {
					if( dto>0 & dto<(length(cmdhist)+1) ) {
						cmdhisttmp <- cmdhist[-dto]
						xscript[] <- cmdhisttmp
						svalue(delRow) <- ""
					}
				} else {
					gmessage("Script step not valid.", title="Input not valid", icon="info", parent=scriptEditWindow)
				}
			}
			scriptEditWindow = gwindow("View script", parent=window, width=700, height=400)
			scriptWidget = ggroup(horizontal=FALSE)
			xscript = gdf(cmdhist, expand=TRUE)
			# TODO: find replacement, cause in linux it wouldnt display anything.
			#enabled(xscript) <- FALSE
			add(scriptWidget, xscript, expand=TRUE)
			gseparator(cont=scriptWidget)
			saveCancelGroup = ggroup(cont=scriptWidget)
			addSpring(saveCancelGroup)
			tmp = ggroup(cont=saveCancelGroup)
			glabel("Delete script step: ", cont=tmp)
			delRow = gedit(text="", width=3, cont=tmp)
			gbutton("Delete", cont=tmp, handler=function(h,...) delCMDhist() )
			addSpring(saveCancelGroup)
			tmp = ggroup(cont=saveCancelGroup)
			glabel("Run script to row: ", cont=tmp)
			runTo = gedit(text=length(cmdhist), width=3, cont=tmp)
			gbutton("Run", cont=tmp, handler=function(h,...) runCMDhist() )
			addSpring(saveCancelGroup)
			gbutton("Close", cont=saveCancelGroup, handler=function(h,...) quitEditScriptWindow() )
			
			add(scriptEditWindow, scriptWidget)
		}
	}
	
	# Script - Run Script
	runScript <- function(...) {
		# dialog and ask if you want to run the whole script on this dataset
		Script.run()
	}
	
  # GUI - Quit
  quitGUI <- function(...) {
		val <- gconfirm("Do you really want to close the window?", parent=window)
		if( as.logical(val) ) {
			dispose(window)
		}
  }
  
  ## initialize
	# set first run
	putd("firstRun", TRUE)
	# set up new script
	Script.new()
  # get values of internal vars if they exist
  activeDataSet <- if( existd("activeDataSet") ) getd("activeDataSet") else ""
  dataSetName <- if( existd("dataSetName") ) getd("dataSetName") else ""
  # save intitial values in env
  if( !dataSetName=="" ) {
    ActiveDataSet(dataSetName)
  }
  putd("dataSetName", dataSetName)

  ## create window
  window = gwindow("sdcMicro GUI")
	addHandlerUnrealize(window, handler = function(h,...) {
				val <- gconfirm("Do you really want to close the window?", parent=h$obj)
				if(as.logical(val))
					return(FALSE)             # destroy
				else
					return(TRUE)              # don't destroy
			})

  ## Menubar
  mbar = list()
  mbar$GUI$Quit$handler = quitGUI
  mbar$Data$"Load Dataset"$handler = loadDataSet
  mbar$Data$"Choose Dataset"$handler = setDataSet
  mbar$Data$"Save Dataset to"$File$handler = saveToFile
  mbar$Data$"Save Dataset to"$Variable$handler = saveToVariable
	mbar$Script$"New"$handler = newScript
	mbar$Script$"Save to file"$handler = saveScript
	mbar$Script$"Load from file"$handler = loadScript
	mbar$Script$"View"$handler = viewScript
	mbar$Script$"Run"$handler = runScript
  mbar$Help$About$handler = asdf

  ## layout
  mainGroup = ggroup(cont=window, horizontal=FALSE)
  # Start - add menu
  add(mainGroup, gmenu(mbar))
  # End - add menu
  # Start - variable Selection Container
  varSelGroup = ggroup(cont=mainGroup)
	mtmp = ggroup(cont=varSelGroup, horizontal=FALSE, expand=TRUE)
  tmp = gframe("Selected variables", cont=mtmp, horizontal=FALSE)
	rtmp = ggroup(cont=tmp)
  tab1 = glabel("")
  add(rtmp, tab1, expand=TRUE)
	addSpring(rtmp)
	addSpring(rtmp)
	addSpring(rtmp)
	rtmp = ggroup(cont=tmp)
  tab2 <- glabel("")
  add(rtmp, tab2, expand=TRUE)
	addSpring(rtmp)
	addSpring(rtmp)
	addSpring(rtmp)
	rtmp = ggroup(cont=tmp)
  tab3 <- glabel("")
  add(rtmp, tab3, expand=TRUE)
	addSpring(rtmp)
	addSpring(rtmp)
	addSpring(rtmp)
  varSelGroupButton = ggroup(cont=varSelGroup, horizontal=FALSE)
  glabel("Loaded data set:", cont=varSelGroupButton)
  if( existd("dataSetName") ) {
    dslab = glabel(getd("dataSetName"),cont=varSelGroupButton)
  } else {
    dslab = glabel("none",cont=varSelGroupButton)
  }
  gb1 = gbutton(text="Select variables", cont=varSelGroupButton,
    handler=function(h,...) confirmSelection())
  enabled(gb1) <- FALSE
  # End - variable Selection Container
  # Start - freqCalc Container
  freqCalcGroup = ggroup(cont=mainGroup)
  fc_tmp = gframe("Frequencies calculation for risk estimation",
                  cont=freqCalcGroup, expand=TRUE)
  tmp = gframe("print", cont=fc_tmp)
  fc_print = gtext(text="", width=240, height=80)
  add(tmp, fc_print)
  addSpring(fc_tmp)
  tmp = gframe("summary", cont=fc_tmp)
  fc_summary = gtext(text="", width=240, height=80)
  add(tmp, fc_summary)
  # End - freqCalc Container
	# indivRisk and globalRecode Container
	indivRiskGlobalRecodeGroup = ggroup(cont=mainGroup)
  # Start - indivRisk Container
  indivRiskGroup = ggroup(cont=indivRiskGlobalRecodeGroup)
  ir_tmp = gframe("Individual risk computation", cont=indivRiskGroup, horizontal=FALSE)
  tmp = gframe("print", cont=ir_tmp, expand=TRUE)
  ir_print = gtext(text="", width=240, height=80)
  add(tmp, ir_print)
  indivRiskGroupButton = ggroup(cont=ir_tmp)
  addSpring(indivRiskGroupButton)
  ir_button = gbutton("plot individual Risk", cont=indivRiskGroupButton,
     handler=function(h, ...) plotIndivRisk(getd("freqCalc"),getd("indivRisk")))
  addSpring(indivRiskGroupButton)
  enabled(ir_button) <- FALSE
  # End - indivRisk Container
	# Start - globalRecode Container
	globalRecodeGroup = ggroup(cont=indivRiskGlobalRecodeGroup, horizontal=FALSE)
	tmp = gframe("Recode", cont=globalRecodeGroup)
	globalRecodeGroupLeft = ggroup(cont=tmp, horizontal=FALSE)
	gr_button1 = gbutton("Global recode", handler=function(h, ...) gr1() )
	enabled(gr_button1) <- FALSE
	add(globalRecodeGroupLeft, gr_button1)
	gr_button3 = gbutton("Rename/-group factor vars", handler=function(h, ...) gr3() )
	enabled(gr_button3) <- FALSE
	add(globalRecodeGroupLeft, gr_button3)
	globalRecodeGroupRight = ggroup(cont=tmp, horizontal=FALSE)
	tmp = gframe("Experts only", cont=globalRecodeGroupRight)
	gr_button2 = gbutton("Script window", handler=function(h, ...) scriptWindow() )
	enabled(gr_button2) <- FALSE
	add(tmp, gr_button2)
	# End - globalRecode Container
  # Start - localSupp Container
  localSuppGroup = ggroup(cont=globalRecodeGroup)
  tmp = gframe("Local suppression", cont=localSuppGroup, horizontal=FALSE)
  ls_button1 = gbutton("Local suppression",
      handler=function(h,...) ls1() )
  add(tmp, ls_button1)
  # local Supp2 removed
	#addSpring(tmp)
  #ls_button2 = gbutton("Local Suppression 2",
  #    handler=function(h,...) ls2() )
  #add(tmp, ls_button2)
  ls_button3 = gbutton("Local suppression 2 wrapper",
      handler=function(h,...) ls3() )
  add(tmp, ls_button3)
  addSpring(tmp)
  enabled(ls_button1) <- FALSE
  #enabled(ls_button2) <- FALSE
  enabled(ls_button3) <- FALSE
  # End - localSupp Container
	# Start - numericalMethod Container
	tmp = gframe("Numerical methods", cont=mainGroup)
	tmp1 = ggroup(cont=tmp, horizontal=FALSE)
	 addSpring(tmp1)
	 nm_button1 = gbutton("Add noise", handler=function(h,...) nm1() )
	 add(tmp1, nm_button1)
	 enabled(nm_button1) <- FALSE
	 nm_button2 = gbutton("Microaggregation", handler=function(h,...) nm2() )
	 add(tmp1, nm_button2)
	 enabled(nm_button2) <- FALSE
	 addSpring(tmp1)
	 nm_button3 = gbutton("Recalculate risk", handler=function(h,...) nm_risk_print_function() )
	 add(tmp1, nm_button3)
	 enabled(nm_button3) <- FALSE
	 addSpring(tmp1)
	tmp1 = ggroup(cont=tmp, horizontal=FALSE, expand=TRUE)
	 tmp2 = gframe("Parameters for risk est.", cont=tmp1, horizontal=FALSE)
	 tmp3 = ggroup(cont=tmp2)
	  glabel("k ", cont=tmp3)
	  nm_risk_slider1 = gslider(from=0, to=0.1, by=0.01, value=0.01)
		enabled(nm_risk_slider1) = FALSE
	  add(tmp3, nm_risk_slider1, expand=TRUE)
	 tmp3 = ggroup(cont=tmp2)
	 	glabel("k2", cont=tmp3)
	  nm_risk_slider2 = gslider(from=0, to=0.05, by=0.01, value=0.05)
		enabled(nm_risk_slider2) = FALSE
	  add(tmp3, nm_risk_slider2, expand=TRUE)
	tmp1 = ggroup(cont=tmp)
	 tmp2 = gframe("Numerical method risk", cont=tmp1)
	  nm_risk_print = gtext(text="", width=240, height=70)
	  add(tmp2, nm_risk_print)
	# End - numericalMethod Container
}
# TODO: remove for final version
#sdcGUI()