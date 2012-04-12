#require(sdcMicro)
# Link zum Testdatenset (data(testdata) enthält die ancestors nicht)
#setwd("D:/Documents/Uni/Eclipse Workspace/FOO")
#dataset <- read.csv2("Test7_data.csv")

#variables <- c("urbrur", "roof", "walls", "water", "sex", "relat")
#ancestors <- c("water2", "water3", "relat2")
#ancestor_setting <- cbind(c(3,5), c(2,1))
#weight <- cbind(rep(1,length(variables)), rep(TRUE,length(variables)))

#dataX <- as.matrix(dataset[,c(variables, ancestors), drop=FALSE])
#k_level <- 0
#FindLowestK <- FALSE
#range <- FALSE
#categoryCount <- FALSE
#lowMemory <- FALSE

#res <- .Call("LocalRecProg", dataX, k_level, FindLowestK, ancestor_setting, weight, range, categoryCount, lowMemory, NA)

