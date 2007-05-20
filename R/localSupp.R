`localSupp` <-
function(x, keyVar, indivRisk, threshold=0.15){
 ## x ... object from class freqCalc
 ## keyVar ... variables used for local suppression, ordered
 ## indivRisk ... vector of individual risks
 ## fixme: better method for local suppression
 ## calculate risk a second (and third time) and choose another keyVar! no
 if( class(x) != "freqCalc" ) warning("x is not from class freqCalc")
 # keyVars = x$keyVars + 1  ## indexG is now first
 x$freqCalc[indivRisk > threshold, keyVar[1]] <- NA
 x
}

