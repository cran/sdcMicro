plot.indivRisk <- function (x, ...)
{
    ## x ... object from class indivRisk
    ## y ... object from class freqCalc
    if (!exists("slider.env"))
        slider.env <<- new.env()
    library(tcltk)
    method = "histogram"
    mu <- 0.0025
    assign("mu", tclVar(mu), env = slider.env)
    sd <- 0.5
    assign("sd", tclVar(sd), env = slider.env)
    s2 <- 0.5
    assign("s2", tclVar(s2), env = slider.env)
    #xmin <- 1
    #assign("xmin", tclVar(xmin), env = slider.env)
    #xmax <- 5
    #assign("xmax", tclVar(xmax), env = slider.env)
    #ymin <- 0
    #assign("ymin", tclVar(ymin), env = slider.env)
    #ymax <- round(dnorm(0, 0, 0.5), 2)
    #assign("ymax", tclVar(ymax), env = slider.env)
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
          p1 <- function(method){
        if( method == "histogram" ){ 
          hist(x$rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
          abline(v=mu, col="blue", lwd=2)
        }
        if( method == "ecdf" ){
          plot(ecdf(x$rk), main="ecdf of individual risk", xlab="individual risk")
                    abline(v=as.numeric(evalq(tclvalue(mu), env = slider.env)), col="blue", lwd=2)
        }
      }
    norm.refresh <- function(...) {
        method = method
        mu <- as.numeric(evalq(tclvalue(mu), env = slider.env))
        sd <- as.numeric(evalq(tclvalue(sd), env = slider.env))
        s2 <- as.numeric(evalq(tclvalue(s2), env = slider.env))
        if (mu != mu.old) {
            s2 <- round(length(which(x$rk > mu)))
            sd <- 1/length(x$rk) * (sum(x$fk[x$rk < mu] * x$rk[x$rk < mu]) + mu*sum(x$fk[x$rk>mu])) * 100  
            try(eval(parse(text = paste("tclvalue(s2)<-", s2,
                sep = "")), env = slider.env))
            try(eval(parse(text = paste("tclvalue(sd)<-", sd,
                sep = "")), env = slider.env))                
            sd.old <<- sd
            s2.old <<- s2 
            #print(sd)
            #print(paste("s2:", s2))           
        }
        if (sd != sd.old) {
            sd <- as.numeric(evalq(tclvalue(s2), env = slider.env)) 
            #mu <- sort(x$rk)[
            s2 <- length(which(x$rk > mu))        
            try(eval(parse(text = paste("tclvalue(s2)<-", s2,
                sep = "")), env = slider.env))
            try(eval(parse(text = paste("tclvalue(sd)<-", sd,
                sep = "")), env = slider.env))                
            sd.old <<- sd
            s2.old <<- s2
        }
        if (s2 != s2.old) {
            s2 <- as.numeric(evalq(tclvalue(s2), env = slider.env))
            sd <- 1/length(x$rk) * (sum(x$fk * x$rk) + 0.02*sum(x$fk))
            try(eval(parse(text = paste("tclvalue(sd)<-", sd,
                sep = "")), env = slider.env))
            sd.old <<- sd
            s2.old <<- length(which(x$rk > mu))
        }
        #xmin <- as.numeric(evalq(tclvalue(xmin), env = slider.env))
        #xmax <- as.numeric(evalq(tclvalue(xmax), env = slider.env))
        #ymin <- as.numeric(evalq(tclvalue(ymin), env = slider.env))
        #ymax <- as.numeric(evalq(tclvalue(ymax), env = slider.env))
      p1 <- function(method){
        if( method == "histogram" ){ 
          hist(x$rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
          abline(v=mu, col="blue", lwd=2)
        }
        if( method == "ecdf" ){
          plot(ecdf(x$rk), main="ecdf of individual risk", xlab="individual risk")
                    abline(v=mu, col="blue", lwd=2)
        }
      }
        p1(method=method)
    }
    m <- tktoplevel()
    tkwm.title(m, "Individual risk adjustments")
        fontHeading <- tkfont.create(family="tahoma",size=10)
        tkpack(tklabel(m, text="Please, see at the plot the active graphik device in R", font=fontHeading))
    tkwm.geometry(m, "+0+0")
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Individual risk threshold =", width = "35"), side = "left")
    tkpack(sc <- tkscale(fr, command = norm.refresh, from = 0,
        to = max(x$rk), orient = "horiz", resolution = 0.001, showvalue = T),
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = mu), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Re-identification rate =", width = "35"),
        side = "left")
    tkpack(sc <- tkscale(fr, command = norm.refresh, from = 0,
        to = maxsd, orient = "horiz", resolution = 0.01, showvalue = T),
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sd), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Unsafe recods =", width = "35"), side = "left")
    tkpack(sc <- tkscale(fr, command = norm.refresh, from = 0,
        to = length(x$rk), orient = "horiz", resolution = 1, showvalue = T),
        side = "left")
        
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = s2), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    #tkpack(tklabel(fr, text = "Xmin =   ", width = 6), side = "left")
    #tkpack(e <- tkentry(fr, width = 8), side = "left")
    #assign("e", e, env = slider.env)
    #evalq(tkconfigure(e, textvariable = xmin), env = slider.env)
    #tkpack(tklabel(fr, text = "Xmax =", width = 6), side = "left")
    #tkpack(e <- tkentry(fr, width = 8), side = "left")
    #assign("e", e, env = slider.env)
    #evalq(tkconfigure(e, textvariable = xmax), env = slider.env)
    #tkpack(fr <- tkframe(m), side = "top")
    #tkpack(tklabel(fr, text = "Ymin =   ", width = 6), side = "left")
    #tkpack(e <- tkentry(fr, width = 8), side = "left")
    #assign("e", e, env = slider.env)
    #evalq(tkconfigure(e, textvariable = ymin), env = slider.env)
    #tkpack(tklabel(fr, text = "Ymax =", width = 6), side = "left")
    #tkpack(e <- tkentry(fr, width = 8), side = "left")
    #assign("e", e, env = slider.env)
    #evalq(tkconfigure(e, textvariable = ymax), env = slider.env)
    #tkpack(tkbutton(m, text = "histogram", command = function(method){p1(method)}),
    #    side = "left")
    tkpack(tkbutton(m, text = "ecdf", command = function(){method="ecdf"; p1(method); abline(v=as.numeric(evalq(tclvalue(mu), env = slider.env)), col="blue")}), side="left")
    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)),
        side = "right")
        

}
