.onLoad <- function(lib,pkg)
{
  #library.dynam("sdcMicro",pkg,lib)
  #cat("\n------------------------------\n\n")
  #print(citation("sdcMicro"))
  #print(citEntry(entry="Article",
  #            title = "Statistical Disclosure Control for Microdata Using the R-Package sdcMicro",
  #            author = personList(as.person("Matthias Templ")),
  #            journal = "Transactions on Data Privacy",
  #            year = "2008",
  #            volume = "1",
  #            number = "2",
  #            pages = "67--85",
  #            textVersion =
  #            paste("Matthias Templ",
  #                  " (2008). Statistical Disclosure Control for Microdata Using the R-Package sdcMicro. Transactions on Data Privacy, 1(2),67-85 ",
  #                  ".", sep="")))
	packageStartupMessage("\n--------\n\n")   
	packageStartupMessage("for references have a look at\n")
	packageStartupMessage("citation('sdcMicro')    \n")                 
	packageStartupMessage("\n Note that since version 2.6.6. the graphical user-interface is provided by package sdcMicroGUI.\n")
	packageStartupMessage("--------\n\n")  
   
}



