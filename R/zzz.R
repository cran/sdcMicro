.First.lib <- function(lib,pkg)
{
  library.dynam("sdcMicro",pkg,lib)
  cat("\n sdcMicro version 2.4.1 has been loaded \n\n")
  #cat("--------\n")
  #cat("Dear Ondrej Vozar, This is a new version of the package for you which \n")
  #cat("is not available on CRAN till now.\n")
  #cat("If you are experiencing any problems with the package \n")
  #cat("please, send a mail to \n\n")
  #cat("       templ@tuwien.ac.at \n\n")
  #cat("Have a nice day. \n")
  #cat("It would be also interesting for me to get in contact \n") 
  #cat("with any user of the package \n")
  cat("--------\n\n")
}



