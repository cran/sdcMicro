.First.lib <- function(lib,pkg)
{
  library.dynam("sdcMicro",pkg,lib)
  cat("\n sdcMicro version 2.0.4 is loaded \n\n")
  cat("--------\n")
  cat("If you are experiencing any problems with the package \n")
  cat("please, send a mail to \n\n")
  cat("       templ@statistik.tuwien.ac.at \n\n")
  cat("It would be also interesting for me to get in contact \n") 
  cat("with any user of the package \n")
  cat("--------\n\n")
}



