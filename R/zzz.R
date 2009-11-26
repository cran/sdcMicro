.First.lib <- function(lib,pkg)
{
  library.dynam("sdcMicro",pkg,lib)
  cat("\n--------\n\n")
  cat("\n", citation("sdcMicro")$note, "has been loaded.\n\n" )
  cat("--------\n\n")
  cat("\n If you want to work with the GUI of sdcMicro GUI, type \n" )  
  cat("\n sdcGUI()\n\n" )  
  cat("--------\n\n")
}



