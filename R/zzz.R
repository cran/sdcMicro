.First.lib <- function(lib,pkg)
{
  library.dynam("sdcMicro",pkg,lib)
  cat("\n--------\n\n")
  cat("\n", citation("sdcMicro")$note, "loaded.\n\n" )
  cat("--------\n\n")
}



