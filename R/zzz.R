.First.lib <- function(lib,pkg)
{
  library.dynam("sdcMicro",pkg,lib)
  cat("\n sdcMicro version 2.5.0 has been loaded \n\n")

  cat("--------\n\n")
}



