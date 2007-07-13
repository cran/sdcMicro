.First.lib <- function(lib,pkg)
{
  library.dynam("sdcMicro",pkg,lib)
  cat("\n sdcMicro version 2.0.1 is already loaded \n")
}


