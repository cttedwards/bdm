#' On load hook
#' 
.onLoad <- function(libname, pkgname)
{
  cat("\n")
  cat("==============================================\n")
  cat("Rock lobster stock assessment plotting library\n")
  cat(lobview.version())
  cat("\nFor help contact darcy@quantifish.co.nz\n")
  cat("==============================================\n")
  cat("\n")
}


