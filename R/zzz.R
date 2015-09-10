.onAttach <- function(libname, pkgname)
{
# delete conflicting packages
if ("package:compiler" %in% search()) {
    detach("package:compiler", unload = TRUE)
    packageStartupMessage("detached package:compiler")
}
packageStartupMessage("bdm beta version 1.0 (2015-09-08 17:44:29)")
}
