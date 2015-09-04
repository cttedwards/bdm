
.onAttach <- function(libname, pkgname)
{
    # delete conflicting packages
    if ("package:compiler" %in% search()) {
        detach("package:compiler", unload = TRUE)
        message('detached package:compiler')
    }
    packageStartupMessage('loaded bdm package')
}


