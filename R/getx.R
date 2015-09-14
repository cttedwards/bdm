#'
#' Estimate an initial vector of depletion values
#' 
#' This function can be applied to a \code{list} or \code{bdmData} object to provide a rough estimate of the depletion time series \eqn{x}. 
#' 
#' This function assumes a logistic production function and uses input values for the intrinsic growth and carrying capacity. These would usually be obtained through calls to \code{\link[lhm:rCalc]{rCalc}} in the \pkg{lhm} package and \code{\link{getlogK}}.
#'
#' @param object either a \code{bdm} class object or a \code{list} object containing a \code{harvest} vector
#' @param r an assumed value for the intrinsic growth rate \eqn{r}
#' @param logK an assumed value for the carrying capacity \eqn{ln(K)}
#' @param ... additional arguments to generic function
#' 
#' @export
getx <- function(object, ...) UseMethod("getx")
#' 
#' @rdname getx
#' @export
getx.bdm <- function(object) {
    
    r    <- getr(object)[['E[r]']]
    logK <- getlogK(object@data, r)
    x    <- getx(object@data, r, logK)
    
    # return
    list('E[x]' = x)
}

#' @rdname getx
#' @export
getx.list <- function(object, r, logK) {
    
    cc <- object$harvest
    tt <- length(cc)
    bm <- numeric(tt)
    
    ll <- 1e-4
    
    bm[1] <- 1
    for (t in 1:tt) 
        bm[t + 1] <- max(bm[t] + r*bm[t]*(1 - bm[t]) - cc[t]/exp(logK),ll)
    bm <- bm[-length(bm)]
    
    init.x <- bm
    
    init.x
}





