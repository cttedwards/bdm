#'
#' Estimate an initial vector of depletion values
#' 
#' This function can be applied to either a \code{bdm} or \code{edat} class object to provide a rough estimate of the depletion time series \eqn{x}. 
#' 
#' For both \code{bdm} and \code{edat} class objects this function assumes a logistic production function and uses input values for the intrinsic growth and carrying capacity. These would usually be obtained through calls to \code{\link{getr}} and \code{\link{getlogK}}.
#'
#' @param object an object of the appropriate class
#' @param r an assumed value for the intrinsic growth rate \eqn{r}
#' @param logK an assumed value for the carrying capacity \eqn{ln(K)}
#' 
#' @export
#' 
# S3 generic function
getx <- function(object, ...) UseMethod("getx")
#' 
#' @rdname getx
#' @export
#' 
getx.bdm <- function(object, r, logK) {
    
    if (missing(r))
        r <- getr(object)
    if (missing(logK))
        logK <- getlogK(object, r)
    
    cc <- object@data$harvest
    tt <- length(cc)
    bm <- numeric(tt)
    
    ll <- 1e-4
    
    bm[1] <- 1
    for (t in 1:tt) 
        bm[t+1] <- max(bm[t] + r*bm[t]*(1 - bm[t]) - cc[t]/exp(logK),ll)
    bm <- bm[-length(bm)]
    
    init.x <- bm
    
    init.x
}
#' 
#' @rdname getx
#' @export
#' 
getx.edat <- function(object, r, logK) {
    
    cc <- object$harvest
    tt <- length(cc)
    bm <- numeric(tt)
    
    ll <- 1e-4
    
    bm[1] <- 1
    for (t in 1:tt) 
        bm[t+1] <- max(bm[t] + r*bm[t]*(1 - bm[t]) - cc[t]/exp(logK),ll)
    bm <- bm[-length(bm)]
    
    init.x <- bm
    
    init.x
}





