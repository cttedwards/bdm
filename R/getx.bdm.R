#'
#' @title Estimate initial depletion values
#' 
#' @export
#' 
#' @include getx-generic.R
#' 
getx.bdm <- function(.Object, r, logK) {
    
    if (missing(r))
        r <- getr(.Object)
    if (missing(logK))
        logK <- getlogK(.Object, r)
    
    cc <- .Object@data$harvest
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



