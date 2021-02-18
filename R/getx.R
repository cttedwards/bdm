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
#' @examples
#' # get some data
#' data(albio)
#' dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = rownames(albio))
#' 
#' # default model
#' mdl <- bdm()
#' 
#' # extract depletion from
#' # bdm object
#' getx(mdl)
#' 
#' # extract with data
#' mdl@@data <- dat
#' getx(mdl)
#' 
#' # calculate depletion from
#' # catches and assumed parameters
#' getx(dat, logK = 6, r = exp(-1))
#' 
#' 
#' @export
getx <- function(object, ...) UseMethod("getx")
#' 
#' @rdname getx
#' @export
getx.bdm <- function(object, ...) {
    
    r    <- getr(object)
    logK <- getlogK(object)
    if (!is.null(logK[['E[logK]']])) {
        x <- getx(object@data, r[['E[r]']], logK[['E[logK]']])
    } else x <- NULL
    
    # return
    list('E[x]' = x)
}

#' @rdname getx
#' @export
getx.list <- function(object, r, logK, ...) {
    
    cc <- object$harvest
    tt <- length(cc)
    bm <- numeric(tt)
    
    ll <- 1e-4
    
    bm[1] <- 1
    for (t in 1:tt) {
        H = min(exp(log(cc[t]) - logK), bm[t])
        bm[t + 1] <- bm[t] + r*bm[t]*(1 - bm[t]) - H
    }
    bm <- bm[-length(bm)]
    
    init.x <- bm
    
    return(init.x)
}





