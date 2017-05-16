#'
#' Extract an initial value for the carrying capacity
#' 
#' This function can be applied to a \code{list} or \code{bdmData} object to provide a rough estimate of \eqn{ln(K)}.
#' 
#' This function assumes a logistic production function and applies a grid search over \eqn{ln(K) = (3, 30)} using a least-squares measure of fit. The value for intrinsic growth must be provided as an input and would usually be obtained through a call to \code{\link[lhm:rCalc]{rCalc}} in the \pkg{lhm} package.
#' 
#' @param object either a \code{bdm} class object or a \code{list} object containing the elements \code{index} (an array) and \code{harvest} (a vector)
#' @param r an assumed value for the intrinsic growth rate \eqn{r}
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
#' # extract logK from
#' # bdm object
#' getlogK(mdl)
#' 
#' # extract with data
#' mdl@@data <- dat
#' getlogK(mdl)
#' 
#' # calculate logK from
#' # catches and assumed r
#' getlogK(dat, r = exp(-1))
#' 
#' @export
getlogK <- function(object, ...) UseMethod("getlogK")
#' 
#' @rdname getlogK
#' @export
getlogK.bdm <- function(object, ...) {
    
    r    <- getr(object)[['E[r]']]
	
    # extract logK distribution parameters from model_code
    m <- regexpr('logK.?~.?uniform\\(.+?\\)',object@model_code)
    x <- regmatches(object@model_code,m)
    
    m1 <- regexpr('\\(.+?\\,', x)
    m1 <- m1 + 1
    attributes(m1)$match.length <- attributes(m1)$match.length - 2
    x1 <- as.numeric(regmatches(x,m1))
    
    m2 <- regexpr('\\,.+?\\)', x)
    m2 <- m2 + 1
    attributes(m2)$match.length <- attributes(m2)$match.length - 2
    x2 <- as.numeric(regmatches(x,m2))
	
	# if there are data then estimate logK
    if (length(object@data) > 0) {
        logK <- getlogK(object@data, r, interval = c(x1, x2))
    } else logK <- NULL
    
    
    
    # return
    list('E[logK]' = logK, 'min[logK]' = x1, 'max[logK]' = x2)
    
}
#' @rdname getlogK
#' @export
getlogK.list <- function(object, r, interval, ...) {
    
    # get logK through grid search
    # assuming a logistic production model
    
    ii <- object$index
    cc <- object$harvest
    tt <- length(cc)
    bm <- numeric(tt)
    
    ll <- 1e-4
    
    # least-squares objective
    # function
    obj <- function(logK) {
        
        bm[1] <- 1
        for (t in 1:tt) 
            bm[t + 1] <- max(bm[t] + r*bm[t]*(1 - bm[t]) - cc[t]/exp(logK),ll)
        bm <- bm[-length(bm)]
        
        q <- mean(apply(ii, 2, function(x) exp(mean(log(x[x > 0]/bm[x > 0])))))
        
        sum(apply(ii, 2, function(x) sum(log(x[x > 0]/(q*bm[x > 0]))^2))) + ifelse(any(bm == ll), Inf, 0) + ifelse(bm[tt] > 0.99,Inf,0)
    }
    
    np <- 1000
    logK.llk <- numeric(np)
    logK.seq <- seq(interval[1],interval[2],length = np)
    for (i in 1:np) 
        logK.llk[i] <- obj(logK.seq[i])
		
	min.loc <- which.min(logK.llk)
    
	if (min.loc %in% c(1,np)) warning("estimate on prior bound") 
	
    init.logK <- logK.seq[min.loc]
    
    init.logK
}




