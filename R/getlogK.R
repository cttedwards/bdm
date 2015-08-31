#'
#' Extract an initial value for the carrying capacity
#' 
#' This function can be applied to either a \code{bdm} or \code{edat} class object to provide a rough estimate of \eqn{ln(K)}. 
#' 
#' For both \code{bdm} and \code{edat} class objects this function assumes a logistic production function and applies a grid search over \eqn{ln(K) = (3, 30)} using a least-squares measure of fit. The value for intrinsic growth must be provided as an input and would usually be obtained through a call to \code{\link{getr}}.
#' 
#' @param object an object of the appropriate class
#' @param r an assumed value for the intrinsic growth rate \eqn{r}
#' @export
#' 
# S3 generic function
getlogK <- function(object, ...) UseMethod("getlogK")
#' 
#' @rdname getlogK
#' @export
#' 
getlogK.bdm <- function(object, r) {
    
    if (missing(r))
        r <- getr(object)
    
    # get logK through grid search
    # assuming a logistic production model
    
    ii <- object@data$index
    cc <- object@data$harvest
    tt <- length(cc)
    bm <- numeric(tt)
    
    ll <- 1e-4
    
    # least-squares objective
    # function
    obj <- function(logK) {
        
        bm[1] <- 1
        for (t in 1:tt) 
            bm[t+1] <- max(bm[t] + r*bm[t]*(1 - bm[t]) - cc[t]/exp(logK),ll)
        bm <- bm[-length(bm)]
        
        q <- mean(apply(ii,2,function(x) exp(mean(log(x[x>0]/bm[x>0])))))
        
        sum(apply(ii,2,function(x) sum(log(x[x>0]/(q*bm[x>0]))^2))) + ifelse(any(bm == ll),Inf,0) + ifelse(bm[tt]>0.99,Inf,0)
    }
    
    np <- 1000
    logK.llk <- numeric(np)
    logK.seq <- seq(3,30,length = np)
    for (i in 1:np) 
        logK.llk[i] <- obj(logK.seq[i])
    
    init.logK <- logK.seq[which.min(logK.llk)]
    
    init.logK
}
#' 
#' @rdname getlogK
#' @export
#' 
getlogK.edat <- function(object, r) {
    
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
        for(t in 1:tt) 
            bm[t+1] <- max(bm[t] + r*bm[t]*(1 - bm[t]) - cc[t]/exp(logK),ll)
        bm <- bm[-length(bm)]
        
        q <- mean(apply(ii,2,function(x) exp(mean(log(x[x>0]/bm[x>0])))))
        
        sum(apply(ii,2,function(x) sum(log(x[x>0]/(q*bm[x>0]))^2))) + ifelse(any(bm==ll),Inf,0) + ifelse(bm[tt]>0.99,Inf,0)
    }
    
    np <- 1000
    logK.llk <- numeric(np)
    logK.seq <- seq(3,30,length=np)
    for(i in 1:np) 
        logK.llk[i] <- obj(logK.seq[i])
    
    init.logK <- logK.seq[which.min(logK.llk)]
    
    init.logK
}




