#'
#' @title Estimate initial value for logK using prior mean value for the intrinsic growth rate
#' 
#' @export
#' 
#' @include getlogK-generic.R
#' 
getlogK.bdm <- function(.Object, r) {
    
    if (missing(r))
        r <- getr(.Object)
    
    # get logK through grid search
    # assuming a logistic production model
    
    ii <- .Object@data$index
    cc <- .Object@data$harvest
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

