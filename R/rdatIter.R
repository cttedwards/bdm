#' @rdname rdatIter-class
#' 
#' @param amax maximum age
#' @param ... further arguments listed in \code{\link{rdat}}
#' 
#' @examples
#' # initialise rdat object
#' amax <- 100
#' iter <- 1
#' dat <- rdat(amax,iter)
#' 
#' # assign life-history data
#' nmort(dat)    <- list(mu=list(M=0.18))
#' size(dat)     <- list(mu=list(Linf=106.5,k=0.229,t0=0.01)) 
#' mass(dat)     <- list(mu=list(a=1.7e-9,b=3.328))
#' sr(dat)       <- list(type='BH',mu=list(h=0.9))
#' maturity(dat) <- list(mu=list(acrit=8))
#'
#' # extract iteration
#' dat <- iteration(dat)
#' 
#' # calculate r
#' rcalc(dat)
#' 
#' @include rdatIter-class.R
#'
#' @export
rdatIter <- function(amax, ...) new("rdatIter", amax, ...)
setMethod("initialize", "rdatIter", function(.Object, amax, nmort, growth, mass, sr, maturity) {
    
    if (!missing(amax)) {
        
        .Object@amax <- amax
        .Object@lhdat[['survivorship']] <- vector('numeric',amax)
        .Object@lhdat[['M']]            <- vector('numeric',amax)
        .Object@lhdat[['size']]         <- vector('numeric',amax)
        .Object@lhdat[['mass']]         <- vector('numeric',amax)
        .Object@lhdat[['maturity']]     <- vector('numeric',amax)
        
    }
    
    if (!missing(sr)) {
        .Object@sr           <- sr
        .Object@lhdat[['h']] <- numeric(1)
    }
    
    .Object
    
})
