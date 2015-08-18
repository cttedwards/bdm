#' @title Initialize rdatIter object
#' 
#' @description Method to initialize rdatIter class object
#' 
#' @include rdatIter-class.R
#'
# initialisation function
setMethod("initialize", "rdatIter", function(.Object, amax, sr, ...) {
    
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
