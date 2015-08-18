#'
#' @title Extract single iteration from rdat object
#' 
#' @export
#' 
#' @include iteration-generic.R
#' 
# S3 method for S4 rdat class object
iteration.rdat <- function(.Object, i) {
    x <- rdatIter(amax = .Object@amax, sr = .Object@sr)
    x@lhdat <- lapply(.Object@lhdat, function(x) x[,i]) 
    return(x)
}


