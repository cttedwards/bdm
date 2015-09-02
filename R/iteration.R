#'
#' Extract single iteration from \code{rdat} object
#' 
#' Extracts a single iteration from an \code{rdat} object
#' 
#' @param object an \code{rdat} object
#' @param iter iteration to be extracted
#' 
#' @return An object of class \code{\link{rdatIter}} which contains a single iteration only.
#' 
#' @include rdatIter-class.R
#' @export
#' 
iteration <- function(object, ...) UseMethod("iteration")
#'
#' @rdname iteration
#' @export
#'
iteration.rdat <- function(object, iter = 1) {
    x <- new('rdatIter', amax = object@amax, sr = object@sr)
    x@lhdat <- lapply(object@lhdat, function(x) x[,iter]) 
    return(x)
}


