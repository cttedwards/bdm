#'
#' Add iterations to existing \code{rdat} object
#' 
#' This function will add an iteration dimension to the life-history data contained in an \code{rdat} object.
#' 
#' @param object existing \code{rdat} object
#' @param iter number of iterations
#' 
#' @return Returns an \code{rdat} object with life-history data replicated \code{iter} times
#' 
#' @examples
#' # initialise data object for calculation of r
#' dat <- rdat(amax = 30, iter = 1)
#' 
#' # to generate monte-carlo iterations first either 
#' # re-initialise 'rdat' object using more iterations or 
#' # iterate existing object:
#' dat <- iterate(dat, iter = 200)
#' 
#' @include rdat-class.R
#'
#' @export
setGeneric("iterate", function(object, iter, ...) standardGeneric("iterate"))
#'
#' @rdname iterate
setMethod("iterate", signature(object = "rdat",iter = "numeric"),
          function(object, iter) new('rdat', rdat = object, iter = iter)
)
