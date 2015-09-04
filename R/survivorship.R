#'
#' Survivorship accessor function
#' 
#' This function can be used to access the survivorship at age contained within a \code{rdat} object. The survivorship is calculated following an assignment of the natural mortality (using \code{\link{nmort}}) and assumed to be one for the first age class. 
#' 
#' @param object a \code{rdat} class object
#' 
#' @examples
#' # M at age vector input
#' dat <- rdat(amax = 10,iter=1)
#' nmort(dat) <- c(0.1,0.1,0.2)
#' nmort(dat)
#' 
#' # extract survivorship
#' survivorship(dat)
#' 
#{{{ accessor function
setGeneric("survivorship", function(object, ...) standardGeneric("survivorship"))
#' @export
setMethod("survivorship",signature(object = "rdat"), function(object) return(object@lhdat$survivorship))
#}}}
