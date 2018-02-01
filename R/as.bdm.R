#'
#' Convert a \code{stanfit} object into a \code{bdm} object
#' 
#' Useful for producing plots with the \pkg{bdm} package functions such as \code{\link{histplot}}
#' #' 
#' @param x \code{stanfit} class object.
#' @param ... additional arguments to the generic function
#' 
#' @return Returns a \code{bdm} object containing MCMC outputs
#' 
#' @include bdm.R
#' 
#' @export
as.bdm <- function(object, ...) UseMethod("as.bdm")
#'
#' @rdname as.bdm
#' @export
as.bdm.stanfit <- function(object, ...) {
    
    bdm.object <- bdm()
    
    bdm.object@thin        <- object@sim$thin
    bdm.object@warmup      <- object@sim$warmup
    bdm.object@chains	   <- object@sim$chains
    bdm.object@nsamples    <- ((object@sim$iter - object@sim$warmup) * object@sim$chains)/object@sim$thin
    
    bdm.object@trace       <- extract(object)
    bdm.object@trace_array <- extract(object,permuted = FALSE,inc_warmup = TRUE)
    
    return(bdm.object)
    
}
