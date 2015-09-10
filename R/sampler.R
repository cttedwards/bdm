#'
#' Fit \code{bdm} model
#' 
#' Execute a Bayesian model fit using \pkg{rstan}.
#' 
#' By default a Bayesian fit is executed through a call to \code{\link[rstan:sampling]{sampling}}, which implements an MCMC algorithm. Default values for \code{chains}, \code{iter}, \code{warmup} and \code{thin} follow those for \pkg{rstan}.
#' 
#' @param object a \code{bdm} model object
#' @param data a \code{list} object containing the model inputs
#' @param init an initialisation function that should take no arguments and return a named list of intial values for the estimated parameters
#' @param chains number of MCMC chains
#' @param iter number of iterations per chain
#' @param warmup number of iterations to be discarded
#' @param thin sampling interval from chains
#' @param ... further arguments to \code{\link[rstan:sampling]{sampling}}
#' 
#' @return Returns a \code{bdm} object containing posterior samples contained in \code{object@@trace}.
#' 
#' @examples
#' library(bdm)
#' 
#' # get some data
#' data(albio)
#' dat <- list(T = nrow(albio), I = 1, 
#' harvest = albio$catch,
#' index = structure(albio$cpue, dim = c(nrow(albio), 1)))
#' 
#' # initialize and fit default model
#' \dontrun{
#' mdl <- bdm()
#' mdl <- compiler(mdl)
#' mdl <- sampler(mdl, dat)
#' }
#' 
#' @include bdm.R
#' 
#' @import methods
#' @import rstan
#' 
#' @export
setGeneric("sampler", function(object, ...) standardGeneric("sampler"))
#'
#' @rdname sampler
setMethod("sampler", signature = "bdm", definition = function(object, data = list(), init = function() list(r = exp(-1), logK = 6), chains, iter, warmup, thin, ...) {
  
  object@data <- data
    
  object@init <- init
  
  # non-default sampling dimensions
  if (!missing(iter)) {
      object@iter   <- iter
      if (missing(warmup)) 
          warmup <- floor(iter/2)
  }
  if (!missing(chains)) object@chains <- chains
  if (!missing(thin))   object@thin   <- thin
  if (!missing(warmup)) {
      if (warmup >= object@iter)
          stop('warmup must be < iter\n')
      object@warmup <- warmup
  }
  
  # number of posterior samples
  object@nsamples <- ((object@iter - object@warmup) * object@chains)/object@thin
    
  # mcmc-sampling using rstan
  stanfit_object <- suppressWarnings(sampling(object,data = object@data,init = object@init,iter = object@iter,chains = object@chains,warmup = object@warmup,thin = object@thin, ...))
    
  # extract traces
  object@trace       <- extract(stanfit_object)
  
  return(object)
})
