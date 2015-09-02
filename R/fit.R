#'
#' Fit \code{bdm} model
#' 
#' Execute a model fit using \pkg{rstan}.
#' 
#' By default a Bayesian fit is executed through a call to \code{\link[rstan]{sampling}}, which implements an MCMC algorithm. The initial values are obtained through calls to \code{\link{getr}}, \code{\link{getlogK}} and \code{\link{getx}}. Default values for \code{chains}, \code{iter}, \code{warmup} and \code{thin} follow those for \pkg{rstan}.
#' 
#' @include bdm-class.R
#' 
#' @export
#' 
setGeneric("fit", function(.Object, data, ...) standardGeneric("fit"))
#'
#' @rdname fit
#' 
#' @param .Object a \code{bdm} model object
#' @param data an \code{edat} or \code{list} object containing the model inputs
#' @param init an initialisation function that should take no arguments and return a named list of intial values for the estimated parameters
#' @param chains number of MCMC chains
#' @param iter number of iterations per chain
#' @param warmup number of iterations to be discarded
#' @param thin sampling interval from chains
#' @param run optional character vector to label the run
#' @param method either \code{'MCMC'} to implement a call to \code{sampling} or \code{'MPD'} for a call to \code{optimizing}
#' @param ... further arguments to \code{sampling} or \code{optimizing}
#' 
#' @return Returns a \code{bdm} object containing parameter estimates.
#' 
#' @export
#' 
setMethod("fit", signature = c("bdm", "list"), function(.Object, data, init, chains, iter, warmup, thin, run, method = "MCMC", ...) {
  
  if (missing(data)) 
    stop('No data object supplied\n')
  
  # number of data indices
  # nix <- ifelse(length(dim(data$index))>1,dim(data$index)[2],1)
  
  if (!missing(run))
    .Object@run <- as.character(run) 
  
  # check data
  if (any(data$harvest < 0) | any(is.na(data$harvest))) 
    stop('missing catch (harvest) data is not allowed\n')
  .Object@data <- lapply(data,function(x) x)
    
  # set initialisation function
  if (.Object@default_model) {
    
    # default initialisation function
    init.func <- function() { 
      
      b    <- init.r/exp(init.logK)
      r    <- init.r * rlnorm(1,log(1)-0.04/2,0.2)
      logK <- max(min(log(r/b),30),3)
      
      x    <- getx(.Object, r, logK)
      
      init.values <- list(logK    =  logK,
                          r       =  r,
                          x       =  x)
      
      init.values
    }     
    
    if (missing(init)) {
      
      init.r    <- getr(.Object)[['E[r]']]
      init.logK <- getlogK(.Object, r  =  init.r)
  	
    } else {
      if (is.list(init)) {
          if (!is.null(init$r))    init.r    <- init$r    else init.r    <- getr(.Object)[['E[r]']]
    	  if (!is.null(init$logK)) init.logK <- init$logK else init.logK <- getlogK(.Object, r  =  init.r)
      }
      if (is.function(init)) {
        init.func <- init
      }
    }
  } else {
    # non-default settings
    if (missing(init))
      stop('must supply initialisation function for non-default model\n')
  	if (is.function(init)) {
  		init.func <- init
  	} else stop('initialisation function must be supplied\n')
  }
    
  # update .Object
  .Object@init.func <- init.func
  
  # sampling dimensions
  if (!missing(chains))    .Object@chains <- chains
  if (!missing(iter))      .Object@iter   <- iter
  if (!missing(thin))      .Object@thin   <- thin
  if (!missing(warmup))    .Object@warmup <- warmup else .Object@warmup <- floor(.Object@iter/2/.Object@thin)
  
  # number of posterior samples
  .Object@nsamples <- ((.Object@iter - .Object@warmup) * .Object@chains)/.Object@thin

  if (method == 'MCMC') {
    
    # initiate mcmc-sampling
    cat('MCMC sampling\n')
    stanfit_object <- suppressWarnings(sampling(.Object,data = .Object@data,init = .Object@init.func,iter = .Object@iter,chains = .Object@chains,warmup = .Object@warmup,thin = .Object@thin, ...))
    
    # extract traces
    .Object@trace_array <- extract(stanfit_object,permuted = FALSE,inc_warmup = TRUE)
    .Object@trace       <- extract(stanfit_object)
    
    # record initial values for each chain
    .Object@init.values <- stanfit_object@inits
  }
  if (method == 'MPD') {
    
    # initiate mpd fit
    cat('MPD optimisation\n')
    .Object@mpd <- optimizing(.Object,data = .Object@data,init = .Object@init.func, ...)
  }
  
  .Object
})
