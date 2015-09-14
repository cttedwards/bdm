#'
#' Extract status and reference point estimates
#' 
#' These functions can be used to extract output values following a model fit.
#' The \code{status} function will look for and return model elements \code{current_biomass}, \code{current_depletion} and \code{current_harvest_rate}. 
#' The \code{refpoints} function will look for and return model elements \code{msy}, \code{depletion_at_msy}, \code{biomass_at_msy} and \code{harvest_rate_at_msy}.
#' These are returned by the default \pkg{bdm} model, but the function will also work if a user-specified model contains potentially different elements in the \code{'generated quantities {}'} block of the code, as long as they are specified in the \code{pars} argument. 
#' 
#' @param object a \code{bdm} model object
#' @param pars a \code{character} vector specifiying the parameters to be returned
#' @param ... additional arguments to the generic function
#' 
#' @return Returns a \code{list} containing the specified elements, each of which is a vector equal to the number of posterior samples.  
#' 
#' @examples
#' data(albio)
#' dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = rownames(albio))
#' 
#' \dontrun{
#' # initialize and fit default model
#' mdl <- bdm()
#' mdl <- compiler(mdl)
#' mdl <- sampler(mdl, dat)
#' 
#' # get median status estimates
#' lapply(status(mdl), median)
#' 
#' # get median reference point estimates
#' lapply(refpoints(mdl), median)
#' }
#' 
#' @include bdm-class.R
#' 
#{{{ status accessor function
#' @export
setGeneric("status", function(object, ...) standardGeneric("status"))
#' @rdname status
setMethod("status",signature("bdm"),
          function(object, pars) {
              
              status.list <- list()
              
              parnames <- names(object@trace)
              
              if (missing(pars)) {
                  pars <- c('current_biomass', 'current_depletion', 'current_harvest_rate')
              }
              
              for (i in 1:length(pars)) {
                  if (pars[i] %in% parnames) {
                      status.list[[pars[i]]] <- object@trace[[pars[i]]]
                  }
              }
              
              return(status.list)
          }
)
#}}}

#{{{ reference point accessor function
#' @rdname status
#' @export
setGeneric("refpoints", function(object, ...) standardGeneric("refpoints"))
#' @rdname status
setMethod("refpoints",signature("bdm"),
          function(object, pars) {
              
              refpoints.list <- list()
              
              parnames <- names(object@trace)
              
              if (missing(pars)) {
                  pars <- c('msy', 'depletion_at_msy', 'biomass_at_msy', 'harvest_rate_at_msy')
              }
              
              for (i in 1:length(pars)) {
                  if (pars[i] %in% parnames) {
                      refpoints.list[[pars[i]]] <- object@trace[[pars[i]]]
                  }
              }
              
              return(refpoints.list)
          }
)
#}}}

