#'
#' Extract reference points
#' 
#' This function can be used to extract deterministic reference point values following a model fit.
#' 
#' This function will look for model elements \code{msy}, \code{depletion_at_msy}, \code{biomass_at_msy} and \code{harvest_rate_at_msy}. These are returned by the default \pkg{bdm} model, but the function will also work if a user-specified model contains these elements in the \code{'generated quantities {}'} block of the code. 
#' 
#' @param object a \code{bdm} model object
#' 
#' @return Returns a \code{list} containing the elements \code{msy}, \code{depletion_at_msy}, \code{biomass_at_msy} and \code{harvest_rate_at_msy}, each of which is a vector equal to the number of iterations.  
#' 
#' @examples
#' # get some data
#' data(hakcr)
#' dat <- edat(harvest = hakcr$catch,index = cbind(hakcr$survey, hakcr$cpue))
#' 
#' # initialize and fit default model
#' mdl <- bdm(compile = TRUE)
#' mdl <- fit(mdl, dat)
#' 
#' # check convergence
#' traceplot(mdl)
#' 
#' # get median reference point estimates
#' lapply(refpoints(mdl), median)
#' 
refpoints <- function(object, ...) UseMethod("refpoints")
#' @export
refpoints.bdm <- function(object) {
    return(list(msy = object@trace$msy,
                depletion_at_msy = object@trace$depletion_at_msy,
                biomass_at_msy = object@trace$biomass_at_msy,
                harvest_rate_at_msy = object@trace$harvest_rate_at_msy))
    }
#}}}
