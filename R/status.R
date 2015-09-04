#'
#' Extract status estimates
#' 
#' This function can be used to extract values for the current biomass, current depletion and current harvest rate following a model fit.
#' 
#' This function will look for model elements \code{current_biomass}, \code{current_depletion} and \code{current_harvest_rate}. These are returned by the default \pkg{bdm} model, but the function will also work if a user-specified model contains these elements in the \code{'generated quantities {}'} block of the code. 
#' 
#' @param object a \code{bdm} model object
#' 
#' @return Returns a \code{list} containing the elements \code{current_biomass}, \code{current_depletion} and \code{current_harvest_rate}, each of which is a vector equal to the number of iterations.  
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
#' # get median status estimates
#' lapply(status(mdl), median)
#' 
#{{{ reference point accessor function
#' @export
setGeneric("status", function(object, ...) standardGeneric("status"))
#' @rdname status
setMethod("status",signature("bdm"),
          function(object) {
            return(list(current_biomass=object@trace$current_biomass,
                        current_depletion=object@trace$current_depletion,
                        current_harvest_rate=object@trace$current_harvest_rate)
                   )
          }
)
#}}}
