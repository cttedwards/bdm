#' Class definition for \code{bdm} object
#' 
#' This is the primary object class for the \pkg{bdm} package. It inherits from the \code{\link[rstan:stanmodel-class]{stanmodel}} superclass.
#'
#' @slot data a \code{list} of input data
#' @slot inits a \code{list} of initial values
#' @slot chains the number of chains
#' @slot iter the number of samples for each chain
#' @slot warmup the 'burnin' period for each chain
#' @slot thin the period of sampling from each chain
#' @slot nsamples the resultant number of samples retained from the sample run
#' @slot trace \code{list} holding the processed samples from \code{\link[rstan:extract]{extract(..., permuted = TRUE, inc_warmup = FALSE)}}.
#' @slot trace_array \code{array} holding the processed samples from \code{\link[rstan:extract]{extract(..., permuted = FALSE, inc_warmup = TRUE)}}. This should rarely be needed for direct access but is used for diagnostic plots.
#' @slot model_name \code{character} string giving the model name.
#' @slot model_code the model code in the Stan modelling language.
#' @slot model_cpp translation of Stan code into C++.
#' @slot dso object of class \code{cxxdso} holding the compiled C++ code as a dynamic shared object.
#' 
# @importClassesFrom rstan stanmodel
#'
#' @import methods
#' @import rstan
#' 
#{{{
setClass("bdm",contains = "stanmodel",
         slots = list(data = "list",          # edat object or list
                    inits = "list",           # initial values populated by initialisation function
                    chains = "numeric",       # number of MCMC chains
                    iter = "numeric",         # number of MCMC iterations per chain
                    warmup = "numeric",       # number of iterations under adaptive sampling
                    thin = "numeric",         # interval between recorded samples
                    nsamples = "numeric",     # total number of posterior samples recorded
                    trace = "list",           # list of posterior samples without warmup and with chains mixed
                    trace_array = "array",    # array of posterior samples including warmup
                    run = "character"         # optional label for this particular run
         )
)
#}}}
#{{{
setMethod("initialize", signature = "bdm", definition = function(.Object, path, model.code, model.name) {
    
    if (!missing(path)) {
        .Object@model_name <- model.name
        .Object@model_code <- paste(readLines(path, warn = FALSE), collapse = '\n')
    }
    if (!missing(model.code)) {
        .Object@model_name <- model.name
        .Object@model_code <- model.code
    }
    
    .Object@chains <- 4
    .Object@iter   <- 2000
    .Object@thin   <- 1
    .Object@warmup <- floor(.Object@iter/2)
    
    return(.Object)
    
})
#}}}
#{{{
setMethod("show", "bdm",
          function(object) {
            cat("bdm S4 object class '", object@model_name, "' coded as follows:\n" ,sep = '') 
            cat(object@model_code, "\n")
          })
#}}}