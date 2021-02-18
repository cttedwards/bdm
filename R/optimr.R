#'
#' Fit \code{bdm} model using maxium penalised density
#' 
#' Execute a Bayesian (MAP) model fit using \pkg{rstan}.
#' 
#' @return Returns a \code{bdm} object containing MAP estimates.
#' 
#' @examples
#' # get some data
#' data(albio)
#' dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = rownames(albio))
#' 
#' # initialize and fit default model
#' \dontrun{
#' mdl <- bdm()
#' mdl <- compiler(mdl)
#' mdl <- optimr(mdl, dat)
#' }
#' 
#' @include bdm.R
#' 
#' @import methods
#' @import rstan
#' 
#' @export
setGeneric("optimr", function(object, ...) standardGeneric("optimr"))
#'
#' @rdname sampler
setMethod("optimr", signature = "bdm", definition = function(object, data = list(), run = character(), init = "random", ...) {
    
    # initial assignments
    if (length(data) > 0) {
        if (length(object@data) > 0)
            warning("Replaced existent data in bdm model object")
        object@data <- data
    }
    if (length(run) > 0) {
        if (length(object@run) > 0)
            warning("Replaced existent run label in bdm model object (", object@run,") with ", run)
        object@run <- run
    }
    
    # check for default model
    # parameterisation
    pars <- getparams(object)
    pars <- setdiff(pars, names(object@data))
    default_pars <- setequal(pars, c('r', 'logK', 'x'))
    
    # initial values
    if (is.character(init) & default_pars) {
        
        init.r    <- getr(object)
        init.logK <- getlogK(object)
        init.x    <- getx(object)
        
        if (init == "fixed") {
            init <- list(r = init.r[['E[r]']], logK = init.logK[['E[logK]']], x = init.x[['E[x]']], sigmap = 0.05)
        } else if (init == "random") {
        
            # helper function
            ff <- function(y) rbeta(length(y), 10 * y, 10 * (1 - y))
            
            init <- list(r = rlnorm(1, init.r[['E[log(r)]']], init.r[['SD[log(r)]']]), logK = init.logK[['E[logK]']], x = ff(init.x[['E[x]']]), sigmap = 0.05)
        } 
    }
    
    # maxium a posterior estimate using rstan
    fit_object <- suppressWarnings(optimizing(object, data = object@data, init = init, as_vector = FALSE, ...))
    
    # extract estimates
    if (fit_object$return_code == 0) {
        object@map <- fit_object$par
    } else {
        warning("Optimization terminated with error")
    }
    
    # return
    return(object)
})
