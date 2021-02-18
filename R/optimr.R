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
    object@data <- data
    object@run  <- run
    
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
            init <- list(r = rlnorm(1, init.r[['E[log(r)]']], init.r[['SD[log(r)]']]), logK = runif(1, init.logK[['min[logK]']], init.logK[['max[logK]']]), x = rbeta(length(init.x[['E[x]']]), 2, 2), sigmap = 0.05)
        } 
    }
    
    # maxium a posterior estimate using rstan
    list_object <- suppressWarnings(optimizing(object, data = object@data, init = init, as_vector = TRUE, ...))
    
    # extract estimates
    object@map <- list_object$par
    
    # return
    return(object)
})
