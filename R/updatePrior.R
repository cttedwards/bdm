#'
#' Update priors
#' 
#' This function can be used to update the priors for \eqn{r} and \eqn{ln(K)}. 
#' 
#' The \pkg{bdm} package by default assumes that the prior on \eqn{r} is log-normal and the prior for \eqn{ln(K)} is uniform. If the function is supplied with a \code{rprior} class object then it will extract the log-normal distribution parameters for \eqn{r} and use regular expression matching to update the model code. If the function is provided with a named list then it can be used to update the priors for either \eqn{r} or \eqn{ln(K)} in a similar manner. See the examples for how the list arguments are specified.
#' 
#' By default this function only updates the model code. The model will need to be re-compiled before it is run for the changes to take effect.
#' 
#' @include bdm-class.R
#' @include rprior-class.R
#' @include getr.R
#' @include compile.R
#' 
#' @param object a \code{bdm} class object
#' @param prior a \code{rprior} class object or a \code{list} object containing information to update the prior
#' @param compile a \code{logical} value indicating whether or not the code should be compiled
#' 
#' @return Returns a \code{bdm} object with updated model code.
#' 
#' @examples
#' # initialise default model
#' mdl <- bdm()
#' 
#' # update prior for r
#' mdl <- updatePrior(mdl, list(par = 'r', meanlog = -1.1, sdlog = 0.1))
#' 
#' update prior for logK
#' mdl <- updatePrior(mdl, list(par = 'logK', min = 1, max = 100))
#' 
#' # compile
#' mdl <- compile(mdl)
#' 
#' @export
updatePrior <- function(object, ...) UseMethod("updatePrior")
#'
#' @rdname updatePrior
#' @export
updatePrior.bdm <-  function(object, prior, compile = FALSE) {
    
    if (is.list(prior)) {
        if (prior$par == 'r') {
            logmu    <- signif(prior$meanlog,3)
            logsigma <- signif(prior$sdlog,3)
            object@model_code <- sub("r.?~.?lognormal\\(.+?\\);",paste("r ~ lognormal(",logmu,",",logsigma,");",sep = ""),object@model_code)
            
        }
        if (prior$par == 'logK') {
            low <- signif(prior$min,3)
            upp <- signif(prior$max,3)
            object@model_code <- sub("logK.?~.?uniform\\(.+?\\);",paste("logK ~ uniform(",low,",",upp,");",sep = ""),object@model_code)
            
        }
    } else {
        lognormal.par <- getr(prior)
        
        logmu    <- signif(lognormal.par[['E[log(r)]']],3)
        logsigma <- signif(lognormal.par[['SD[log(r)]']],3)
        object@model_code <- sub("r.?~.?lognormal\\(.+?\\);",paste("r ~ lognormal(",logmu,",",logsigma,");",sep = ""),object@model_code)
    }
    
    if (compile) {
        object <- compile(object)
    } else message('re-compile the model before running fit()')
    
    object
}
