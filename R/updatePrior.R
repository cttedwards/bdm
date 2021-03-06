#'
#' Update priors in \code{bdm} model
#' 
#' This function can be used to update the priors for \eqn{r} and \eqn{ln(K)}. 
#' 
#' The \pkg{bdm} package by default assumes that the prior on \eqn{r} is log-normal and the prior for \eqn{ln(K)} is uniform. If the function is supplied with a \code{\link[package:lhm]{prior}} class object then it will extract the log-normal distribution parameters for \eqn{r} and use regular expression matching to update the model code. If the function is provided with a named list then it can be used to update the priors for \eqn{r}, \eqn{ln(K)} or the initial depletion \eqn{x0}, in a similar manner. See the examples for how the list arguments are specified.
#' 
#' By default this function only updates the model code. The model will need to be re-compiled before it is run for the changes to take effect.
#' 
#' @param object a \code{bdm} class object
#' @param prior a \code{prior} class object or a \code{list} object containing information to update the prior
#' @param ... additional arguments to generic function
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
#' # update prior for logK
#' mdl <- updatePrior(mdl, list(par = 'logK', min = 1, max = 100))
#' 
#' # update prior for initial depletion
#' mdl <- updatePrior(mdl, list(par = 'x0', meanlog = log(0.8), sdlog = 0.01))
#' 
#' # check updates
#' getr(mdl)
#' getlogK(mdl)
#' 
#' # update using a prior class
#' # object
#' library(lhm)
#' 
#' # create object containing
#' # vector of r values
#' iter <- 100
#' mu <- 0.1
#' cv <- 0.2
#' sd <- sqrt(log(1+cv^2))
#' x <- rlnorm(iter,log(mu)-sd^2/2,sd)
#' r <- prior(x)
#' 
#' # update model
#' mdl <- updatePrior(mdl, r)
#' 
#' # check update
#' mean(log(r))
#' getr(mdl)[['E[log(r)]']]
#' 
#' @export
updatePrior <- function(object, ...) UseMethod("updatePrior")
#'
#' @rdname updatePrior
#' @export
updatePrior.bdm <-  function(object, prior, ...) {
    
    if (is.list(prior)) {
        if (prior$par == 'r') {
            mu    <- signif(prior$meanlog,3)
            sigma <- signif(prior$sdlog,3)
            object@model_code <- sub("r.?~.?lognormal\\(.+?\\);",paste("r ~ lognormal(",mu,",",sigma,");",sep = ""),object@model_code)
            object@model_code <- sub("rPrior.?=.?lognormal\\_rng\\(.+?\\);",paste("rPrior = lognormal_rng(",mu,",",sigma,");",sep = ""),object@model_code)
            
        }
        if (prior$par == 'logK') {
            low <- signif(prior$min,3)
            upp <- signif(prior$max,3)
            object@model_code <- sub("logK.?~.?uniform\\(.+?\\);",paste("logK ~ uniform(",low,",",upp,");",sep = ""),object@model_code)
            object@model_code <- sub("logKprior.?=.?uniform\\_rng\\(.+?\\);",paste("logKprior = uniform_rng(",low,",",upp,");",sep = ""),object@model_code)
            
        }
        if (prior$par == 'x0') {
            mu    <- signif(prior$meanlog,3)
            sigma <- signif(prior$sdlog,3)
            object@model_code <- sub("x\\[1\\].?~.?lognormal\\(.+?\\);",paste("x[1] ~ lognormal(",mu - (sigma^2)/2,",",sigma,");",sep = ""),object@model_code)
            
        }
        if (prior$par == 'sigmap2') {
            alpha <- signif(prior$alpha,3)
            beta  <- signif(prior$beta,3)
            object@model_code <- sub("sigmap2.?~.?inv_gamma\\(.+?\\);",paste("sigmap2 ~ inv_gamma(",alpha,",",beta,");",sep = ""),object@model_code)
            
        }
    } else if (class(prior) == 'prior') {
        
        lognormal.par <- prior@lognormal.par
        
        if (length(lognormal.par) == 0) {
            
            # transform to normal
            y <- log(prior@.Data)
            
            # estimate parameters of
            # normal distribution log(x)
            mu     <- mean(y)
            sigma  <- sd(y)
            sigma2 <- sigma^2
            
            # estimate parameters of
            # log-normal distribution
            theta <- exp(mu + sigma2/2)
            nu    <- exp(2*mu + sigma2)*(exp(sigma2) - 1)
            cv    <- sqrt(exp(sigma2) - 1)
            
            # assign
            lognormal.par <- list('E[log(r)]' = mu, 'SD[log(r)]' = sigma, 'E[r]' = theta, 'VAR[r]' = nu, 'CV[r]' = cv)
            
        } 
        
        logmu    <- signif(lognormal.par[['E[log(r)]']],3)
        logsigma <- signif(lognormal.par[['SD[log(r)]']],3)
        object@model_code <- sub("r.?~.?lognormal\\(.+?\\);",paste("r ~ lognormal(",logmu,",",logsigma,");",sep = ""),object@model_code)
        object@model_code <- sub("rPrior.?=.?lognormal\\_rng\\(.+?\\);",paste("rPrior = lognormal_rng(",logmu,",",logsigma,");",sep = ""),object@model_code)
    
    } else {
        stop('prior is incorrectly specified\n')
    }
    
    message('re-compile the model before running sampler()')
    
    return(object)
}
