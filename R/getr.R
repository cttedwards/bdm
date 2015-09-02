#'
#' Extract log-normal parameters for the prior on intrinsic growth rate \eqn{r}
#' 
#' Can be applied to either a \code{bdm} or \code{rprior} class object. When applied to a \code{bdm} class object the function uses regular expression matching to extract log-normal distribution parameters for \eqn{r} from the model code. When applied to a \code{rprior} class object the function will estimate and return the distribution parameter values.
#' 
#' By default the \pkg{bdm} package assumes that the prior on intrinsic growth rate can be described by a log-normal distribution with parameters \eqn{\mu} and \eqn{\sigma}, which correspond to the mean and standard deviation of \eqn{\ln(r)}.
#' 
#' @param object an object of the appropriate class
#' 
#' @return A list containing the elements \code{'E[log(r)]'} (equal to \eqn{\mu}), \code{'SD[log(r)]'} (equal to \eqn{\sigma}), \code{'E[r]'}, \code{'VAR[r]'} and \code{'CV[r]'}.
#' 
#' @export
#' 
# S3 generic function
getr <- function(object, ...) UseMethod("getr")
#'
#' @rdname getr
#' 
#' @export
#' 
getr.bdm <- function(object) {
    
    # extract r from model_code
    
    m <- regexpr('r.?~.?lognormal\\(.+?\\)',object@model_code)
    x <- regmatches(object@model_code,m)
    
    m1 <- regexpr('\\(.+?\\,',x)
    m1 <- m1 + 1
    attributes(m1)$match.length <- attributes(m1)$match.length - 2
    x1 <- regmatches(x,m1)
    
    m2 <- regexpr('\\,.+?\\)',x)
    m2 <- m2 + 1
    attributes(m2)$match.length <- attributes(m2)$match.length - 2
    x2 <- regmatches(x,m2)
    
    # parameters of
    # normal distribution 
    # log(r) ~ N(mu, sigma2)
    mu     <- as.numeric(x1)
    sigma  <- as.numeric(x2)
    sigma2 <- sigma^2
    
    # parameters of
    # log-normal distribution
    # r ~ LN(theta, nu)
    theta <- exp(mu + sigma2/2)
    nu    <- exp(2*mu + sigma2)*(exp(sigma2) - 1)
    cv    <- sqrt(exp(sigma2) - 1)
    
    # return
    list('E[log(r)]' = mu, 'SD[log(r)]' = sigma, 'E[r]' = theta, 'VAR[r]' = nu, 'CV[r]' = cv)
    
}
#' 
#' @rdname getr
#' 
#' @export
#' 
getr.rprior <- function(object) {
    
    x <- object@.Data
    if (!length(x) > 2) {
        stop('rprior object should contain vector of r values')
    }
    
    if (length(object@lognormal.par) == 0) {
        
        # transform to normal
        y <- log(x)
        
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
        object@lognormal.par <- list('E[log(r)]' = mu, 'SD[log(r)]' = sigma, 'E[r]' = theta, 'VAR[r]' = nu, 'CV[r]' = cv)
        
    } 
    
    object@lognormal.par
    
}

