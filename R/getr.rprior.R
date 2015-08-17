#'
#' @title Extract prior log-normal parameters for the intrinsic growth rate from an rprior object
#' 
#' @export
#' 
#' @include getr-generic.R
#' 
# S3 method for S4 rprior class object
getr.rprior <- function(.Object) {
    
    x <- .Object@.Data
    if (!length(x) > 2) stop('need >2 r values')
    
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
    
    # return
    list('E[log(r)]' = mu, 'SD[log(r)]' = sigma, 'E[r]' = theta, 'VAR[r]' = nu, 'CV[r]' = cv)
    
}
