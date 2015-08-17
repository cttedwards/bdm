#'
#' @title Extract prior log-normal parameters for the intrinsic growth rate from a bdm object
#' 
#' @export
#' 
#' @include getr-generic.R
#' 
# S3 method for S4 bdm class object
getr.bdm <- function(.Object) {
    
    # extract r from model_code
    
    m <- regexpr('r.?~.?lognormal\\(.+?\\)',.Object@model_code)
    x <- regmatches(.Object@model_code,m)
    
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

