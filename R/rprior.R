#' @title Initialise \code{rprior} class object for \code{bdm} package
#' 
#' @description An S4 object class containing a prior distribution for the intrinsic growth rate \eqn{r}. It includes both a numeric vector for values generated using Monte Carlo methods and a list of parameters describing the associated log-normal distribution.
#' 
#' @param x either an integer number or numeric vector
#' 
#' @details If \code{length(x)>1} then the function creates an object containing values of \code{x}, otherwise it creates a vector of zero's of length equal to \code{x}.
#' 
#' @examples
#' # create empty object
#' r <- rprior()
#'
#' # create object containing
#' # vector of r values
#' iter <- 100
#' mu <- 0.1
#' cv <- 0.2
#' sd <- sqrt(log(1+cv^2))
#' x <- rlnorm(iter,log(mu)-sd^2/2,sd)
#' r <- rprior(x)
#' 
#' @include rprior-initialize.R
#' @export
#'
#{{{
rprior <- function(x = 0L) new("rprior", x)
#}}}
