#'
#' Class containing a prior distribution for the intrinsic growth rate \eqn{r}
#' 
#' This is an S4 object class that includes both a numeric vector for storage of \eqn{r} values generated using Monte Carlo methods, and a list of parameters describing the associated log-normal distribution.
#' 
#' If \code{length(x)>1} then the function creates an object containing values of \code{x}, otherwise it creates a vector of zero's of length equal to \code{x}. Values for \eqn{r} can be simulated directly or generated using the \code{\link{rcalc}} function. The class contains an additional slot to hold parameters of the log-normal distribution, which is used by \pkg{bdm} to describe the prior for \eqn{r}.
#' 
#' @slot .Data numeric vector of \eqn{r} values
#' @slot lognormal.par log-normal distribution parameter values
#'
setClass("rprior", contains = "numeric", slots = list(lognormal.par = "list"))