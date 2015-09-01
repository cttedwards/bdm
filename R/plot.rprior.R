#'
#' Plot prior for the intrinsic growth rate \eqn{r}
#' 
#' This function provides a simple \code{plot} method for the \code{rprior} object class.
#' 
#' @param object an \code{\link{rprior}} object class
#' @param ... additional arguments for \code{\link[graphics]{hist}}
#' 
#' @examples
#' # create object containing
#' # vector of r values
#' iter <- 100
#' mu <- 0.1
#' cv <- 0.2
#' sd <- sqrt(log(1+cv^2))
#' x <- rlnorm(iter,log(mu)-sd^2/2,sd)
#' r <- rprior(x)
#' 
#' # plot
#' plot(r)
#' 
#' @include ggtheme.R
#' 
#' @method plot edat
#' @export
plot.rprior <- function(object, ...)
{
  logmu    <- object@lognormal.par[['E[log(x)]']]
  logsigma <- object@lognormal.par[['SD[log(x)]']]
  
  hist(object@.Data, freq = FALSE, xlab = 'r', ylab = '', yaxt = 'n', main = '', ...)
  curve(dlnorm(x, logmu, logsigma), col = 2, lwd = 2, add = TRUE)
}