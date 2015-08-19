#'
#' @title Plot rprior
#' 
#' @method plot rprior
#' @export
plot.rprior <- function(.Object, ...)
{
  logmu    <- .Object@lognormal.par[['E[log(x)]']]
  logsigma <- .Object@lognormal.par[['SD[log(x)]']]
  
  hist(.Object@.Data,freq = FALSE,xlab = 'r', ylab = '', yaxt = 'n', main = '')
  curve(dlnorm(x, logmu, logsigma), col = 2, lwd = 2, add = TRUE)
}