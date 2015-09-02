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
#' @export
#' 
setGeneric("update_prior", function(.Object, x, ...) standardGeneric("update_prior"))
#'
#' @rdname update_prior
#' 
#' @param .Object a \code{bdm} class object
#' @param x a \code{rprior} class object or a \code{list} object containing information to update the prior
#' @param compile a \code{logical} value indicating whether or not the code should be compiled
#' 
#' @return Returns a \code{bdm} object with updated model code.
#' 
#' @export
#'
setMethod("update_prior", signature = c("bdm", "rprior"), function(.Object, x, compile = FALSE) {
    
    lognormal.par <- getr(x)
    
    logmu    <- signif(lognormal.par[['E[log(r)]']],3)
    logsigma <- signif(lognormal.par[['SD[log(r)]']],3)
    .Object@model_code <- sub("r.?~.?lognormal\\(.+?\\);",paste("r ~ lognormal(",logmu,",",logsigma,");",sep = ""),.Object@model_code)
    
    #if (plot) {
    #  windows()
    #  hist(x, freq = FALSE, xlab = 'r', ylab='', yaxt='n', main='')
    #  curve(dlnorm(x,logmu,logsigma),col = 2,add = T)
    #}
    
    if (compile) {
      .Object <- compile(.Object)
    } else message('re-compile the model before running fit()')
    
    .Object
})
#'
#' @rdname update_prior
#' 
#' @examples
#' # initialise default model
#' mdl <- bdm()
#' 
#' # update prior for r
#' mdl <- update_prior(mdl, list(par = 'r', meanlog = -1.1, sdlog = 0.1))
#' 
#' update prior for logK
#' mdl <- update_prior(mdl, list(par = 'logK', min = 1, max = 100))
#' 
#' # compile
#' mdl <- compile(mdl)
#' 
#' @export
#'
setMethod("update_prior", signature = c("bdm", "list"),function(.Object, x, compile = FALSE) {
  
  if (x$par == 'r') {
    logmu    <- signif(x$meanlog,3)
    logsigma <- signif(x$sdlog,3)
    .Object@model_code <- sub("r.?~.?lognormal\\(.+?\\);",paste("r ~ lognormal(",logmu,",",logsigma,");",sep = ""),.Object@model_code)
    
    #if (plot) {
    #  windows()
    #  curve(dlnorm(x,logmu,logsigma),col=2,from=0,to=2,xlab='r',ylab='',yaxt='n',main='')
    #}
  }
  #if (x$par=='sigRsq') {
  #  alpha <- signif (x$a,3)
  #  beta  <- signif (x$b,3)
  #  .Object@model_code <- sub("sigRsq.?~.?inv_gamma\\(.+?\\);",paste("sigRsq ~ inv_gamma(",alpha,",",beta,");",sep=""),.Object@model_code)
  #  
  #  if (plot) {
  #    dinvgamma <- function(x,alpha,beta) (beta^alpha / gamma(alpha)) * (1/(x^(alpha+1))) * exp(-beta / x)
  #    windows()
  #    curve(dinvgamma(x,alpha,beta),col=2,from=0,to=2,xlab='Observation error (sigRsq)',ylab='',yaxt='n',main='')
  #    abline(v=beta/(alpha + 1),col=2,lty=2)
  #  }
  #}
  #if (x$par=='sigQsq') {
  #  alpha <- signif (x$a,3)
  #  beta  <- signif (x$b,3)
  #  .Object@model_code <- sub("sigQsq.?~.?inv_gamma\\(.+?\\);",paste("sigQsq ~ inv_gamma(",alpha,",",beta,");",sep=""),.Object@model_code)
  #  
  #  if (plot) {
  #    dinvgamma <- function(x,alpha,beta) (beta^alpha / gamma(alpha)) * (1/(x^(alpha+1))) * exp(-beta / x)
  #    windows()
  #    curve(dinvgamma(x,alpha,beta),col=2,from=0,to=2,xlab='Process error (sigQsq)',ylab='',yaxt='n',main='')
  #    abline(v=beta/(alpha + 1),col=2,lty=2)
  #  }
  #}
  if (x$par == 'logK') {
    low <- signif(x$min,3)
    upp <- signif(x$max,3)
    .Object@model_code <- sub("logK.?~.?uniform\\(.+?\\);",paste("logK ~ uniform(",low,",",upp,");",sep = ""),.Object@model_code)
    
    #if (plot) {
    #  windows()
    #  curve(dunif(x,low,upp),from=max(low-mean(c(low,upp))/4,0),to=upp+mean(c(low,upp))/4,col=2,xlab='logK',ylab='',yaxt='n',main='')
    #}
  }
  
  if (compile) {
    .Object <- compile(.Object)
  } else message('re-compile the model before running fit()')
  
  .Object
})
#}}
#}}}
