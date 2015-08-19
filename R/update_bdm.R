#'
#' @title Update function
#' 
#' @export
#{{{ update prior distributions for r or logK in bdm model object
setGeneric("update_bdm", function(.Object,x, ...) standardGeneric("update_bdm"))
#{{ rprior object
setMethod("update_bdm",signature=c("bdm","rprior"),function(.Object,x,compile=FALSE,plot=FALSE) {
  
  if(length(x@lognormal.par)==0)
    x <- .fitr(x)
  
  logmu    <- signif(x@lognormal.par[['E[log(x)]']],3)
  logsigma <- signif(x@lognormal.par[['SD[log(x)]']],3)
  .Object@model_code <- sub("r.?~.?lognormal\\(.+?\\);",paste("r ~ lognormal(",logmu,",",logsigma,");",sep=""),.Object@model_code)
  
  if(plot) {
    windows()
    hist(x,freq=FALSE,xlab='r',ylab='',yaxt='n',main='')
    curve(dlnorm(x,logmu,logsigma),col=2,add=T)
  }
  
  if(compile)
    .Object <- compile(.Object)
  
  .Object
})
#}}
#{{ list object
setMethod("update_bdm",signature=c("bdm","list"),function(.Object,x,compile=FALSE,plot=FALSE) {
    
  if(length(.Object@model_code)==0)
    stop('no model code in bdm object: @model_code is empty\n')
  
  if(x$par=='r') {
    logmu    <- signif(x$a,3)
    logsigma <- signif(x$b,3)
    .Object@model_code <- sub("r.?~.?lognormal\\(.+?\\);",paste("r ~ lognormal(",logmu,",",logsigma,");",sep=""),.Object@model_code)
    
    if(plot) {
      windows()
      curve(dlnorm(x,logmu,logsigma),col=2,from=0,to=2,xlab='r',ylab='',yaxt='n',main='')
    }
  }
  #if(x$par=='sigRsq') {
  #  alpha <- signif(x$a,3)
  #  beta  <- signif(x$b,3)
  #  .Object@model_code <- sub("sigRsq.?~.?inv_gamma\\(.+?\\);",paste("sigRsq ~ inv_gamma(",alpha,",",beta,");",sep=""),.Object@model_code)
  #  
  #  if(plot) {
  #    dinvgamma <- function(x,alpha,beta) (beta^alpha / gamma(alpha)) * (1/(x^(alpha+1))) * exp(-beta / x)
  #    windows()
  #    curve(dinvgamma(x,alpha,beta),col=2,from=0,to=2,xlab='Observation error (sigRsq)',ylab='',yaxt='n',main='')
  #    abline(v=beta/(alpha + 1),col=2,lty=2)
  #  }
  #}
  #if(x$par=='sigQsq') {
  #  alpha <- signif(x$a,3)
  #  beta  <- signif(x$b,3)
  #  .Object@model_code <- sub("sigQsq.?~.?inv_gamma\\(.+?\\);",paste("sigQsq ~ inv_gamma(",alpha,",",beta,");",sep=""),.Object@model_code)
  #  
  #  if(plot) {
  #    dinvgamma <- function(x,alpha,beta) (beta^alpha / gamma(alpha)) * (1/(x^(alpha+1))) * exp(-beta / x)
  #    windows()
  #    curve(dinvgamma(x,alpha,beta),col=2,from=0,to=2,xlab='Process error (sigQsq)',ylab='',yaxt='n',main='')
  #    abline(v=beta/(alpha + 1),col=2,lty=2)
  #  }
  #}
  if(x$par=='logK') {
    low <- signif(x$a,3)
    upp <- signif(x$b,3)
    .Object@model_code <- sub("logK.?~.?uniform\\(.+?\\);",paste("logK ~ uniform(",low,",",upp,");",sep=""),.Object@model_code)
    
    if(plot) {
      windows()
      curve(dunif(x,low,upp),from=max(low-mean(c(low,upp))/4,0),to=upp+mean(c(low,upp))/4,col=2,xlab='logK',ylab='',yaxt='n',main='')
    }
  }
  
  if(compile)
    .Object <- compile_bdm(.Object)
  
  .Object
})
#}}
#}}}
