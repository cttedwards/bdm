
#{{{ update r-prior distribution in bdm model object and recompile
setGeneric("update_bdm", function(.Object,x, ...) standardGeneric("update_bdm"))
setMethod("update_bdm",signature=c("bdm","rprior"),function(.Object,x,compile=FALSE,plot=FALSE) {
  
  if(length(x@lognormal.par)==0)
    x <- .fitr(x)
  
  logmu    <- signif(x@lognormal.par$mu,3)
  logsigma <- signif(x@lognormal.par$sigma,3)
  .Object@model_code <- sub("r0.?~.?lognormal\\(.+?\\);",paste("r0 ~ lognormal(",logmu,",",logsigma,");",sep=""),.Object@model_code)
  
  if(plot) {
    windows()
    hist(x,freq=FALSE,xlab='r',ylab='',yaxt='n',main='')
    curve(dlnorm(x,logmu,logsigma),col=2,add=T)
  }
  
  if(compile)
    .Object <- compile_bdm(.Object)
  
  .Object
})
#{ fit log-normal distribution to monte-carlo samples from r-prior
.fitr <- function(.Object) {
  
  x <- .Object@.Data
  if(!length(x)>2) stop('need more than two r values')
  
  # transform to normal
  y <- log(x)
  n <- length(y)
  
  # estimate parameters of
  # normal distribution
  ybar <- mean(y)
  s2   <- sum((y-ybar)^2)/n
  
  # estimate parameters of
  # log-normal distribution
  theta <- exp(ybar + s2/2)
  nu <- sqrt(exp(2*ybar + s2)*(exp(s2) - 1))
  
  # assign and return
  .Object@lognormal.par <- list(mu=ybar,sigma=sqrt(s2),theta=theta,nu=nu)
  return(.Object)
  
}
#}
setMethod("update_bdm",signature=c("bdm","list"),function(.Object,x,compile=FALSE,plot=FALSE) {
    
  if(x$par=='r') {
    logmu    <- signif(x$a,3)
    logsigma <- signif(x$b,3)
    .Object@model_code <- sub("r0.?~.?lognormal\\(.+?\\);",paste("r0 ~ lognormal(",logmu,",",logsigma,");",sep=""),.Object@model_code)
    
    if(plot) {
      windows()
      curve(dlnorm(x,logmu,logsigma),col=2,xlab='r',ylab='',yaxt='n',main='')
    }
  }
  if(x$par=='sigRsq') {
    alpha <- signif(x$a,3)
    beta  <- signif(x$b,3)
    .Object@model_code <- sub("sigRsq.?~.?inv_gamma\\(.+?\\);",paste("sigRsq ~ inv_gamma(",alpha,",",beta,");",sep=""),.Object@model_code)
  
    if(plot) {
      dinvgamma <- function(x,alpha,beta) (beta^alpha / gamma(alpha)) * (1/(x^(alpha+1))) * exp(-beta / x)
      windows()
      curve(dinvgamma(x,alpha,beta),col=2,xlab='sigRsq',ylab='',yaxt='n',main='')
      abline(v=beta/(alpha + 1),col=2,lty=2)
    }
  }
  if(x$par=='sigQsq') {
    alpha <- signif(x$a,3)
    beta  <- signif(x$b,3)
    .Object@model_code <- sub("sigQsq.?~.?inv_gamma\\(.+?\\);",paste("sigQsq ~ inv_gamma(",alpha,",",beta,");",sep=""),.Object@model_code)
    
    if(plot) {
      dinvgamma <- function(x,alpha,beta) (beta^alpha / gamma(alpha)) * (1/(x^(alpha+1))) * exp(-beta / x)
      windows()
      curve(dinvgamma(x,alpha,beta),col=2,xlab='sigQsq',ylab='',yaxt='n',main='')
      abline(v=beta/(alpha + 1),col=2,lty=2)
    }
  }
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
#}}}
