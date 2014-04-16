
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
  
  # estimate parameters of
  # normal distribution log(x)
  mu    <- mean(y)
  sigma <- sd(y)
  sigma2 <- sigma^2
  
  # estimate parameters of
  # log-normal distribution
  theta <- exp(mu + sigma2/2)
  nu <- exp(2*mu + sigma2)*(exp(sigma2) - 1)
  cv <- sqrt(exp(sigma2) - 1)
  
  # assign and return
  .Object@lognormal.par <- list('E[log(x)]'=mu,'SD[log(x)]'=sigma,'E[x]'=theta,'VAR[x]'=nu,'CV[x]'=cv)
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
      curve(dlnorm(x,logmu,logsigma),col=2,from=0,to=2,xlab='r',ylab='',yaxt='n',main='')
    }
  }
  if(x$par=='sigRsq') {
    alpha <- signif(x$a,3)
    beta  <- signif(x$b,3)
    .Object@model_code <- sub("sigRsq.?~.?inv_gamma\\(.+?\\);",paste("sigRsq ~ inv_gamma(",alpha,",",beta,");",sep=""),.Object@model_code)
  
    if(plot) {
      dinvgamma <- function(x,alpha,beta) (beta^alpha / gamma(alpha)) * (1/(x^(alpha+1))) * exp(-beta / x)
      windows()
      curve(dinvgamma(x,alpha,beta),col=2,from=0,to=2,xlab='Observation error (sigRsq)',ylab='',yaxt='n',main='')
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
      curve(dinvgamma(x,alpha,beta),col=2,from=0,to=2,xlab='Process error (sigQsq)',ylab='',yaxt='n',main='')
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
