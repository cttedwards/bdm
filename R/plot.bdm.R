
#{{{ plot traces and histograms sampled from posterior
#setMethod("plot",signature=list(x="bdm",y="missing"),function(x,y,par, ...) {
  
plot.bdm <- function(x,par, ...) {
  
  nix <- x@data$N
  if(missing(par)) par <- c('logK','r0','sigRsq','sigQsq','q','lp__')
  
  #op <- options()
  #options("device.ask.default" = TRUE)
  
  dev.new()
  traceplot(x@fit,par,nrow=2,ncol=2)
  
  dev.new()
  par(mfrow=c(2,2))
  for(i in 1:length(par)) {
    loc <- match(par[i],names(x@trace))
    if(par[i]=='q' & nix > 1) { apply(x@trace[[loc]],2,hist,ylab='',yaxt='n',xlab='',main='q')
    } else hist(x@trace[[loc]],ylab='',yaxt='n',xlab='',main=par[i])
  }
  
  # reset graphics options
  #options(op)
  
}
  #)
#}}}
