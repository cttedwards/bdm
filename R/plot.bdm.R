
#{{{ plot traces and histograms sampled from posterior
#setMethod("plot",signature=list(x="bdm",y="missing"),function(x,y,par, ...) {
  
plot.bdm <- function(x,par, ...) {
  
  nix <- x@data$I
  if(missing(par)) par <- 'lp__'
  
  #op <- options()
  #options("device.ask.default" = TRUE)
  
  #dev.new()
  #par(mfrow=c(2,1))
  traceplot(x@fit,par,nrow=1,ncol=1)
  
  #dev.new()
  #par(mfrow=c(2,2))
  #for(i in 1:length(par)) {
  #  loc <- match(par[i],names(x@trace))
  #  if(par[i]=='q' & nix > 1) { apply(x@trace[[loc]],2,hist,ylab='',yaxt='n',xlab='',main='q')
  #  } else hist(x@trace[[loc]],ylab='',yaxt='n',xlab='',main=par[i])
  #}
  #dev.new()
  loc <- match(par,names(x@trace))
  if(par=='q' & nix > 1) { 
    par(mfrow=c(nix,1))
    hist.tmp <- apply(x@trace[[loc]],2,hist,plot=FALSE)
    for(i in 1:nix) plot(hist.tmp[[i]],ylab='',yaxt='n',xlab='',main=paste('q[',i,']',sep=''))
  } else {
    hist(x@trace[[loc]],ylab='',yaxt='n',xlab='',main=par)
  }
  
  # reset graphics options
  #options(op)
  
}
  #)
#}}}
