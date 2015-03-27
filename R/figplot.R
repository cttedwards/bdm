
#{{{ histogram diagnostic plot
setGeneric("figplot", function(.Object,par, ...) standardGeneric("figplot"))
setMethod("figplot",signature=c("bdm"), function(.Object,par, ...) {
    
  if(missing(par)) {
    if(.Object@default_model) 
      par <- 'depletion'
    else stop('must define par for non-default model\n')
  } else {
    if(length(par)>1)
	  stop('only plot one par at a time\n')
  }
  
  time <- .Object@data[['time']]
  nsamples <- .Object@nsamples
  
  #######################################################
  # code for extraction of iterations from object@trace #
  # (list of parameter arrays with all chains combined) #
  #######################################################
  par.arr <- .Object@trace[[par]]
  dimnames(par.arr) <- list(iter=1:nsamples,time=time)
  
  dfr <- melt(par.arr)
  
  gg <- ggplot(dfr,aes(time,value)) + 
			stat_summary(fun.ymin=function(x) quantile(x,0.025),fun.ymax=function(x) quantile(x,0.975),geom='ribbon',alpha=0.3) +
			stat_summary(fun.y=function(x) mean(x),geom='line',lwd=1.5) +
			labs(x='Time',y='Predicted Value') +
			theme_bw()
  
  suppressMessages(suppressWarnings(print(gg)))
})
#}}}
