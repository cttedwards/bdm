
#{{{ histogram diagnostic plot
setGeneric("dynplot", function(.Object,pars, ...) standardGeneric("dynplot"))
setMethod("dynplot",signature=c("bdm"), function(.Object,pars, ...) {
    
  if(missing(pars)) {
    if(.Object@default_model) 
      pars <- 'depletion'
    else stop('must define pars for non-default model\n')
  } else {
      if(!(pars %in% c('depletion', 'biomass', 'harvest_rate', 'surplus_production')))
        stop('pars must be one or more of depletion, biomass, harvest_rate or surplus_production\n')
  }
  
  time <- .Object@data[['time']]
  nsamples <- .Object@nsamples
  
  #######################################################
  # code for extraction of iterations from object@trace #
  # (list of parameter arrays with all chains combined) #
  #######################################################
  dfr <- data.frame(iter=integer(),time=integer(),value=numeric(),label=character())
  for(par in pars) {
      par.arr <- .Object@trace[[par]]
      dimnames(par.arr) <- list(iter=1:nsamples,time=time)
      
      par.dfr <- melt(par.arr)
      par.dfr <- data.frame(par.dfr,label=par)
      
      dfr <- rbind(dfr,par.dfr)
  }
  
  gg <- ggplot(dfr,aes(time,value)) + 
			stat_summary(fun.ymin=function(x) quantile(x,0.025),fun.ymax=function(x) quantile(x,0.975),geom='ribbon',alpha=0.3) +
			stat_summary(fun.y=function(x) mean(x),geom='line',lwd=1.5) +
			labs(x='Time',y='Predicted Value') +
			.theme_bdm()
  
  if(length(pars)>1)
      gg <- gg + facet_grid(label~., scales = 'free_y')
  
  return(gg)
})
#}}}
