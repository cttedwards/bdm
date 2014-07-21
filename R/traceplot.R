
#{{{ plot traces
setMethod("traceplot",signature=list(object="bdm"),function(object, pars, inc_warmup = TRUE, ask = FALSE, ...) {
  
  if(missing(pars)) {
    if(object@default_model) 
      pars <- c('r','logK','lp__')
    else stop('must define pars for non-default model')
  }
  
  dfr <- data.frame(variable=NULL,chain=NULL,value=NULL)
  
  # extract paramater values from object@trace_array
  for(par in pars) {
    m <- regexpr('\\[.+\\]',par)
    if(m>0) {
      i <- match(par,dimnames(object@trace_array)$parameters)
      if(!is.na(i)) {
        dfr.tmp <- melt(object@trace_array[,,i])
	      if(ncol(dfr.tmp)>2) {
			    dfr <- rbind(dfr,data.frame(variable=dimnames(object@trace_array)$parameters[i],iteration=dfr.tmp$iterations,chain=dfr.tmp$chains,value=dfr.tmp$value))
		    } else {
			    dfr <- rbind(dfr,data.frame(variable=dimnames(object@trace_array)$parameters[i],iteration=1:dim(dfr.tmp)[1],chain='chain:1',value=dfr.tmp$value))
		    }
	    }
    } else {
      mm <- 0
      for(parname in dimnames(object@trace_array)$parameters) {
        m  <- regexpr(par,parname)
        if(m>0) { 
          i <- match(parname,dimnames(object@trace_array)$parameters)
          dfr.tmp <- melt(object@trace_array[,,i])
		      if(ncol(dfr.tmp)>2) {
			      dfr <- rbind(dfr,data.frame(variable=parname,iteration=dfr.tmp$iterations,chain=dfr.tmp$chains,value=dfr.tmp$value)) 
          } else {
			      dfr <- rbind(dfr,data.frame(variable=parname,iteration=1:dim(dfr.tmp)[1],chain='chain:1',value=dfr.tmp$value)) 
		      }
		      mm <- mm + 1
        } else {
          if(mm>0) break
        }
      }
    }
  }
  if(!nrow(dfr)>0) stop('parameter not found\n')
  
  # extract chain numbering
  dfr$chain <- unlist(lapply(strsplit(as.character(dfr$chain),split=':'),function(x) x[2]))
  
  if(!inc_warmup) 
    dfr <- subset(dfr,iteration>object@warmup)
  
  gg <- ggplot(dfr)
    
  if(inc_warmup) 
    gg <- gg + geom_vline(xintercept=object@warmup,linetype='longdash',col='grey50')
  
  gg <- gg + geom_line(aes(x=iteration,y=value,col=chain)) + 
    facet_wrap(~variable,scales='free_y') +
    xlab('Iteration') +
    ylab('Parameter value') + theme_bw()
  
  suppressMessages(print(gg))
  
})
#}}}
