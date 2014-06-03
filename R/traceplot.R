
#{{{ plot traces
setMethod("traceplot",signature=list(object="bdm"),function(object, pars, inc_warmup = TRUE, ask = FALSE, ...) {
  
  require(ggplot2)
  require(reshape2)
  
  if(missing(pars) & object@default_model) 
    pars <- c('r','logK','lp__')
  
  dfr <- data.frame(variable=NULL,chain=NULL,value=NULL)
  
  for(par in pars) {
    m <- regexpr('\\[.+\\]',par)
    if(m>0) {
      i <- match(par,dimnames(object@fit)$parameters)
      if(!is.na(i)) {
        dfr.tmp <- melt(object@fit[,,i])
        dfr <- rbind(dfr,data.frame(variable=dimnames(object@fit)$parameters[i],iteration=dfr.tmp$iterations,chain=dfr.tmp$chains,value=dfr.tmp$value))
      }
    } else {
      mm <- 0L
      for(parname in dimnames(object@fit)$parameters) {
        m  <- regexpr(par,parname)
        if(m>0) { 
          i <- match(parname,dimnames(object@fit)$parameters)
          dfr.tmp <- melt(object@fit[,,i])
          dfr <- rbind(dfr,data.frame(variable=parname,iteration=dfr.tmp$iterations,chain=dfr.tmp$chains,value=dfr.tmp$value)) 
          mm <- mm + 1L
        } else {
          if(mm>0) break
        }
      }
    }
  }
  if(!nrow(dfr)>0) stop('parameter not found\n')
  
  dfr$chain <- unlist(lapply(strsplit(as.character(dfr$chain),split=':'),function(x) x[2]))
  
  if(!inc_warmup) dfr <- subset(dfr,iteration>object@warmup)
  
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
