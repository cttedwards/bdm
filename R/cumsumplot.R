
#{{{ histogram diagnostic plot
setGeneric("cumsumplot", function(.Object,pars,inc_warmup=FALSE, ...) standardGeneric("cumsumplot"))
setMethod("cumsumplot",signature=c("bdm"), function(.Object,pars,inc_warmup=FALSE) {
    
  if(missing(pars)) {
    if(.Object@default_model) 
      pars <- c('r','logK','lp__')
    else stop('must define pars for non-default model')
  }
  
  #############################################################
  # code for extraction of iterations from object@trace_array #
  # (array with dimensions: iteration; parameter; chain)      #
  #############################################################
  dfr <- data.frame(variable=NULL,chain=NULL,value=NULL)
  
  for(par in pars) {
    m <- regexpr('\\[.+\\]',par)
    if(m>0) {
      i <- match(par,dimnames(.Object@trace_array)$parameters)
      if(!is.na(i)) {
        dfr.tmp <- melt(.Object@trace_array[,,i])
        if(ncol(dfr.tmp)>2) {
          dfr <- rbind(dfr,data.frame(variable=dimnames(.Object@trace_array)$parameters[i],iteration=dfr.tmp$iterations,chain=dfr.tmp$chains,value=dfr.tmp$value))
        } else {
          dfr <- rbind(dfr,data.frame(variable=dimnames(.Object@trace_array)$parameters[i],iteration=1:dim(dfr.tmp)[1],chain='chain:1',value=dfr.tmp$value))
        }
      }
    } else {
      mm <- 0
      for(parname in dimnames(.Object@trace_array)$parameters) {
        m  <- regexpr(par,parname)
        if(m>0) { 
          i <- match(parname,dimnames(.Object@trace_array)$parameters)
          dfr.tmp <- melt(.Object@trace_array[,,i])
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
  
  dfr$chain <- unlist(lapply(strsplit(as.character(dfr$chain),split=':'),function(x) x[2]))
  
  if(!inc_warmup) dfr <- subset(dfr,iteration>.Object@warmup)
  
  dfr <- plyr::ddply(dfr, .(variable,chain), summarize, value = value[order(value)], cumsum = (1:length(chain))/length(chain))
  
  gg <- ggplot(dfr) + 
          geom_line(aes(x=value,y=cumsum,col=chain),size=1.5) + 
          facet_wrap(~variable,scales='free_x') +
          xlab('Parameter value') +
          ylab('Cumulative Sum') + .theme_bdm()
  
  return(gg)
})
#}}}
