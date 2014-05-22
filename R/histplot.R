
#{{{ histogram diagnostic plot
setGeneric("histplot", function(.Object,pars, ...) standardGeneric("histplot"))
setMethod("histplot",signature=c("bdm"), function(.Object,pars) {
  
  require(ggplot2)
  
  if(missing(pars) & .Object@default_model) 
    pars <- c('r','logK','lp__')
  
  loc <- match(pars,names(.Object@trace))
  
  dfr <- data.frame(variable=NULL,value=NULL)
  for(i in loc) {
    if(length(dim(.Object@trace[[i]]))>1) {
      list.tmp <- lapply(apply(.Object@trace[[i]],2,function(x) list(x)),unlist)
      for(j in 1:length(list.tmp))
        dfr <- rbind(dfr,data.frame(variable=paste(names(.Object@trace)[i],'[',j,']',sep=''),value=list.tmp[[j]]))
    } else {
      dfr <- rbind(dfr,data.frame(variable=names(.Object@trace)[i],value=.Object@trace[[i]]))
    }
  }
  
  gg <- ggplot(dfr) + 
          geom_histogram(aes(x=value),position='identity') + 
          facet_wrap(~variable,scales='free_x') +
          xlab('Parameter value') +
          ylab('Sample counts') + theme_bw()
  
  suppressMessages(print(gg))
})
#}}}
