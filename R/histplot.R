
#{{{ histogram diagnostic plot
setGeneric("histplot", function(.Object,pars,inc_warmup=FALSE, ...) standardGeneric("histplot"))
setMethod("histplot",signature=c("bdm"), function(.Object,pars,inc_warmup=FALSE) {
  
  require(ggplot2)
  
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
  
  gg <- ggplot(dfr) + 
          geom_histogram(aes(x=value,fill=chain),position='stack') + 
          facet_wrap(~variable,scales='free_x') +
          xlab('Parameter value') +
          ylab('Sample counts') + theme_bw()
  
  #######################################################
  # code for extraction of iterations from object@trace #
  # (list of parameter arrays with all chains combined) #
  #######################################################
  #loc <- array(dim=c(length(pars),2),dimnames=list(pars,c('i','j')))
  #for(par in pars) {
  #  m <- regexpr('\\[.+\\]',par)
  #  if(m>0) {
  #    loc[par,'i'] <- match(substr(par,1,m-1),names(.Object@trace))
  #    m <- m + 1
  #    attributes(m)$match.length <- attributes(m)$match.length - 2
  #    loc[par,'j'] <- as.numeric(regmatches(par,m))
  #  } else {
  #    loc[par,'i'] <- match(par,names(.Object@trace))
  #  }
  #}
  #
  #dfr <- data.frame(variable=NULL,value=NULL)
  #for(par in pars) {
  #  
  #  i <- loc[par,'i']
  #  j <- loc[par,'j']
  #  
  #  if(length(dim(.Object@trace[[i]]))>1) {
  #    list.tmp <- lapply(apply(.Object@trace[[i]],2,function(x) list(x)),unlist)
  #    if(is.na(j)) {
  #      for(j in 1:length(list.tmp)) {
  #        dfr <- rbind(dfr,data.frame(variable=paste(names(.Object@trace)[i],'[',j,']',sep=''),value=list.tmp[[j]]))
  #      }
  #    } else {
  #      dfr <- rbind(dfr,data.frame(variable=paste(names(.Object@trace)[i],'[',j,']',sep=''),value=list.tmp[[j]]))
  #    }
  #  } else {
  #    dfr <- rbind(dfr,data.frame(variable=names(.Object@trace)[i],value=.Object@trace[[i]]))
  #  }
  #}
  #
  #gg <- ggplot(dfr) + 
  #        geom_histogram(aes(x=value),position='identity') + 
  #        facet_wrap(~variable,scales='free_x') +
  #        xlab('Parameter value') +
  #        ylab('Sample counts') + theme_bw()
  
  suppressMessages(suppressWarnings(print(gg)))
})
#}}}
