#'
#' Plot posterior histograms
#' 
#' Plots histograms of posterior samples from an MCMC chain contained within a \code{bdm} class object.
#' 
#' @export
#' 
histplot <- function(object, ...) UseMethod("histplot")
#' 
#' @rdname histplot
#' @param object a \code{bdm} class object.
#' @param pars character vector of model parameters to be plotted. Defaults to \code{pars = c('r','logK','lp__')}.
#' @param inc_warmup logical value indicating whether MCMC warmup should be included in the plot.
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @include ggtheme.R
#' 
#' @export
histplot.bdm <- function(object, pars, inc_warmup  =  FALSE) {
    
  if (missing(pars)) {
    if (object@default_model) 
      pars <- c('r','logK','lp__')
    else stop('must define pars for non-default model')
  }
  
  #############################################################
  # code for extraction of iterations from object@trace_array #
  # (array with dimensions: iteration; parameter; chain)      #
  #############################################################
  dfr <- data.frame(variable = NULL,chain = NULL,value = NULL)
  
  for (par in pars) {
    m <- regexpr('\\[.+\\]',par)
    if (m > 0) {
      i <- match(par,dimnames(object@trace_array)$parameters)
      if (!is.na(i)) {
        dfr.tmp <- melt(object@trace_array[,,i])
        if (ncol(dfr.tmp) > 2) {
          dfr <- rbind(dfr,data.frame(variable = dimnames(object@trace_array)$parameters[i],iteration = dfr.tmp$iterations,chain = dfr.tmp$chains,value = dfr.tmp$value))
        } else {
          dfr <- rbind(dfr,data.frame(variable = dimnames(object@trace_array)$parameters[i],iteration = 1:dim(dfr.tmp)[1],chain = 'chain:1',value = dfr.tmp$value))
        }
      }
    } else {
      mm <- 0
      for (parname in dimnames(object@trace_array)$parameters) {
        m  <- regexpr(par,parname)
        if (m > 0) { 
          i <- match(parname,dimnames(object@trace_array)$parameters)
          dfr.tmp <- melt(object@trace_array[,,i])
          if (ncol(dfr.tmp) > 2) {
            dfr <- rbind(dfr,data.frame(variable = parname,iteration = dfr.tmp$iterations,chain = dfr.tmp$chains,value = dfr.tmp$value)) 
          } else {
            dfr <- rbind(dfr,data.frame(variable = parname,iteration = 1:dim(dfr.tmp)[1],chain = 'chain:1',value = dfr.tmp$value)) 
          }
          mm <- mm + 1
        } else {
          if (mm > 0) break
        }
      }
    }
  }
  if (!nrow(dfr) > 0) stop('parameter not found\n')
  
  dfr$chain <- unlist(lapply(strsplit(as.character(dfr$chain),split = ':'),function(x) x[2]))
  
  if (!inc_warmup) dfr <- subset(dfr,iteration > object@warmup)
  
  gg <- ggplot(dfr) + 
          geom_histogram(aes(x = value,fill = chain),position = 'stack') + 
          facet_wrap(~variable,scales = 'free_x') +
          xlab('Parameter value') +
          ylab('Sample counts') + ggtheme()
  
  #######################################################
  # code for extraction of iterations from object@trace #
  # (list of parameter arrays with all chains combined) #
  #######################################################
  #loc <- array(dim = c(length(pars),2),dimnames = list(pars,c('i','j')))
  #for (par in pars) {
  #  m <- regexpr('\\[.+\\]',par)
  #  if (m>0) {
  #    loc[par,'i'] <- match(substr(par,1,m-1),names(object@trace))
  #    m <- m + 1
  #    attributes(m)$match.length <- attributes(m)$match.length - 2
  #    loc[par,'j'] <- as.numeric(regmatches(par,m))
  #  } else {
  #    loc[par,'i'] <- match(par,names(object@trace))
  #  }
  #}
  #
  #dfr <- data.frame(variable = NULL,value = NULL)
  #for (par in pars) {
  #  
  #  i <- loc[par,'i']
  #  j <- loc[par,'j']
  #  
  #  if (length(dim(object@trace[[i]]))>1) {
  #    list.tmp <- lapply(apply(object@trace[[i]],2,function(x) list(x)),unlist)
  #    if (is.na(j)) {
  #      for (j in 1:length(list.tmp)) {
  #        dfr <- rbind(dfr,data.frame(variable = paste(names(object@trace)[i],'[',j,']',sep = ''),value = list.tmp[[j]]))
  #      }
  #    } else {
  #      dfr <- rbind(dfr,data.frame(variable = paste(names(object@trace)[i],'[',j,']',sep = ''),value = list.tmp[[j]]))
  #    }
  #  } else {
  #    dfr <- rbind(dfr,data.frame(variable = names(object@trace)[i],value = object@trace[[i]]))
  #  }
  #}
  #
  #gg <- ggplot(dfr) + 
  #        geom_histogram(aes(x = value),position = 'identity') + 
  #        facet_wrap(~variable,scales = 'free_x') +
  #        xlab('Parameter value') +
  #        ylab('Sample counts') + ggtheme()
  
  return(gg)
}
#}}}
