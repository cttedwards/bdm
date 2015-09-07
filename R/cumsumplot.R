#'
#' Plot cumulative sum of MCMC chain
#' 
#' Plots the cumulative sum of ordered posterior samples from an MCMC chain contained within a \code{bdm} class object.
#' 
#' @export
cumsumplot <- function(x, ...) UseMethod("cumsumplot")
#' 
#' @param x \code{bdm} class object.
#' @param pars character vector of model parameters to be plotted. Defaults to \code{pars = c('r','logK','lp__')}.
#' @param inc_warmup logical value indicating whether MCMC warmup should be included in the plot.
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @include ggtheme.R
#' 
#' @rdname cumsumplot
#' @export
cumsumplot.bdm <- function(x, pars, inc_warmup = FALSE) {
    
    if (missing(pars)) {
        if (x@default_model) 
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
        if (m>0) {
            i <- match(par,dimnames(x@trace_array)$parameters)
            if (!is.na(i)) {
                dfr.tmp <- melt(x@trace_array[,,i])
                if (ncol(dfr.tmp)>2) {
                    dfr <- rbind(dfr,data.frame(variable = dimnames(x@trace_array)$parameters[i],iteration = dfr.tmp$iterations,chain = dfr.tmp$chains,value = dfr.tmp$value))
                } else {
                    dfr <- rbind(dfr,data.frame(variable = dimnames(x@trace_array)$parameters[i],iteration = 1:dim(dfr.tmp)[1],chain = 'chain:1',value = dfr.tmp$value))
                }
            }
        } else {
            mm <- 0
            for (parname in dimnames(x@trace_array)$parameters) {
                m  <- regexpr(par,parname)
                if (m>0) { 
                    i <- match(parname,dimnames(x@trace_array)$parameters)
                    dfr.tmp <- melt(x@trace_array[,,i])
                    if (ncol(dfr.tmp)>2) {
                        dfr <- rbind(dfr,data.frame(variable = parname,iteration = dfr.tmp$iterations,chain = dfr.tmp$chains,value = dfr.tmp$value)) 
                    } else {
                        dfr <- rbind(dfr,data.frame(variable = parname,iteration = 1:dim(dfr.tmp)[1],chain = 'chain:1',value = dfr.tmp$value)) 
                    }
                    mm <- mm + 1
                } else {
                    if (mm>0) break
                }
            }
        }
    }
    if (!nrow(dfr)>0) stop('parameter not found\n')
    
    dfr$chain <- unlist(lapply(strsplit(as.character(dfr$chain),split = ':'),function(x) x[2]))
    
    if (!inc_warmup) dfr <- subset(dfr,iteration > x@warmup)
    
    dfr <- plyr::ddply(dfr, .(variable,chain), summarize, value  =  value[order(value)], cumsum  =  (1:length(chain))/length(chain))
    
    gg <- ggplot(dfr) + 
        geom_line(aes(x = value,y = cumsum,col = chain),size = 1.5) + 
        facet_wrap(~variable,scales = 'free_x') +
        xlab('Parameter value') +
        ylab('Cumulative Sum') + ggtheme()
    
    return(gg)
}
#}}}
