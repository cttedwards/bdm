#'
#' Plot the \code{bdm} model fit
#' 
#' Plots the estimated abundance index dynamics over time against the observed values
#' 
#' @param x a fitted \code{bdm} class object.
#' @param labels character vector of labels for each index.
#' @param ... additional arguments to the generic function
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @include ggtheme.R
#' 
#' @import ggplot2
#' @import reshape2
#' 
#' @export
idxplot <- function(x, ...) UseMethod("idxplot")
#'
#' @rdname idxplot
#' @export
idxplot.bdm <- function(x, labels, ...) {
    
    nidx     <- x@data[['I']]
    time     <- x@data[['time']]
    nsamples <- x@nsamples
    
    if (missing(labels))
        labels <- paste('index:', 1:nidx, sep = '')
    
    #######################################################
    # code for extraction of iterations from object@trace #
    # (list of parameter arrays with all chains combined) #
    #######################################################
    dfr <- data.frame(iter = integer(),time = integer(),value = numeric(),label = character())
    idx.arr <- x@trace[['predicted_index']]
    dimnames(idx.arr) <- list(iter = 1:nsamples,time = time,index = 1:nidx)
    for (i in 1:nidx) {
        
        idx.dfr <- melt(idx.arr[,,i])
        idx.dfr <- data.frame(idx.dfr,label = labels[i])
        
        dfr <- rbind(dfr,idx.dfr)
    }
    
    gg <- ggplot() + 
        stat_summary(data = dfr,aes(time,value),fun.ymin = function(x) quantile(x,0.025),fun.ymax = function(x) quantile(x,0.975),geom = 'ribbon',alpha = 0.3) +
        stat_summary(data = dfr,aes(time,value),fun.y = function(x) mean(x),geom = 'line',lwd = 1.5) +
        labs(x = 'Time',y = 'Predicted Index') +
        theme_bw(base_size = 12) %+replace% theme(legend.position = "none")
    
    dfr.empirical <- x@data$index
    dimnames(dfr.empirical) <- list(time  =  x@data$time, label  =  labels)
    dfr.empirical[dfr.empirical < 0] <- NA
    dfr.empirical <- melt(dfr.empirical)
    
    gg <- gg + geom_point(data = dfr.empirical,aes(time,value,col = label), size = 4) + 
        geom_line(data = dfr.empirical,aes(time,value,col = label), size = 1.5)
    
    if (nidx > 1)
        gg <- gg + facet_grid(label~.)
    
    return(gg)
}
#}}}
