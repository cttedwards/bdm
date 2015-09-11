#'
#' Plot estimated dynamics from \code{bdm} model fit
#' 
#' Plots the dynamics over time of the estimated biomass, depletion, harvest rate or surplus production.
#' 
#' Depletion is measured as the biomass over the carrying capacity, harvest rate is the catch over the estimated biomass, and surplus production is the production function multiplied by the process error residual. 
#' 
#' @param x \code{bdm} class object.
#' @param pars character vector of model parameters to be plotted. Must be one or more of \code{'depletion'}, \code{'biomass'}, \code{'harvest_rate'} or \code{'surplus_production'}.
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @include ggtheme.R
#' 
#' @import ggplot2
#' @import reshape2
#' 
#' @export
dynplot <- function(x, ...) UseMethod("dynplot")
#'
#' @rdname dynplot
#' @export
dynplot.bdm <- function(x, pars = 'depletion') {
    
    time <- x@data[['time']]
    nsamples <- x@nsamples
    
    #######################################################
    # code for extraction of iterations from object@trace #
    # (list of parameter arrays with all chains combined) #
    #######################################################
    dfr <- data.frame(iter = integer(),time = integer(),value = numeric(),label = character())
    for (par in pars) {
        par.arr <- x@trace[[par]]
        dimnames(par.arr) <- list(iter = 1:nsamples,time = time)
        
        par.dfr <- melt(par.arr)
        par.dfr <- data.frame(par.dfr,label = par)
        
        dfr <- rbind(dfr,par.dfr)
    }
    
    gg <- ggplot(dfr,aes(time,value)) + 
        stat_summary(fun.ymin = function(x) quantile(x,0.025),fun.ymax = function(x) quantile(x,0.975),geom = 'ribbon',alpha = 0.3) +
        stat_summary(fun.y = function(x) mean(x),geom = 'line',lwd = 1.5) +
        labs(x = 'Time',y = 'Predicted Value') +
        ggtheme()
    
    if (length(pars)>1)
        gg <- gg + facet_grid(label~., scales  =  'free_y')
    
    return(gg)
}
#}}}
