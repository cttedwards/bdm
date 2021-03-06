#'
#' Plot estimated dynamics from \code{bdm} model fit
#' 
#' Plots the dynamics over time of the estimated biomass, depletion, harvest rate or surplus production.
#' 
#' Depletion is measured as the biomass over the carrying capacity, harvest rate is the catch over the estimated biomass, and surplus production is the production function multiplied by the process error residual. 
#' 
#' @param x \code{bdm} class object.
#' @param pars character vector of model parameters to be plotted. Must be one or more of \code{'depletion'}, \code{'biomass'}, \code{'harvest_rate'} or \code{'surplus_production'}.
#' @param ... additional arguments to the generic function
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @import ggplot2
#' @import reshape2
#' 
#' @export
dynplot <- function(x, ...) UseMethod("dynplot")
#'
#' @rdname dynplot
#' @export
#dynplot.bdm <- function(x, pars = 'depletion', ...) {
#    
#    time <- x@data[['time']]
#    nsamples <- x@nsamples
#    
#    #######################################################
#    # code for extraction of iterations from object@trace #
#    # (list of parameter arrays with all chains combined) #
#    #######################################################
#    dfr <- data.frame(iter = integer(),time = integer(),value = numeric(),label = character())
#    for (par in pars) {
#        par.arr <- x@trace[[par]]
#        dimnames(par.arr) <- list(iter = 1:nsamples,time = time)
#        
#        par.dfr <- melt(par.arr)
#        par.dfr <- data.frame(par.dfr,label = par)
#        
#        dfr <- rbind(dfr,par.dfr)
#    }
#    
#    gg <- ggplot(dfr,aes(time,value)) + 
#        stat_summary(fun.ymin = function(x) quantile(x,0.025),fun.ymax = function(x) quantile(x,0.975),geom = 'ribbon',alpha = 0.3) +
#        stat_summary(fun.y = function(x) mean(x),geom = 'line',lwd = 1.5) +
#        labs(x = 'Time',y = 'Predicted Value') #+
#        #ggtheme()
#    
#    if (length(pars)>1)
#        gg <- gg + facet_grid(label~., scales  =  'free_y')
#    
#    return(gg)
#}

dynplot.bdm <- function(x, ..., pars = 'depletion', labels = character()) {
    
    y <- c(x, list(...))
    
    is.labelled <- ifelse(length(labels) > 0, TRUE, FALSE)
    
    if (is.labelled & length(y) != length(labels)) {
        stop("'labels' vector length does not match number of models")  
    }
    
    dfr <- data.frame(iter = integer(),time = integer(),value = numeric(),label = character(),run = character())
    for (i in 1:length(y)) {
        x <- y[[i]]
        for (par in pars) {
            par.arr <- x@trace[[par]]
            dimnames(par.arr) <- list(iter = 1:x@nsamples,time = x@data$time)
            
            if (is.labelled) {
                x@run <- labels[i]
            }
            if (length(x@run) == 0) {
                warning("'run' unspecified") 
            }
            
            par.dfr <- melt(par.arr)
            par.dfr <- data.frame(par.dfr,label = par, run = ifelse(length(x@run) == 0, "", x@run))
            
            dfr <- rbind(dfr,par.dfr)
        }
    }
    
    if (is.labelled) {
        dfr$run <- factor(dfr$run)
        dfr$run <- factor(dfr$run, levels = levels(dfr$run)[match(levels(dfr$run), labels)])
    }
    
    if (length(y) > 1) {
        gg <- ggplot(dfr,aes(time,value,col = run, fill = run)) + labs(x = 'Time', y = 'Predicted Value', col = 'Model\nrun', fill = 'Model\nrun')
    } else {
        gg <- ggplot(dfr,aes(time,value)) + labs(x = 'Time', y = 'Predicted Value')
    }
        
    gg <- gg + stat_summary(fun.min = function(x) quantile(x,0.025), fun.max = function(x) quantile(x,0.975), geom = 'ribbon', alpha = 0.3) +
        stat_summary(fun = function(x) mean(x), geom = 'line', lwd = 1.5) +
        theme_bw(base_size = 12)
    
    if (length(pars)>1)
        gg <- gg + facet_grid(label~., scales  =  'free_y')
    
    return(gg)
}
#}}}
