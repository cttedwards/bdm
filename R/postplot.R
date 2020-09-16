#'
#' Plot posterior updates from \code{bdm} model fit
#' 
#' Plots the posterior updates for $r$ and $log(K)$.
#' 
#' Depletion is measured as the biomass over the carrying capacity, harvest rate is the catch over the estimated biomass, and surplus production is the production function multiplied by the process error residual. 
#' 
#' @param x \code{bdm} class object.
#' @param type character vector of plot type. Must be one or more of \code{'histogram'} or \code{'point'}.
#' @param ... additional arguments to the generic function
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
#' 
#' @import ggplot2
#' 
#' @export
postplot <- function(x, ...) UseMethod("postplot")
#'
#' @rdname postplot
#' @export
postplot.bdm <- function(x, ..., type = "histogram", labels) {
  
    pars = c("r", "rPrior", "logK", "logKprior")
    
    y <- c(x, list(...))
    
    if (!missing(labels) & length(y) != length(labels)) {
      stop("'labels' vector length does not match number of models")  
    }
    
    if (type == "histogram") {
      
      dfr <- data.frame(iter = integer(), value = numeric(), label = character(), par = character(), run = character())
      for (i in 1:length(y)) {
          x <- y[[i]]
          
          if (!missing(labels)) {
              x@run <- labels[i]
          }
          if (length(x@run) == 0) {
              warning("'run' unspecified") 
          }
          
          for (par in pars) {
            
              par.dfr <- data.frame(iter = 1:x@nsamples, 
                                    value = x@trace[[par]], 
                                    label = ifelse(par %in% c("r", "logK"), "Posterior", "Prior"), 
                                    par = ifelse(par %in% c("r", "rPrior"), "r", "logK"), 
                                    run = ifelse(length(x@run) == 0, "", x@run))
              
              dfr <- rbind(dfr,par.dfr)
          }
      }
      
      if (!missing(labels)) {
        dfr$run <- factor(dfr$run)
        dfr$run <- factor(dfr$run, levels = levels(dfr$run)[match(levels(dfr$run), labels)])
      }
      
      gg <- ggplot(dfr) + geom_density(aes(x = value, fill = label), alpha = 0.4) + theme_bw(base_size = 16) + labs(xlab = "", ylab = "", fill = "")
      
      if (length(y) > 1) {
          gg <- gg + facet_grid(run~par, scales = "free")
      } else {
          gg <- gg + facet_wrap(~par, scales = "free")
      }
    
    } else {
      
      dfr <- data.frame(iter = integer(), value.r = numeric(), value.logK = numeric(), label = character(), run = character())
      for (i in 1:length(y)) {
          x <- y[[i]]
          
          if (!missing(labels)) {
            x@run <- labels[i]
          }
          if (length(x@run) == 0) {
              warning("'run' unspecified") 
          }
          
          par.dfr <- data.frame(iter = 1:x@nsamples, 
                                value.r = x@trace[["rPrior"]], 
                                value.logK = x@trace[["logKprior"]], 
                                label = "Prior", 
                                run = ifelse(length(x@run) == 0, "", x@run))
          
          dfr <- rbind(dfr,par.dfr)
          
          par.dfr <- data.frame(iter = 1:x@nsamples, 
                                value.r = x@trace[["r"]], 
                                value.logK = x@trace[["logK"]], 
                                label = "Posterior", 
                                run = ifelse(length(x@run) == 0, "", x@run))
          
          dfr <- rbind(dfr,par.dfr)
      }
      
      if (!missing(labels)) {
        dfr$run <- factor(dfr$run)
        dfr$run <- factor(dfr$run, levels = levels(dfr$run)[match(levels(dfr$run), labels)])
      }
      
      gg <- ggplot(dfr) + 
        geom_point(aes(x = value.r, y = value.logK, col = label), alpha = 0.4, size = 2) +
        labs(x = expression(r), y = expression(log(K)), col = "Samples") +
        theme_bw(base_size = 16)
      
      if (length(y) > 1) {
          gg <- gg + facet_wrap(~run)
      }
    }
    return(gg)
}
#}}}
