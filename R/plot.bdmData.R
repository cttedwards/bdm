#'
#' Plot index and harvest data
#' 
#' This function provides a \code{plot} method for the \code{bdmData} object class. It returns a \code{ggplot} object that can be assigned and manipulated using functions provided by \pkg{ggplot2}.
#' 
#' @param x an \code{\link{bdmData}} object class
#' @param ... additional arguments to the generic function
#' 
#' @examples
#' # load Indian Ocean albacore data
#' data(albio)
#' 
#' # create bdmData object
#' dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = rownames(albio))
#' 
#' # plot
#' plot(dat)
#' 
#' @include ggtheme.R
#' 
#' @import ggplot2
#' 
#' @method plot bdmData
#' @export
plot.bdmData <- function(x, ...)
{

	time    <- as.numeric(x[['time']])
	harvest <- x[['harvest']]
    index   <- x[['index']]
    
    index[index < 0] <- NA
    
    #plt <- rainbow(dim(index)[2])
    
    # color blind palette
    plt <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
	
	gg <- ggplot() + 
        geom_bar(data = data.frame(time = time,value = harvest,label = 'Harvest'),aes(time,value), stat = 'identity', fill = "#999999")
	
    for (i in 1:dim(index)[2]) {
        dfr <- data.frame(time = time,value = index[,i],label = 'Index')
        gg <- gg + 
            geom_point(data = dfr,aes(time,value),col = plt[i], size = 4) +
            geom_line(data = dfr,aes(time,value),col = plt[i], size = 1.5)
    }
    
    gg <- gg + 
        facet_grid(label ~ ., scales = 'free_y') + 
        xlab('Time') +
	    ylab('') + 
        ggtheme()
    
	return(gg)
}