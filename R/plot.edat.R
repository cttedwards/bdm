#'
#' @title Plot data
#' 
#' @export
#' 
#' @include ggtheme.R
#' 
# S3 method for S4 edat class object
plot.edat <- function(object, ...)
{

	time    <- as.numeric(object[['time']])
	harvest <- object[['harvest']]
    index   <- object[['index']]
    
    index[index<0] <- NA
    
    plt <- rainbow(dim(index)[2])
    
    # color blind palette
    #plt <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
	
	gg <- ggplot() + 
        geom_bar(data = data.frame(time = time,value = harvest,label = 'Harvest'),aes(time,value), stat = 'identity', fill = "#999999")
	
    for (i in 1:dim(index)[2]) {
        dfr <- data.frame(time = time,value = index[,i],label = 'Index')
        gg <- gg + 
            geom_point(data = dfr,aes(time,value),col = plt[i], size = 4) +
            geom_line(data = dfr,aes(time,value),col = plt[i], size = 1.5)
    }
    
    gg <- gg + 
        facet_grid(label~., scales = 'free_y') + 
        xlab('Time') +
	    ylab('') + 
        ggtheme()
    
	return(gg)
}