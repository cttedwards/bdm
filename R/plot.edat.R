plot.edat <- function(object, ...)
{

	time    <- as.numeric(object[['time']])
	harvest <- object[['harvest']]
    index   <- object[['index']]
    
    index[index<0] <- NA
    
	cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
	
	gg <- ggplot() + 
        geom_bar(data=data.frame(time=time,value=harvest,label='Harvest'),aes(time,value), stat='identity', fill=cbPalette[1])
	
    for(i in 1:dim(index)[2]) {
        dfr <- data.frame(time=time,value=index[,i],label='Index')
        gg <- gg + 
            geom_point(data=dfr,aes(time,value),col=cbPalette[i]) +
            geom_line(data=dfr,aes(time,value),col=cbPalette[i])
    }
    
    gg <- gg + 
        facet_wrap(~label, scales = 'free_y') + 
        xlab('Time') +
	    ylab('') + 
        theme_bw()
    
	suppressMessages(suppressWarnings(print(gg)))
}