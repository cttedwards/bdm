
#{{{ diagnostic plot
setGeneric("idxplot", function(.Object, ...) standardGeneric("idxplot"))
setMethod("idxplot",signature=c("bdm"), function(.Object, ...) {
  
  nidx <- .Object@data[['I']]
  time <- .Object@data[['time']]
  nsamples <- .Object@nsamples
  
  # color blind palette
  plt <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  #######################################################
  # code for extraction of iterations from object@trace #
  # (list of parameter arrays with all chains combined) #
  #######################################################
  dfr <- data.frame(iter=integer(),time=integer(),value=numeric(),label=character())
  idx.arr <- .Object@trace[['predicted_index']]
  dimnames(idx.arr) <- list(iter=1:nsamples,time=time,index=1:nidx)
  for(i in 1:nidx) {
      
      idx.dfr <- melt(idx.arr[,,i])
      idx.dfr <- data.frame(idx.dfr,label=paste('index:',i,sep=''))
      
      dfr <- rbind(dfr,idx.dfr)
  }
  
  gg <- ggplot(dfr,aes(time,value)) + 
      #stat_summary(fun.ymin=function(x) quantile(x,0.025),fun.ymax=function(x) quantile(x,0.975),geom='ribbon',alpha=0.3) +
      #stat_summary(fun.y=function(x) mean(x),geom='line',lwd=1.5) +
      labs(x='Time',y='Predicted Value') +
      .theme_bdm()
  
  dfr.empirical <- .Object@data$index
  dimnames(dfr.empirical) <- list(time = .Object@data$time, label = paste('index:',1:nidx,sep=''))
  dfr.empirical[dfr.empirical < 0] <- NA
  dfr.empirical <- melt(dfr.empirical)
  
  for(i in 1:nidx) {
    gg <- geom_point(data=dfr.empirical,aes(time,value)) + 
          geom_line(data=dfr.empirical,aes(time,value))
  }
  
  if(length(nidx)>1)
      gg <- gg + facet_grid(label~.)
  
  return(gg)
})
#}}}
