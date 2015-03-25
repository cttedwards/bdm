
#{{{ empirical data class
setClass("edat",contains="list",representation(names="character"))
setMethod("initialize","edat",function(.Object,index,harvest,time,n,sigmao,sigmap,renormalise=TRUE) {
  
  .Object@.Data <- vector('list',8)
  names(.Object) <- c('T','I','index','harvest','time','n','sigmao','sigmap')
  
  if(!missing(index)) {
    if(any(!is.na(index))) {
    
      index[which(is.na(index))] <- -1
      if(is.vector(index)) index <- as.matrix(index)
	  
      .Object$T <- T.index <- nrow(index)
      .Object$I <- ncol(index)
      
      .Object$index <- index
      
      # renormalise indices to (geometric) mean
      if(renormalise) {
        #renorm <- function(x) { y<-x[x>0]; x[x>0] <- y/(prod(y)^(1/length(y))); x }
		renorm <- function(x) { y<-x[x>0]; x[x>0] <- y/mean(y); x }
        if(.Object$I>1) {
          .Object$index <- apply(index,2,renorm)
          cat('Re-normalised indices.\n')
        } else {
          .Object$index <- renorm(index)
          cat('Re-normalised index.\n')
        }
      }
    }
  }
  
  if(!missing(harvest)) {
    if(any(!is.na(harvest))) {
    
      harvest[which(is.na(harvest))] <- -1
      
      .Object$T       <- T.harvest <- length(harvest)
      .Object$harvest <- harvest
    }
  }
  
  if(!missing(index) & !missing(harvest)) 
    if(any(!is.na(index)) & any(!is.na(harvest)))
      if(T.index!=T.harvest) 
        stop('index and catch (harvest) must have the same time dimension\n')
  
  if(!missing(time)) {
    .Object$time <- time
  } else .Object$time <- 1:length(harvest)
  
  .Object$n <- 2
  if(!missing(n))
    .Object$n <- n
  
  if(!missing(sigmao)) {
    if(length(sigmao)<.Object$I) {
		if(length(sigmao)>1)
			warning('length of sigmao is >1 but < number of indices: only first value used\n')
      sigmao <- rep(sigmao[1],.Object$I)
    }
	  .Object$sigmao <- structure(sigmao,.Dim=.Object$I)
  } else .Object$sigmao <- structure(rep(0.2,.Object$I),.Dim=.Object$I)
  
  if(!missing(sigmap)) {
    if(length(sigmap)>1)
      warning('length of sigmap is >1: only first value used\n')
	  .Object$sigmap <- structure(sigmap[1],.Dim=NULL)
  } else .Object$sigmap <- structure(0.05,.Dim=NULL)

  .Object
})
# constructor
edat <- function(index,harvest, ...) new("edat",index,harvest, ...)
#}}}
