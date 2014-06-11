
#{{{ empirical data class
setClass("edat",contains="list",representation(names="character"))
setMethod("initialize","edat",function(.Object,index,harvest,year,n,sigmaO,sigmaP,renormalise=TRUE) {
  
  .Object@.Data <- vector('list',8)
  names(.Object) <- c('T','I','index','harvest','year','n','sigmaO','sigmaP')
  
  if(!missing(index)) {
    if(any(!is.na(index))) {
    
      index[which(is.na(index))] <- -1
      if(is.vector(index)) index <- as.matrix(index)
	  
      .Object$T <- T.index <- nrow(index)
      .Object$I <- ncol(index)

      # renormalise indices to geometric mean
      if(renormalise) {
        renorm <- function(x) { y<-x[x>0]; x[x>0] <- y/(prod(y)^(1/length(y))); x }
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
  
  if(!missing(year)) {
    .Object$year <- year
  } else .Object$year <- 1:length(harvest)
  
  .Object$n <- 2
  if(!missing(n))
    .Object$n <- n
  
  if(!missing(sigmaO)) {
    if(length(sigmaO)<.Object$I) {
      sigmaO <- rep(sigmaO[1],.Object$I)
      if(length(sigmaO)>1)
        warning('length of sigmaO is >1 but < number of indices: only first value used\n')
    }
	  .Object$sigmaO <- structure(sigmaO,.Dim=.Object$I)
  } else .Object$sigmaO <- structure(rep(0.2,.Object$I),.Dim=.Object$I)
  
  if(!missing(sigmaP)) {
    if(length(sigmaP)>1)
      warning('length of sigmaP is >1: only first value used\n')
	  .Object$sigmaP <- structure(sigmaP[1],.Dim=NULL)
  } else .Object$sigmaP <- structure(0.05,.Dim=NULL)

  .Object
})
# constructor
edat <- function(index,harvest,year, ...) new("edat",index,harvest,year, ...)
#}}}
