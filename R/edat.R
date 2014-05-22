
#{{{ empirical data class
setClass("edat",contains="list",representation(names="character"))
setMethod("initialize","edat",function(.Object,index,catch,year,sigmaO,sigmaP,renormalise=TRUE) {
  
  .Object@.Data <- vector('list',7)
  names(.Object) <- c('T','I','index','catch','year','sigmaO','sigmaP')
  
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
  if(!missing(catch)) {
    if(any(!is.na(catch))) {
    
      catch[which(is.na(catch))] <- -1
      
      .Object$T     <- T.catch <- length(catch)
      .Object$catch <- catch
    }
  }
  if(!missing(index) & !missing(catch)) 
    if(any(!is.na(index)) & any(!is.na(catch)))
      if(T.index!=T.catch) 
        stop('index and catch must have the same time dimension\n')
  if(!missing(year)) {
    .Object$year <- year
  } else .Object$year <- 1:length(catch)
  .Object$sigmaO   <- numeric(2)
  if(!missing(sigmaO)) {
	.Object$sigmaO[] <- sigmaO
  } else .Object$sigmaO[] <- 0.2
  if(!missing(sigmaP)) {
	.Object$sigmaP <- sigmaP
  } else .Object$sigmaP <- 0.05

  .Object
})
# constructor
edat <- function(index,catch,year, ...) new("edat",index,catch,year, ...)
#}}}
