
#{{{ empirical data class
setClass("edat",contains="list",representation(names="character"))
setMethod("initialize","edat",function(.Object,index,catch,year) {
  
  .Object@.Data <- vector('list',5)
  names(.Object) <- c('T','N','index','catch','year')
  
  if(!missing(index)) {
    if(any(!is.na(index))) {
    
      index[which(is.na(index))] <- -1
      if(is.vector(index)) index <- as.matrix(index)
      
      .Object$T     <- T.index <- nrow(index)
      .Object$N     <- ncol(index)
      .Object$index <- index
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
  
  .Object
})
# constructor
edat <- function(index,catch,year) new("edat",index,catch,year)
#}}}
