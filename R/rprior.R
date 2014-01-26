
#{{{ rprior class
setClass("rprior",contains="numeric",slots=list(lognormal.par="list"))
setMethod("initialize","rprior",function(.Object,x) {
  
  if(missing(x)) .Object@.Data <- numeric()
  else {
    if(length(x)>1) .Object@.Data <- as.numeric(x)
    else            .Object@.Data <- numeric(x)
  }
  .Object
  
})
# constructor
rprior <- function(x=0L) new("rprior",x)
#}}}
