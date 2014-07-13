
#{{{ accessor function
setGeneric("sigmap", function(x, ...)
  standardGeneric("sigmap"))
setMethod("sigmap",signature(x="edat"),
          function(x) return(x[['sigmap']])
)
#}}}

#{{{ assignment function
setGeneric("sigmap<-", function(x,i,j, ...,value) standardGeneric("sigmap<-"))
#{{ numeric
setMethod("sigmap<-",
          signature(x="edat",value="numeric"),
          function(x,i,j, ...,value) {
            
            if(length(value)>1)
              warning('length of value is >1: only first value used\n')
            x$sigmap <- structure(value[1],.Dim=NULL)       
            
            x
          }
)
#}}
#}}}
