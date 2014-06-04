
#{{{ accessor function
setGeneric("sigmao", function(x, ...)
  standardGeneric("sigmao"))
setMethod("sigmao",signature(x="edat"),
          function(x) return(x[['sigmaO']])
)
#}}}

#{{{ assignment function
setGeneric("sigmao<-", function(x,i,j, ...,value) standardGeneric("sigmao<-"))
#{{ numeric
setMethod("sigmao<-",
          signature(x="edat",value="numeric"),
          function(x,i,j, ...,value) {
            
            if(length(value)<x$I) {
              value <- rep(value[1],x$I)
              if(length(value)>1)
                warning('length of input vector is >1 but < number of indices: only first value used\n')
            }
            x$sigmaO <- structure(value,.Dim=x$I)        
            
            x
          }
)
#}}
#}}}
