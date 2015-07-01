
#{{{ accessor function
setGeneric("sigmao", function(x, ...)
  standardGeneric("sigmao"))
setMethod("sigmao",signature(x="edat"),
          function(x) return(x[['sigmao']])
)
#}}}

#{{{ assignment function
setGeneric("sigmao<-", function(x,i,j, ...,value) standardGeneric("sigmao<-"))
#{{ numeric
setMethod("sigmao<-",
          signature(x="edat",value="numeric"),
          function(x,i,j, ...,value) {
            
            if(length(value)<x$I) {
              if(length(value)>1)
                warning('length of input vector is >1 but < number of indices: only first value used\n')
              value <- rep(value[1],x$I)
            }
            x$sigmao <- structure(value,.Dim=x$I)        
            
            x
          }
)
#}}
#{{ numeric
setMethod("sigmao<-",
          signature(x="edat",value="matrix"),
          function(x,i,j, ...,value) {
              
              if(any(dim(value) != dim(x$index))) {
                warning('dimensions do not match index dimensions\n')
              } else {
                x$sigmao <- structure(value, .Dim = dim(x$index))        
              }
              
              x
          }
)
#}}
#}}}
