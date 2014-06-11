
#{{{ accessor function
setGeneric("shape", function(x,par, ...)
  standardGeneric("shape"))
setMethod("shape",signature(x="edat",par="missing"),
          function(x,par) {return(x[['n']])}
)
setMethod("shape",signature(x="edat",par="character"),
          function(x,par) {
            if(par=="n")   return(x[['n']])
            if(par=="phi") return((1/x[['n']])^(1/(x[['n']]-1))) 
          }
)
#}}}

#{{{ assignment function
setGeneric("shape<-", function(x,i,j, ...,value) standardGeneric("shape<-"))
#{{ numerical solution to equation for dmsy in terms of paramter n
setMethod("shape<-",
          signature(x="edat",value="numeric"),
          function(x,i,j, ...,value) {
            
            obj.func <- function(n) {
              dmsy <- (1/n)^(1/(n-1))
              dmsy - value
            }
            x$n <- uniroot(obj.func,interval=c(0,10))$root      
            
            x
          }
)
#}}
#}}}
