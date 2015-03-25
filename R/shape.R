
#{{{ accessor function
setGeneric("shape", function(x,par, ...)
  standardGeneric("shape"))
setMethod("shape",signature(x="edat",par="missing"),
          function(x,par) {return(x[['phi']])}
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
            
			if(value < 0 | value > 1)
				stop('Depletion at MSY must be: 0 < phi < 1\n')
			
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
