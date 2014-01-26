
#{{{ accessor function
setGeneric("mass", function(x, ...)
  standardGeneric("mass"))
setMethod("mass",signature(x="rdat"),
          function(x) return(x@lhdat$mass)
)
#}}}

#{{{ assignment function
setGeneric("mass<-", function(x,i,j, ...,value) standardGeneric("mass<-"))
setMethod("mass<-",
          signature(x="rdat"),
          function(x,i,j, ...,value) {
            
            a.mu <- value$mu$a
            b.mu <- value$mu$b
            
            if(!is.null(value$cv$a)) {
              a.sd <- sqrt(log(1+value$cv$a^2))
              aa <- a.mu * rlnorm(x@iter,-a.sd^2/2,a.sd) 
            } else aa <- rep(a.mu,x@iter)
            
            if(!is.null(value$cv$b)) {
              b.sd <- sqrt(log(1+value$cv$b^2))
              bb <- b.mu * rlnorm(x@iter,-b.sd^2/2,b.sd)
            } else bb <- rep(b.mu,x@iter)
            
            for(i in 1:x@iter)
              for(a in 1:x@amax)
                x@lhdat[['mass']][a,i] <- aa[i] * x@lhdat[['size']][a,i]^bb[i]
            
            x
          }
)
#}}}
