
#{{{ accessor function
setGeneric("mass", function(x, ...)
  standardGeneric("mass"))
setMethod("mass",signature(x="rdat"),
          function(x) return(x@lhdat$mass)
)
#}}}

#{{{ assignment function
setGeneric("mass<-", function(x,i,j, ...,value) standardGeneric("mass<-"))
#{{ list
setMethod("mass<-",
          signature(x="rdat",value="list"),
          function(x,i,j, ...,value) {
            
            a.mu <- value$mu$a
            b.mu <- value$mu$b
            
            if(!is.null(value$cv)) {
              if(is.list(value$cv)) {
                
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
              } else {
                mass.sd <- sqrt(log(1+value$cv))
                for(i in 1:x@iter)
                  x@lhdat[['mass']][,i] <- a.mu * x@lhdat[['size']][,i]^b.mu * rlnorm(1,-mass.sd^2/2,mass.sd)
              }
            } else {
              for(i in 1:x@iter)
                x@lhdat[['mass']][,i] <- a.mu * x@lhdat[['size']][,i]^b.mu
            }
            
            x
          }
)
#}}
#{{ numeric
setMethod("mass<-",
          signature(x="rdat",value="numeric"),
          function(x,i,j, ...,value) {
            
            mass.mu <- value
            if(length(mass.mu)<x@amax) 
              stop('length of mass-at-age vector must equal number of age classes\n')
            
            x@lhdat[['mass']] <- apply(x@lhdat[['mass']],2,function(x) mass.mu)
            
            x
          }
)
#}}
#}}}
