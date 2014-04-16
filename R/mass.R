
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
                  b.sd <- value$cv$b * b.mu
                  bb <- rnorm(x@iter,b.mu,b.sd)
                } else bb <- rep(b.mu,x@iter)
                
                #x@lhdat[['mass']] <- x@lhdat[['size']]
                #x@lhdat[['mass']] <- t(apply(x@lhdat[['mass']],1,function(y) aa*y^bb))
                for(i in 1:x@iter)
                  x@lhdat[['mass']][,i] <- aa[i] * x@lhdat[['size']][,i]^bb[i] 
              } else {
                mass.sd <- sqrt(log(1+value$cv))
                for(i in 1:x@iter)
                  x@lhdat[['mass']][,i] <- a.mu * x@lhdat[['size']][,i]^b.mu * rlnorm(1,-mass.sd^2/2,mass.sd)
              }
            } else {
              x@lhdat[['mass']] <- x@lhdat[['size']]
              x@lhdat[['mass']] <- apply(x@lhdat[['mass']],2,function(y) a.mu*y^b.mu)
              #for(i in 1:x@iter)
              #  x@lhdat[['mass']][,i] <- a.mu * x@lhdat[['size']][,i]^b.mu
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
