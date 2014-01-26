
#{{{ accessor function
setGeneric("maturity", function(x, ...)
  standardGeneric("maturity"))
setMethod("maturity",signature(x="rdat"),
          function(x) return(x@lhdat$maturity)
)
#}}}

#{{{ assignment function
setGeneric("maturity<-", function(x,i,j, ...,value) standardGeneric("maturity<-"))
setMethod("maturity<-",
          signature(x="rdat"),
          function(x,i,j, ...,value) {
            
            acrit.mu <- value$mu$acrit
            delta.mu <- value$mu$delta
            
            if(!is.null(delta.mu)) {
              
              if(!is.null(value$cv$acrit)) {
                acrit.sd <- sqrt(log(1+value$cv$acrit^2))
                acrit <- acrit.mu * rlnorm(x@iter,-acrit.sd^2/2,acrit.sd)
              } else acrit <- rep(acrit.mu,x@iter)
              
              if(!is.null(value$cv$delta)) {
                delta.sd <- sqrt(log(1+value$cv$delta^2))
                delta <- delta.mu * rlnorm(x@iter,-delta.sd^2/2,delta.sd)
              } else delta <- rep(delta.mu,x@iter)
              
              for(i in 1:x@iter)
                for(a in 1:x@amax)
                  x@lhdat[['maturity']][a,i] <- 1/(1+exp((acrit[i]-a)/delta[i]))
            } else {
              acrit.mu <- as.integer(acrit.mu)
              if(acrit.mu<=x@amax)
                x@lhdat[['maturity']][acrit.mu:x@amax,] <- 1
            }
            
            x
            
          }
)
#}}}
