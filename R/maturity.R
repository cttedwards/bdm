
#{{{ accessor function
setGeneric("maturity", function(x, ...)
  standardGeneric("maturity"))
setMethod("maturity",signature(x="rdat"),
          function(x) return(x@lhdat$maturity)
)
#}}}

#{{{ assignment functions
setGeneric("maturity<-", function(x,i,j, ...,value) standardGeneric("maturity<-"))
#{{ list
setMethod("maturity<-",
          signature(x="rdat",value="list"),
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
                x@lhdat[['maturity']][,i] <- 1/(1+exp((acrit[i]-(1:x@amax))/delta[i]))
            } else {
              acrit.mu <- as.integer(acrit.mu)
              if(acrit.mu<=x@amax)
                x@lhdat[['maturity']][acrit.mu:x@amax,] <- 1
            }
            
            x
            
          }
)
#}}
#{{ numeric
setMethod("maturity<-",
          signature(x="rdat",value="numeric"),
          function(x,i,j, ...,value) {
            
            mat.mu <- value
            if(length(mat.mu)<x@amax)
                mat.mu[(length(mat.mu)+1):x@amax] <- rep(mat.mu[length(mat.mu)],x@amax-length(mat.mu))
            if(length(mat.mu)>x@amax)
                mat.mu <- mat.mu[1:x@amax]
            
            x@lhdat[['maturity']] <- apply(x@lhdat[['maturity']],2,function(x) mat.mu)
            
            x
          }
)
#}}
#}}}
