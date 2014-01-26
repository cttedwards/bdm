
#{{{ accessor function
setGeneric("nmort", function(x, ...)
  standardGeneric("nmort"))
setMethod("nmort",signature(x="rdat"),
          function(x) return(x@lhdat$M)
)
#}}}

#{{{ assignment function
setGeneric("nmort<-", function(x,i,j, ...,value) standardGeneric("nmort<-"))
setMethod("nmort<-",
          signature(x="rdat"),
          function(x,i,j, ...,value) {
            
            M.mu <- value$mu$M
            if(length(M.mu)<x@amax)
              M.mu[(length(M.mu)+1):x@amax] <- rep(M.mu[length(M.mu)],x@amax-length(M.mu))
            
            if(!is.null(value$cv$M)) {
              M.sd <- sqrt(log(1+value$cv$M^2)) 
              for(i in 1:x@iter) 
                x@lhdat[['M']][,i]  <- M.mu * rlnorm(1,-M.sd^2/2,M.sd)
              
            } else {
              x@lhdat[['M']] <- apply(x@lhdat[['M']],2,function(x) M.mu)
            }
            
            # recalculate survivorship
            for(i in 1:x@iter)
              for(a in 1:x@amax)
                x@lhdat[['survivorship']][a,i] <- exp(-sum(x@lhdat[['M']][1:a,i]))
            
            x
          }
)
#}}}
