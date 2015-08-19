#'
#' @title Size function
#' 
#' @export
#{{{ accessor function
setGeneric("size", function(x, ...)
  standardGeneric("size"))
setMethod("size",signature(x="rdat"),
          function(x) return(x@lhdat$size)
)
#}}}

#{{{ assignment function
setGeneric("size<-", function(x,i,j, ...,value) standardGeneric("size<-"))
#{{ list
setMethod("size<-",
          signature(x="rdat",value="list"),
          function(x,i,j, ...,value) {
            
            Linf.mu <- value$mu$Linf
            k.mu    <- value$mu$k
            t0.mu   <- value$mu$t0
            
            if(!is.null(value$cv)) {
              if(is.list(value$cv)) {
                if(!is.null(value$cv$Linf)) {
                  Linf.sd <- sqrt(log(1+value$cv$Linf^2))
                  Linf <- Linf.mu * rlnorm(x@iter,-Linf.sd^2/2,Linf.sd) 
                } else Linf <- rep(Linf.mu,x@iter)
                
                if(!is.null(value$cv$k)) {
                  k.sd <- value$cv$k * k.mu
                  k <- rnorm(x@iter,k.mu,k.sd)
                } else k <- rep(k.mu,x@iter)
                
                if(!is.null(value$cv$t0)) {
                  t0.sd <- abs(value$cv$t0 * t0.mu)
                  t0 <- rnorm(x@iter,t0.mu,t0.sd)
                  t0[t0>0] <- t0.mu
                } else t0 <- rep(t0.mu,x@iter)
                
                for(i in 1:x@iter)
                  x@lhdat[['size']][,i] <- Linf[i] * (1 - exp(-k[i]*(1:x@amax-t0[i])))
              } else {
                size.sd <- sqrt(log(1+value$cv))
                x@lhdat[['size']] <- apply(x@lhdat[['size']],2,function(y) Linf.mu * (1 - exp(-k.mu*(c(1:x@amax)-t0.mu))) * rlnorm(1,-size.sd^2/2,size.sd))
              }
            } else {
              x@lhdat[['size']] <- apply(x@lhdat[['size']],2,function(y) Linf.mu * (1 - exp(-k.mu*(c(1:x@amax)-t0.mu))))
            }
            x@lhdat[['size']] <- apply(x@lhdat[['size']],2,function(y) { y[y<0] <- 0; y })
            
            x
          }
)
#}}
#{{ numeric
setMethod("size<-",
          signature(x="rdat",value="numeric"),
          function(x,i,j, ...,value) {
            
            size.mu <- value
            if(length(size.mu)<x@amax) 
              stop('length of size-at-age vector must equal number of age classes\n')
            
            x@lhdat[['size']] <- apply(x@lhdat[['size']],2,function(x) size.mu)
            
            x
          }
)
#}}
#}}}
