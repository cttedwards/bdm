#'
#' @title Natural mortality function
#' 
#' @include rdat-class.R
#' 
#' @export
#{{{ accessor function
setGeneric("nmort", function(x, ...)
  standardGeneric("nmort"))
setMethod("nmort",signature(x="rdat"),
          function(x) return(x@lhdat$M)
)
#}}}

#{{{ assignment functions
setGeneric("nmort<-", function(x,i,j, ...,value) standardGeneric("nmort<-"))
#{{ list
setMethod("nmort<-",
          signature(x="rdat",value="list"),
          function(x,i,j, ...,value) {
            
            if(!is.null(value$mu)) {
              M.mu <- value$mu
              if(length(M.mu)<x@amax)
                M.mu[(length(M.mu)+1):x@amax] <- rep(M.mu[length(M.mu)],x@amax-length(M.mu))
              
              if(!is.null(value$cv)) {
                M.sd <- sqrt(log(1+value$cv^2)) 
                x@lhdat[['M']] <- apply(x@lhdat[['M']],2,function(y) M.mu * rlnorm(1,-M.sd^2/2,M.sd))
                if(!is.null(value$range)) {
                  low <- value$range[1]
                  upp <- value$range[2]
                  x@lhdat[['M']] <- apply(x@lhdat[['M']],2,function(y) ifelse(y>upp|y<low,M.mu,y))
                }
              } else {
                x@lhdat[['M']] <- apply(x@lhdat[['M']],2,function(y) M.mu)
              }
            } else { 
              if(!is.null(value$range)) {
                low <- value$range[1]
                upp <- value$range[2]
                x@lhdat[['M']] <- apply(x@lhdat[['M']],2,function(y) rep(runif(1,low,upp),x@amax))
              } else {
                stop('must supply mean or range of values\n')
              }
            }
            
            # calculate survivorship (assuming constant M-at-age)
            x@lhdat[['survivorship']] <- x@lhdat[['M']]
            x@lhdat[['survivorship']] <- apply(x@lhdat[['survivorship']],2,function(y) {  m <- y; 
                                                                                          y[1] <- 1;
                                                                                          for(a in 2:x@amax)
                                                                                            y[a] <- y[a-1]*exp(-m[a-1]);
                                                                                          y
                                                                                         })
            # calculate survivorship (for age dependent M)
            #for(i in 1:x@iter)
            #  for(a in 1:x@amax)
            #    x@lhdat[['survivorship']][a,i] <- exp(-sum(x@lhdat[['M']][1:a,i]))
            
            x
          }
)
#}}
#{{ numeric
setMethod("nmort<-",
          signature(x="rdat",value="numeric"),
          function(x,i,j, ...,value) {
            
            M.mu <- value
            if(length(M.mu)<x@amax)
              M.mu[(length(M.mu)+1):x@amax] <- rep(M.mu[length(M.mu)],x@amax-length(M.mu))
            
            x@lhdat[['M']] <- apply(x@lhdat[['M']],2,function(y) M.mu)
                        
            # recalculate survivorship
            for(i in 1:x@iter)
              for(a in 1:x@amax)
                x@lhdat[['survivorship']][a,i] <- exp(-sum(x@lhdat[['M']][1:a,i]))
            
            x
          }
)
#}}
#}}}
