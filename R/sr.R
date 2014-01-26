
#{{{ accessor function
setGeneric("sr", function(x, ...)
  standardGeneric("sr"))
setMethod("sr",signature(x="rdat"),
          function(x) return(list(type=x@sr,h=x@lhdat$h))
)
#}}}

#{{{ assignment function
setGeneric("sr<-", function(x,i,j, ...,value) standardGeneric("sr<-"))
setMethod("sr<-",
          signature(x="rdat"),
          function(x,i,j, ...,value) {
            
            if(!is.null(value$type)) 
              x@sr <- value$type
            else stop('must specify type of stock-recruitment function (either BH or R)\n')
            
            if(!is.null(value$cv$h)) {
              
              if(x@sr=='BH') hmax <- 1
              if(x@sr=='R')  hmax <- 165
              
              bdist.mu <- function(alpha,beta) alpha/(alpha+beta)
              bdist.sd <- function(alpha,beta) sqrt(alpha*beta / ((alpha+beta)^2 * (alpha+beta+1)))
              
              sr.mu <- function(alpha,beta) 0.2 + bdist.mu(alpha,beta) * (hmax - 0.2)
              sr.cv <- function(alpha,beta) bdist.sd(alpha,beta) * (hmax - 0.2) / sr.mu(alpha,beta)
              
              obj <- function(par) {
                (sr.mu(par[1],par[2]) - value$mu$h)^2 + (sr.cv(par[1],par[2]) - value$cv$h)^2 
              }
              
              par.opt <- optim(c(3,2),obj)$par
              cat('optimised mean and cv:',round(sr.mu(par.opt[1],par.opt[2]),2),';',round(sr.cv(par.opt[1],par.opt[2]),2),'\n')
              
              x@lhdat[['h']][] <- 0.2 + rbeta(x@iter,par.opt[1],par.opt[2]) * (hmax - 0.2)
              
            } else {
              if(!is.null(value$mu$h))
                x@lhdat[['h']][] <- value$mu$h
            }
            
            x
            
          }
)
#}}}
