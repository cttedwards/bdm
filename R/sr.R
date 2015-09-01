#'
#' @title Stock-recruitment function
#' 
#' @include rdat-class.R
#' 
#' @export
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
          signature(x="rdat",value="list"),
          function(x,i,j, ...,value) {
            
            if(!is.null(value$type)) 
              x@sr <- value$type
            else stop('must specify type of stock-recruitment function (either BH or RK)\n')
            
            if(!is.null(value$mu)) {
              if(!is.null(value$cv)) {
              
                if(x@sr=='BH') hmax <- 1
                if(x@sr=='RK') hmax <- 165
                
                # numerically serach for Beta distribution parameters
                # that give required mean and cv
                bdist.mu <- function(alpha,beta) alpha/(alpha+beta)
                bdist.sd <- function(alpha,beta) sqrt(alpha*beta / ((alpha+beta)^2 * (alpha+beta+1)))
                
                sr.mu <- function(alpha,beta) 0.2 + bdist.mu(alpha,beta) * (hmax - 0.2)
                sr.cv <- function(alpha,beta) bdist.sd(alpha,beta) * (hmax - 0.2) / sr.mu(alpha,beta)
                
                obj <- function(par) {
                  (sr.mu(par[1],par[2]) - value$mu)^2 + (sr.cv(par[1],par[2]) - value$cv)^2 
                }
                
                par.opt <- optim(c(3,2),obj)$par
                cat('optimised mean and cv:',round(sr.mu(par.opt[1],par.opt[2]),2),';',round(sr.cv(par.opt[1],par.opt[2]),2),'\ngiving Beta distribution parameters alpha =',round(par.opt[1],2), 'and beta =',round(par.opt[2],2),'\n')
                
                # export Beta distribution parameters to global
                # environment
                .alpha <<- par.opt[1]
                .beta  <<- par.opt[2]
                
                # simulate steepness values
                x@lhdat[['h']][] <- 0.2 + rbeta(x@iter,par.opt[1],par.opt[2]) * (hmax - 0.2)
              
              } else {
                x@lhdat[['h']][] <- value$mu
              }
              
            } else{
              if(!is.null(value$range)) {
                low <- value$range[1]
                upp <- value$range[2]
                x@lhdat[['h']][] <- runif(x@iter,low,upp)
              }   
            }
            
            x
            
          }
)
#}}}
