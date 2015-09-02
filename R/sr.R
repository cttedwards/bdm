#'
#' Stock-recruitment function
#' 
#' This function can be used to access or assign the stock recruitment function in an \code{\link{rdat}} object.
#' 
#' When executing \code{\link{rcalc}} the recruitment functions are used to calculate the recruits per spawner, using steepness and the equilibrium spawning biomass per recruit \eqn{SBPR}. For the Beverton-Holt stock recruitment function recruits per spawner is \deqn{Recruits Per Spawner = 4h/(SBPR * (1 - h)),} and for the Ricker function it is \deqn{Recruits Per Spawner = h^1.25 / (SBPR * exp(ln(0.2)/0.8)).} To introduce uncertainty the steepness is represented as: \deqn{h_i = 0.2 + rbeta(\alpha,\beta)(h^{MAX} - 0.2)} where \eqn{h^{MAX}=1} for the Beverton-Holt function and \eqn{h^{MAX}\approx 165} for the Ricker function. The \eqn{\alpha} and \eqn{\beta} parameters of the \eqn{Beta} distribution are found using a numerical search algorithm to give mean and uncertainty values approximately equal to those input. 
#' 
#' @param object a \code{rdat} object
#' @param value a \code{list} containing the components \code{type}, \code{mu} and \code{cv}
#' \describe{
#' \item{\code{type}}{can be either \code{'BH'} or \code{'RK'} indicating the Beverton-Holt and Ricker curves respectively}
#' \item{\code{mu}}{a list containing the single steepness parameter \code{h} which represents the mean value}
#' \item{\code{cv}}{a list containing the single steepness parameter \code{h} which represents the coefficient of variation}
#' }
#' 
#' @examples
#' # single iteration
#' dat <- rdat(amax = 30,iter=1)
#' sr(dat) <- list(type='BH',mu=0.75)
#' sr(dat)
#' 
#' # multiple stochastic iterations
#' dat <- rdat(amax = 30, iter = 10)
#' sr(dat) <- list(type='BH', mu=0.75, cv=0.1)
#' sr(dat)
#' 
#' # alpha and beta parameters are returned 
#' # as invisible objects for diagnostic plots
#' .alpha
#' .beta
#' 
#' @include rdat-class.R
#' 
#{{{ accessor function
#' @export
setGeneric("sr", function(object, ...) standardGeneric("sr"))
#' @rdname sr
setMethod("sr",signature(object = "rdat"), function(object) return(list(type = object@sr, h = object@lhdat$h)))
#}}}

#{{{ assignment function
#' @rdname sr
#' @export
setGeneric("sr<-", function(object, value) standardGeneric("sr<-"))
#' @rdname sr
setMethod("sr<-",
          signature(object = "rdat",value = "list"),
          function(object,value) {
            
            if (!is.null(value$type)) 
              object@sr <- value$type
            else stop('must specify type of stock-recruitment function (either BH or RK)\n')
            
            if (!is.null(value$mu)) {
              if (!is.null(value$cv)) {
              
                if (object@sr == 'BH') hmax <- 1
                if (object@sr == 'RK') hmax <- 165
                
                # numerically serach for Beta distribution parameters
                # that give required mean and cv
                bdist.mu <- function(alpha,beta) alpha/(alpha + beta)
                bdist.sd <- function(alpha,beta) sqrt(alpha*beta / ((alpha + beta)^2 * (alpha+beta+1)))
                
                sr.mu <- function(alpha,beta) 0.2 + bdist.mu(alpha,beta) * (hmax - 0.2)
                sr.cv <- function(alpha,beta) bdist.sd(alpha,beta) * (hmax - 0.2) / sr.mu(alpha,beta)
                
                obj <- function(par) {
                  (sr.mu(par[1],par[2]) - value$mu)^2 + (sr.cv(par[1],par[2]) - value$cv)^2 
                }
                
                par.opt <- optim(c(3,2),obj)$par
                message('optimised mean and cv: ',round(sr.mu(par.opt[1],par.opt[2]),2),';',round(sr.cv(par.opt[1],par.opt[2]),2),'\ngiving Beta distribution parameters alpha = ',round(par.opt[1],2), ' and beta = ',round(par.opt[2],2))
                
                # export Beta distribution parameters to global
                # environment
                .alpha <<- par.opt[1]
                .beta  <<- par.opt[2]
                
                # simulate steepness values
                object@lhdat[['h']][] <- 0.2 + rbeta(object@iter,par.opt[1],par.opt[2]) * (hmax - 0.2)
              
              } else {
                object@lhdat[['h']][] <- value$mu
              }
              
            } else{
              if (!is.null(value$range)) {
                low <- value$range[1]
                upp <- value$range[2]
                object@lhdat[['h']][] <- runif(object@iter,low,upp)
              }   
            }
            
            object
            
          }
)
#}}}
