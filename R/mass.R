#'
#' Access or assign mass at age
#' 
#' This function can be used to access or assign the biomass at age in an \code{\link{rdat}} object. 
#' 
#' Assignment can be of a list or numeric vector. When a list is assigned it should provide the \eqn{a} and \eqn{b} parameters of the function: \deqn{Mass = a * Length ^ b.}
#' 
#' Uncertainty accross iterations in the \code{rdat} object can be included by providing either an overall coefficient of variation (\code{cv}) or a \code{cv} for each of the parameters. Overall uncertainty is assumed to be log-normal with \eqn{\sigma = \sqrt{ln(1+cv^2)}}. Parameter uncertainty is assumed to be normal with \eqn{\sigma = \mu * cv}.
#' 
#' When assigning a numeric vector it is assumed that each replicate is identical.
#' 
#' @param object a \code{\link{rdat}} object
#' @param value either a named \code{list} or \code{numeric} vector 
#' \describe{
#'  \item{\code{numeric}}{a vector of length equal to the maximum age}
#'  \item{\code{list}}{a list object containing the components \code{mu} and \code{cv}}
#'  \item{\code{mu}}{a list containing the parameters \code{a} and \code{b} representing their mean values}
#'  \item{\code{cv}}{optional single numeric value or a list containing parameters \code{a} and \code{b} representing their coefficients of variation}
#' }
#' 
#' @return Accessor function returns a matrix of mass at age across iterations. Assignment function populates the matrix across iterations.
#' 
#' @examples
#' 
#' # initialize single iteration
#' dat <- rdat(amax = 30, iter = 1)
#' 
#' # add length data
#' size(dat) <- list(mu=list(Linf = 100, k = 0.1, t0 = -0.5))
#' size(dat)
#' 
#' # mass at age
#' mass(dat) <- list(mu = list(a = 9.63e-6, b = 3.173))
#' mass(dat)
#' 
#' # iterate
#' dat <- iterate(dat, iter = 10)
#' size(dat)
#' mass(dat)
#' 
#' # multiple stochastic iterations
#' # with overall uncertainty
#' mass(dat) <- list(mu = list(a = 9.63e-6,b = 3.173), cv = 0.05)
#' mass(dat)
#' 
#' # multiple stochastic iterations
#' # with parameter uncertainty
#' mass(dat) <- list(mu = list(a = 9.63e-6,b = 3.173), cv = list(a = 0.05, b = 0.05))
#' mass(dat)
#
#' 
#{{{ accessor function
#' @export
setGeneric("mass", function(object, ...) standardGeneric("mass"))
#' @rdname mass
setMethod("mass",signature(object = "rdat"), function(object) return(object@lhdat$mass))
#}}}

#{{{ assignment function
#' @rdname mass
#' @export
setGeneric("mass<-", function(object, value) standardGeneric("mass<-"))
#{{ list
#' @rdname mass
setMethod("mass<-",
          signature(object = "rdat", value = "list"),
          function(object, value) {
            
            a.mu <- value$mu$a
            b.mu <- value$mu$b
            
            if (!is.null(value$cv)) {
              if (is.list(value$cv)) {
                
                if (!is.null(value$cv$a)) {
                  a.sd <-  value$cv$a * a.mu
                  aa <- rnorm(object@iter,a.mu,a.sd)
                } else aa <- rep(a.mu,object@iter)
                
                if (!is.null(value$cv$b)) {
                  b.sd <- value$cv$b * b.mu
                  bb <- rnorm(object@iter,b.mu,b.sd)
                } else bb <- rep(b.mu,object@iter)
                
                #object@lhdat[['mass']] <- object@lhdat[['size']]
                #object@lhdat[['mass']] <- t(apply(object@lhdat[['mass']],1,function(y) aa*y^bb))
                for (i in 1:object@iter)
                  object@lhdat[['mass']][,i] <- aa[i] * object@lhdat[['size']][,i]^bb[i] 
              } else {
                mass.sd <- sqrt(log(1 + value$cv))
                for (i in 1:object@iter)
                  object@lhdat[['mass']][,i] <- a.mu * object@lhdat[['size']][,i]^b.mu * rlnorm(1,-mass.sd^2/2,mass.sd)
              }
            } else {
              object@lhdat[['mass']] <- object@lhdat[['size']]
              object@lhdat[['mass']] <- apply(object@lhdat[['mass']],2,function(y) a.mu*y^b.mu)
              #for(i in 1:object@iter)
              #  object@lhdat[['mass']][,i] <- a.mu * object@lhdat[['size']][,i]^b.mu
            }
            
            object
          }
)
#}}
#{{ numeric
#' @rdname mass
setMethod("mass<-",
          signature(object = "rdat",value = "numeric"),
          function(object, value) {
            
            mass.mu <- value
            if (length(mass.mu) < object@amax) 
              stop('length of mass-at-age vector must equal number of age classes\n')
            
            object@lhdat[['mass']] <- apply(object@lhdat[['mass']],2,function(object) mass.mu)
            
            object
          }
)
#}}
#}}}
