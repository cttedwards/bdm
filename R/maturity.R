#'
#' Access or assign maturity at age
#' 
#' This function can be used to access or assign the maturity at age in an \code{\link{rdat}} object. 
#' 
#' Assignment can be of a list or numeric vector. When a list is assigned it should provide the \eqn{a_{crit}} and optionally the \eqn{\delta} parameter. If only \code{acrit} is supplied then maturity is knife-edged at that age. If both \code{acrit} and \code{delta} are supplied then maturity-at-age is calculated as a logistic ogive: \deqn{Maturity = 1/(1+exp((acrit-Age)/delta)).}
#' 
#' Uncertainty accross iterations in the \code{rdat} object can be included by providing a \code{cv} for each of the parameters. Parameter uncertainty is assumed to be log-normal with \eqn{\sigma = \sqrt{ln(1+cv^2)}}.
#' 
#' When assigning a numeric vector it is assumed that each replicate is identical.
#' 
#' @param object a \code{\link{rdat}} object
#' @param value either a named \code{list} or \code{numeric} vector 
#' \describe{
#'  \item{\code{numeric}}{a vector of length equal to the maximum age}
#'  \item{\code{list}}{a list object containing the components \code{mu} and \code{cv}}
#'  \item{\code{mu}}{a list containing the parameters \code{acrit} and (optionally) \code{delta} representing their mean values}
#'  \item{\code{cv}}{optional single numeric value or a list containing parameters \code{acrit} and \code{delta} representing their coefficients of variation}
#' }
#' 
#' @return Accessor function returns a matrix of maturity at age across iterations. Assignment function populates the matrix across iterations.
#' 
#' @examples
#' 
#' # initialize single iteration
#' dat <- rdat(amax = 30, iter = 1)
#' 
#' # add knife-edged maturity
#' maturity(dat) <- list(mu = list(acrit = 10))
#' maturity(dat)
#' 
#' # increase iterations
#' dat <- iterate(dat, iter = 10)
#' maturity(dat)
#' 
#' # add logistic maturity ogive
#' # with uncertainty
#' maturity(dat) <- list(mu=list(acrit = 1, delta = 3), 
#'                          cv=list(acrit = 0.01, delta = 0.01))
#' maturity(dat)

#' 
#{{{ accessor function
#' @export
setGeneric("maturity", function(object, ...) standardGeneric("maturity"))
#' @rdname maturity
setMethod("maturity",signature(object = "rdat"), function(object) return(object@lhdat$maturity))
#}}}

#{{{ assignment functions
#' @rdname maturity
#' @export
setGeneric("maturity<-", function(object, value) standardGeneric("maturity<-"))
#{{ list
#' @rdname maturity
setMethod("maturity<-",
          signature(object = "rdat", value = "list"),
          function(object, value) {
            
            acrit.mu <- value$mu$acrit
            delta.mu <- value$mu$delta
            
            if (!is.null(delta.mu)) {
              
              if (!is.null(value$cv$acrit)) {
                acrit.sd <- sqrt(log(1 + value$cv$acrit^2))
                acrit <- acrit.mu * rlnorm(object@iter,-acrit.sd^2/2,acrit.sd)
              } else acrit <- rep(acrit.mu,object@iter)
              
              if (!is.null(value$cv$delta)) {
                delta.sd <- sqrt(log(1 + value$cv$delta^2))
                delta <- delta.mu * rlnorm(object@iter,-delta.sd^2/2,delta.sd)
              } else delta <- rep(delta.mu,object@iter)
              
              for (i in 1:object@iter)
                object@lhdat[['maturity']][,i] <- 1/(1 + exp((acrit[i] - (1:object@amax))/delta[i]))
            } else {
              acrit.mu <- as.integer(acrit.mu)
              if (acrit.mu <= object@amax)
                object@lhdat[['maturity']][acrit.mu:object@amax,] <- 1
            }
            
            object
            
          }
)
#}}
#{{ numeric
#' @rdname maturity
setMethod("maturity<-",
          signature(object = "rdat",value = "numeric"),
          function(object,value) {
            
            mat.mu <- value
            if (length(mat.mu) < object@amax)
                mat.mu[(length(mat.mu) + 1):object@amax] <- rep(mat.mu[length(mat.mu)],object@amax - length(mat.mu))
            if (length(mat.mu) > object@amax)
                mat.mu <- mat.mu[1:object@amax]
            
            object@lhdat[['maturity']] <- apply(object@lhdat[['maturity']],2,function(object) mat.mu)
            
            object
          }
)
#}}
#}}}
