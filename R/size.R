#'
#' Access or assign size at age
#' 
#' This function can be used to access or assign the size at age in an \code{\link{rdat}} object. 
#' 
#' Assignment can be of a list or numeric vector. When a list is assigned it should provide the parameters of the von Bertalanffy growth function: \deqn{Size = max(Linf * (1 - exp(-k*(Age-t0))),0).} namely \code{Linf}, \code{k} and \code{t0}. 
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
#'  \item{\code{mu}}{a list containing the parameters \code{Linf}, \code{k} and \code{t0} representing their mean values}
#'  \item{\code{cv}}{optional single numeric value or a list containing parameters \code{Linf}, \code{k} and \code{t0} representing their coefficients of variation}
#' }
#' 
#' @return Accessor function returns a matrix of size-at-age across iterations. Assignment function populates the matrix across iterations. If assignment is of a numeric vector of if no \code{cv} is provided then each iteration is identical.
#' 
#' @examples
#' 
#' amax <- 30
#' Linf <- 100
#' k    <- 0.08
#' t0   <- -0.5
#' 
#' # initialize rdat object
#' dat <- rdat(amax, iter = 1)
#' 
#' # assignment via numeric
#' size(dat) <- Linf * (1 - exp(-k*(1:amax - t0)))
#' size(dat)
#' 
#' # assignment via list
#' size(dat) <- list(mu=list(Linf = Linf, k = k, t0 = t0))
#' size(dat)
#' 
#' # increase number of iterations
#' dat <- iterate(dat, 100)
#' size(dat)
#' 
#' # assignment via list with overall uncertainty
#' size(dat) <- list(mu = list(Linf = Linf, k = k, t0 = t0), 
#'                      cv = 0.05)
#' size(dat)
#' 
#' # assignment via list with parameter uncertainty
#' size(dat) <- list(mu = list(Linf = Linf,k = k,t0 = t0), 
#'                      cv = list(Linf = 0.1, k = 0.2, t0 = 0.5))
#' size(dat)
#' 
#' 
#' @include rdat-class.R
#' 
#{{{ accessor function
#' @export
setGeneric("size", function(object, ...) standardGeneric("size"))
#' @rdname size
setMethod("size",signature(object = "rdat"),function(object) return(object@lhdat$size))
#}}}

#{{{ assignment function
#' @rdname size
#' @export
setGeneric("size<-", function(object,value) standardGeneric("size<-"))
#{{ list
#' @rdname size
setMethod("size<-",
          signature(object = "rdat", value = "list"),
          function(object, value) {
            
            Linf.mu <- value$mu$Linf
            k.mu    <- value$mu$k
            t0.mu   <- value$mu$t0
            
            if (!is.null(value$cv)) {
              if (is.list(value$cv)) {
                if (!is.null(value$cv$Linf)) {
                  Linf.sd <- value$cv$Linf * Linf.mu
                  Linf <- rnorm(object@iter,Linf.mu,Linf.sd)
                } else Linf <- rep(Linf.mu,object@iter)
                
                if (!is.null(value$cv$k)) {
                  k.sd <- value$cv$k * k.mu
                  k <- rnorm(object@iter,k.mu,k.sd)
                } else k <- rep(k.mu,object@iter)
                
                if (!is.null(value$cv$t0)) {
                  t0.sd <- abs(value$cv$t0 * t0.mu)
                  t0 <- rnorm(object@iter,t0.mu,t0.sd)
                } else t0 <- rep(t0.mu,object@iter)
                
                for (i in 1:object@iter)
                  object@lhdat[['size']][,i] <- Linf[i] * (1 - exp(-k[i]*(1:object@amax - t0[i])))
              } else {
                size.sd <- sqrt(log(1 + value$cv))
                object@lhdat[['size']] <- apply(object@lhdat[['size']],2,function(y) Linf.mu * (1 - exp(-k.mu*(c(1:object@amax) - t0.mu))) * rlnorm(1,-size.sd^2/2,size.sd))
              }
            } else {
              object@lhdat[['size']] <- apply(object@lhdat[['size']],2,function(y) Linf.mu * (1 - exp(-k.mu*(c(1:object@amax) - t0.mu))))
            }
            object@lhdat[['size']] <- apply(object@lhdat[['size']],2,function(y) { y[y < 0] <- 0; y })
            
            object
          }
)
#}}
#{{ numeric
#' @rdname size
setMethod("size<-",
          signature(object = "rdat", value = "numeric"),
          function(object, value) {
            
            size.mu <- value
            if (length(size.mu) < object@amax) 
              stop('length of size-at-age vector must equal number of age classes\n')
            
            object@lhdat[['size']] <- apply(object@lhdat[['size']],2,function(x) size.mu)
            
            object
          }
)
#}}
#}}}
