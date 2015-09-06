#'
#' Access or assign the process error
#' 
#' By default the \pkg{bdm} package assumes that the process error variance is fixed on input and specified in the \code{\link{edat}} object class. This function can be used to access or assign the standard deviation \eqn{\sigma_p} within an \code{edat} object.
#' 
#' Process error is used to refer to the deviation of the process equation from the deterministic expectation:
#' \deqn{
#' E[B_{t+1}] = max(B_{t} + g(B_{t}) - H_{t}, 0)
#' }
#' where \eqn{B} is the biomass, \eqn{g()} is the production function and \eqn{H} is the catch (or harvest). The process error is assumed by default to follow a log-normal distribution:
#' \deqn{
#' B_{t+1} \sim LN(ln(E[B_{t+1}])-\sigma^2_p/2, \sigma^2_p)
#' }
#' Realistic values for the process error are typically \eqn{0.05 < \sigma_p < 0.15}. The default value is \eqn{\sigma_p = 0.05}.
#' 
#' @param object an \code{edat} object
#' @param value a \code{numeric} value for \eqn{\sigma_p}
#' 
#' @return Accessor function returns a numeric value. Assignment function populates the \code{edat} object.
#' 
#' @examples
#' # initialize edat object
#' dat <- edat(harvest = 20:30, index = runif(11))
#' 
#' # assign and access sigmap
#' sigmap(dat) <- 0.1
#' sigmap(dat)
#' 
#{{{ accessor function
#'
#' @export
setGeneric("sigmap", function(object, ...) standardGeneric("sigmap"))
#'
#' @rdname sigmap
setMethod("sigmap",signature(object = "edat"),function(object) return(object[['sigmap']]))
#}}}

#{{{ assignment function
#'
#' @rdname sigmap
#' @export
setGeneric("sigmap<-", function(object,value, ...) standardGeneric("sigmap<-"))
#'
#' @rdname sigmap
setMethod("sigmap<-",
          signature(object = "edat",value = "numeric"),
          function(object, value) {
            
            if (length(value) > 1)
              warning('length of value is >1: only first value used\n')
            object$sigmap <- structure(value[1],.Dim=NULL)       
            
            object
          }
)
#}}}
  