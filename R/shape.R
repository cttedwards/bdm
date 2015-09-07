#'
#' Access or assign shape of the production function
#' 
#' This function can used to access or assign the shape of the generalised production function. The shape is assumed to be fixed on input and is therefore contained within the \code{edat} object class as the parameter \eqn{n}.
#' 
#' The \pkg{bdm} package assumes by default a generalised Fletcher-Schaefer hybrid production function. The inflection points occurs at: \deqn{\phi = B_{MSY}/K = (1/n)^(1/(n-1)).} 
#' 
#' The discontinuity in the relationship between \eqn{phi} and \eqn{n} is accommodated by assuming \eqn{\phi = 1/e} at \eqn{n = 1}.
#' 
#' @param object an \code{edat} class object
#' @param par a character string of either \code{n} or \code{phi}
#' @param value a numeric value specifying the value of either \code{n} or \code{phi}
#' 
#' @examples
#' # initialize edat object
#' dat <- edat(harvest = 20:30, index = runif(11))
#' 
#' # access default shape
#' shape(dat, 'phi')
#' shape(dat, 'n')
#' 
#' # assign shape
#' shape(dat) <- 0.4
#' shape(dat)
#' 
#{{{ accessor function
#' @export
setGeneric("shape", function(object, ...) standardGeneric("shape"))
#' @rdname shape
setMethod("shape",signature(object = "edat"),
          function(object, par = "phi") {
              
            n <- object[['n']]
              
            if (par == "n") {
                return(n)
            }
            else if (par == "phi") {
                if (n == 1) return(exp(-1))
                else return((1/n)^(1/(n-1))) 
            }
          }
)
#}}}

#{{{ assignment function
#' @rdname shape
#' @export
setGeneric("shape<-", function(object, ..., value) standardGeneric("shape<-"))
#' @rdname shape
setMethod("shape<-",
          signature(object = "edat", value = "numeric"),
          function(object, par = "phi", value) {
            
            if (par == "phi") {
                if (value <= 0 | value >= 1)
                    stop('Depletion at MSY must be: 0 < phi < 1\n')
                # numerical solution to equation for dmsy
                # in terms of paramter n
                obj.func <- function(n) {
                    dmsy <- ifelse(n == 1, exp(-1), (1/n)^(1/(n-1)))
                    dmsy - value
                }
                object$n <- uniroot(obj.func,interval = c(0, 100))$root  
            } else if (par == "n") {
                object$n <- value
            }
            
            object
          }
)
#}}
#}}}
