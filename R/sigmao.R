#'
#' Access or assign the observation error
#' 
#' By default the \pkg{bdm} package assumes that the observation error variance is fixed on input and specified in the \code{\link{bdmData}} object class. This function can be used to access or assign the standard deviation \eqn{\sigma_o} within an \code{bdmData} object.
#' 
#' Observation error is used to refer to the deviation of the abundance observation from the deterministic expectation:
#' \deqn{
#' E[I_{t}] = qB_{t}
#' }
#' where \eqn{B} is the biomass, \eqn{I} is the abundance index and \eqn{q} is the catchability scalar, which is estimated analytically as a nuisance parameter. The observation error is time variant and assumed by default to follow a log-normal distribution:
#' \deqn{
#' I_{t} \sim LN(ln(E[I_{t}])-\sigma^2_{o,t}/2, \sigma^2_{o,t})
#' }
#' The distribution of \eqn{I} around the value predicted by the model can be due to a variety of difference uncertainties, not just observation, and is sometimes referred to as the total error. Realistic values for the observation error are typically informed by standardisation of the abundance index time series. The default value is \eqn{\sigma_o = 0.2} for all time points. 
#' 
#' @param object an \code{bdmData} object
#' @param value a \code{numeric} vector or \code{matrix} for \eqn{\sigma_o}
#' @param ... additional arguments to the generic function
#' 
#' @return Accessor function returns a matrix of \eqn{\sigma_o} values. Assignment function populates the \code{bdmData} object.
#' 
#' @examples
#' # initialize bdmData object
#' dat <- bdmData(harvest = 20:30, index = cbind(runif(11), runif(11)))
#' 
#' # assign single value
#' sigmao(dat) <- 0.1
#' sigmao(dat)
#' 
#' # assign values specific
#' # to each index
#' sigmao(dat) <- c(0.05, 0.1)
#' sigmao(dat)
#' 
#' # assign values specific
#' # to each time
#' sigmao(dat) <- seq(0.05, 0.2, length = 11)
#' sigmao(dat)
#' 
#' # assign values specific
#' # to each time and index
#' sigmao(dat) <- matrix(runif(22), nrow = 11)
#' sigmao(dat)
#' 
#{{{ accessor function
#' @export
setGeneric("sigmao", function(object, ...) standardGeneric("sigmao"))
#' @rdname sigmao
setMethod("sigmao",signature(object = "bdmData"), function(object) return(object[['sigmao']]))
#}}}

#{{{ assignment functions
#' @rdname sigmao
#' @export
setGeneric("sigmao<-", function(object, value) standardGeneric("sigmao<-"))
#{{ numeric
#' @rdname sigmao
setMethod("sigmao<-",
          signature(object = "bdmData", value = "numeric"),
          function(object, value) {
            
              sigmao.dim    <- c(object$T, object$I)
              sigmao.length <- object$T * object$I
              
              if (length(value) < sigmao.length) {
                  if (sigmao.length %% length(value) != 0)
                      stop('dimensions for sigmao do not match dimensions for indices\n')
                  if (length(value) == 1)
                      value <- matrix(value, sigmao.dim[1], sigmao.dim[2])
                  if (length(value) == sigmao.dim[1])
                      value <- matrix(value, sigmao.dim[1], sigmao.dim[2], byrow = FALSE)
                  if (length(value) == sigmao.dim[2])
                      value <- matrix(value, sigmao.dim[1], sigmao.dim[2], byrow = TRUE)
              } else {
                  stop('dimensions for sigmao do not match dimensions for indices\n')
              }
              
            object$sigmao <- value  
            
            object$sigmao[object$index == -1] <- -1
            
            object
          }
)
#}}
#{{ matrix
#' @rdname sigmao
setMethod("sigmao<-",
          signature(object = "bdmData", value = "matrix"),
          function(object, value) {
              
              if (any(dim(value) != dim(object$index))) {
                stop('dimensions do not match index dimensions\n')
              } 
                
              object$sigmao <- value
              
              object$sigmao[object$index == -1] <- -1
              
              object
          }
)
#}}
#}}}
