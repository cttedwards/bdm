#'
#' Extract residuals
#' 
#' This function will extract the residual error values following a default model fit.
#' 
#' @param object an \code{\link{bdm}} object class
#' @param log a \code{logical} indicating whether residuals should be returned on a log scale
#' 
#' @return Returns a list containing the process error and total error (observation plus process error) residuals.
#' 
#' @method residuals bdm
#' @export
residuals.bdm <- function(object, log = TRUE, ...) {
  
 total_error    <- object@trace[['epsilon_o']]
 process_error  <- object@trace[['epsilon_p']]
  
 dimnames(total_error)   <- list(iterations = 1:object@nsamples, year = object@data$year,index = paste('index_',1:dim(object@data$index)[2],sep = ''))
 dimnames(process_error) <- list(iterations = 1:object@nsamples, year = object@data$year)
 
 if (log) { 
     return(list(total_error = log(total_error), process_error = log(process_error)))
 } else {
     return(list(total_error = total_error, process_error = process_error)) }
}