#'
#' @title Extract residuals
#' 
#' @export
residuals.bdm <- function(object,log=TRUE, ...) {
  
 total_error    <- object@trace[['epsilon_o']]
 process_error  <- object@trace[['epsilon_p']]
  
 dimnames(total_error)   <- list(iterations=1:object@nsamples,year=object@data$year,index=paste('index_',1:dim(object@data$index)[2],sep=''))
 dimnames(process_error) <- list(iterations=1:object@nsamples,year=object@data$year)
 
 if(log) { return(list(total_error=log(total_error),process_error=log(process_error)))
 } else { return(list(total_error=total_error,process_error=process_error)) }
}