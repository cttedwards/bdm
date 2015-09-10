#'
#' Compile stan model code in \code{bdm} object
#' 
#' This function calls \code{rstan::stan_model} to compile the model.
#' 
#' @param object a \code{bdm} class object that contains the model code in \code{object@@model_code}
#' @param ... additional arguments to \code{stan_model}
#' 
#' @examples
#' # initialise default model object
#' mdl <- bdm()
#' 
#' # compile Stan model code
#' mdl <- compile(mdl)
#' 
#' @return A \code{bdm} class object with a compiled DSO in \code{object@@dso}.
#' 
#' @seealso See the \href{https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started}{RStan Quick Start Guide} to ensure that \code{\link{rstan}} is installed correctly before compilation.
#' 
#' @export
compile <- function(object, ...) UseMethod("compile")
#'
#' @rdname compile
#' 
#' @export
compile.bdm <- function(object, ...) {
    
    tmp <- rstan::stan_model(model_code = object@model_code, model_name = object@model_name,...)
    
    object@model_cpp  <- tmp@model_cpp
    object@dso        <- tmp@dso
    
    object
  
}
