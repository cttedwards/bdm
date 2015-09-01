#'
#' Compile stan model code in \code{bdm} object
#' 
#' This function calls \code{rstan::stan_model} to compile the model
#' 
#' @param object a \code{bdm} class object that contains the model code in \code{object@@model_code}
#' @param ... additional arguments to \code{stan_model}
#' 
#' @examples
#' # initialise default model object
#' mdl <- bdm(compile = FALSE)
#' 
#' # compile Stan model code
#' mdl <- compile(mdl)
#' 
#' @return a \code{bdm} class object with a compiled DSO in \code{object@@dso}
#' 
#' @seealso See the \href{https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started}{RStan Quick Start Guide} to ensure that \code{\link{rstan}} is installed correctly before compilation.
#' 
#' @export
#' 
compile <- function(object, ...) UseMethod("compile", ...)
#'
#' @rdname compile
#' 
#' @export
#' 
compile.bdm <- function(object, ...) {
    
  if (object@path != 'default_model' & object@path != 'local_declaration') {
    tmp <- stan_model(file = object@path, ...)
    object@model_code <- tmp@model_code
  } else {
    if (length(object@model_code) > 0) {
          tmp <- stan_model(model_code = object@model_code, ...)
          object@path  <- ifelse(object@default_model,'default_model','local_declaration')
    } else {
        stop('compilation error; check object@path and object@model_code\n')
    }
  }
  
  object@model_cpp  <- tmp@model_cpp
  object@dso        <- tmp@dso
  
  object
  
}
