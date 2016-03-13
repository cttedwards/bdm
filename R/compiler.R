#'
#' Compile stan model code in \code{bdm} object
#' 
#' This function calls \code{rstan::stan_model} to compile the model.
#' 
#' @param object a \code{bdm} class object that contains the model code in \code{object@@model_code}
#' @param ... additional arguments to \code{stan_model}
#' 
#' @examples
#' library(bdm)
#' 
#' # initialise default model object
#' mdl <- bdm()
#' 
#' # compile Stan model code
#' \dontrun{
#' mdl <- compiler(mdl)
#' }
#' 
#' @return A \code{bdm} class object with a compiled DSO in \code{object@@dso}.
#' 
#' @include bdm.R
#' 
#' @import methods
#' @import rstan
#' 
#' @export
setGeneric("compiler", function(object, ...) standardGeneric("compiler"))
#'
#' @rdname compiler
setMethod("compiler", signature = "bdm", definition = function(object, ...) {
    
    stanc_list_return <- stanc(model_code = object@model_code, model_name = object@model_name, ...)
    stanmodel_object  <- stan_model(stanc_ret = stanc_list_return)
    
	object@mk_cppmodule <- stanmodel_object@mk_cppmodule
    object@model_cpp    <- stanmodel_object@model_cpp
    object@dso          <- stanmodel_object@dso
    
    return(object)
  
})
