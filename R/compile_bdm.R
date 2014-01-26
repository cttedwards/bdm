
#{{{ compile stan model in bdm object
setGeneric("compile_bdm", function(.Object,model_code, ...) standardGeneric("compile_bdm"))
setMethod("compile_bdm",signature="bdm",function(.Object,model_code) {
  
  require(rstan)
  
  if(!missing(model_code)) .Object@model_code <- model_code
  
  tmp <- stan_model(model_code=.Object@model_code)
  
  .Object@model_name <- 'BDM'
  .Object@model_cpp  <- tmp@model_cpp
  .Object@dso        <- tmp@dso
  
  .Object
  
})
#}}}
