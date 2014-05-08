
#{{{ compile stan model in bdm object
setGeneric("compile_bdm", function(.Object, ...) standardGeneric("compile_bdm"))
setMethod("compile_bdm",signature="bdm",function(.Object) {
  
  require(rstan)
  
  if(length(.Object@path)>0) {
    tmp <- stan_model(file=.Object@path)
    .Object@model_code <- tmp@model_code
  }
  else if(length(.Object@model_code)>0) {
    tmp <- stan_model(model_code=.Object@model_code)
    .Object@path  <- 'local declaration'
  }
  
  .Object@model_cpp  <- tmp@model_cpp
  .Object@dso        <- tmp@dso
  
  .Object
  
})
#}}}
