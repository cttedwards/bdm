
#{{{ compile stan model in bdm object
setGeneric("compile_bdm", function(.Object, ...) standardGeneric("compile_bdm"))
setMethod("compile_bdm",signature="bdm",function(.Object) {
  
  require(rstan)
  
  if(.Object@path!='default_model' & .Object@path!='local_declaration') {
    tmp <- stan_model(file=.Object@path)
    .Object@model_code <- tmp@model_code
  }
  else { if(length(.Object@model_code)>0) {
    tmp <- stan_model(model_code=.Object@model_code)
    .Object@path  <- ifelse(.Object@default_model,'default_model','local_declaration')
  }
  else stop('compilation error; check .Object@path and .Object@model_code\n')
  }
  
  .Object@model_cpp  <- tmp@model_cpp
  .Object@dso        <- tmp@dso
  
  .Object
  
})
#}}}
