#' @title Initialize bdm object
#' 
#' @description Method to initialize bdm class object
#' 
#' @include bdm-class.R
#'
#{{{
# initialisation function
setMethod("initialize", "bdm", function(.Object, path, model.code, model.name, compile, default_model, ...) {
    
    if (missing(model.name)) 
        model.name <- 'BDM'
    
    if (!missing(path)) {
        .Object@path <- path
        .Object@model_name <- model.name
        if (compile) {
            tmp <- stan_model(file = .Object@path, ...)
            
            .Object@model_code <- tmp@model_code
            .Object@model_cpp  <- tmp@model_cpp
            .Object@dso        <- tmp@dso
        }
    }
    if (!missing(model.code)) {
        .Object@model_code <- model.code
        .Object@model_name <- model.name
        .Object@path       <- ifelse(default_model,'default_model','local_declaration')
        if (compile) {
            tmp <- stan_model(model_code = .Object@model_code, ...)
            
            .Object@model_cpp  <- tmp@model_cpp
            .Object@dso        <- tmp@dso
        }
    }
    
    .Object@chains <- 4
    .Object@iter   <- 2000
    .Object@thin   <- 1
    .Object@warmup <- floor(.Object@iter/2/.Object@thin)
    
    .Object@nsamples <- ((.Object@iter - .Object@warmup) * .Object@chains)/.Object@thin
    
    .Object@default_model <- default_model
    
    .Object
    
})
#}}}