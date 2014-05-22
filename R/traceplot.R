
#{{{ plot traces
setMethod("traceplot",signature=list(object="bdm"),function(object, pars, inc_warmup = TRUE, ask = FALSE, ...) {
  
  if(missing(pars) & object@default_model) 
    pars <- c('r','logK','lp__')
  
  traceplot(object@fit,pars,inc_warmup,ask, ...)
  
})
#}}}
