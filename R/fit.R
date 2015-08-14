
#{{{ fit functions
setGeneric("fit", function(.Object,data, ...) standardGeneric("fit"))
#{{ fit bdm model to data
setMethod("fit",signature=c("bdm","list"),function(.Object,data,init,chains,iter,warmup,thin,run,method="MCMC", ...) {
  
  if(missing(data)) 
    stop('No data object supplied\n')
  
  # number of data indices
  # nix <- ifelse(length(dim(data$index))>1,dim(data$index)[2],1)
  
  if(!missing(run))
    .Object@run <- as.character(run) 
  
  # check data
  if(any(data$harvest<0) | any(is.na(data$harvest))) 
    stop('missing catch (harvest) data is not allowed\n')
  .Object@data <- lapply(data,function(x) x)
    
  # set initialisation function
  if(.Object@default_model) {
    
    # default initialisation function
    init.func <- function() { 
      
      b    <- init.r/exp(init.logK)
      r    <- init.r * rlnorm(1,log(1)-0.04/2,0.2)
      logK <- max(min(log(r/b),30),3)
      
      x    <- getx(data, r, logK)
      
      init.values <- list(logK   = logK,
                          r      = r,
                          x      = x)
      
      init.values
    }     
    
    if(missing(init)) {
      
      init.r    <- getr(.Object)
      init.logK <- getlogK(data, r = init.r)
  	
    } else {
      if(is.list(init)) {
          if(!is.null(init$r))    init.r    <- init$r    else init.r    <- getr(.Object)
    	  if(!is.null(init$logK)) init.logK <- init$logK else init.logK <- getlogK(data, r = init.r)
      }
      if(is.function(init)) {
        init.func <- init
      }
    }
  } else {
    # non-default settings
    if(missing(init))
      stop('must supply initialisation function for non-default model\n')
  	if(is.function(init)) {
  		init.func <- init
  	} else stop('initialisation function must be supplied\n')
  }
    
  # update .Object
  .Object@init.func <- init.func
  
  # sampling dimensions
  if(!missing(chains))    .Object@chains <- chains
  if(!missing(iter))      .Object@iter   <- iter
  if(!missing(thin))      .Object@thin   <- thin
  if(!missing(warmup))    .Object@warmup <- warmup else .Object@warmup <- floor(.Object@iter/2/.Object@thin)
  
  # number of posterior samples
  .Object@nsamples <- ((.Object@iter - .Object@warmup) * .Object@chains)/.Object@thin

  if(method=='MCMC') {
    
    # initiate mcmc-sampling
    cat('MCMC sampling\n')
    stanfit_object <- suppressWarnings(sampling(.Object,data=.Object@data,init=.Object@init.func,iter=.Object@iter,chains=.Object@chains,warmup=.Object@warmup,thin=.Object@thin, ...))
    
    # extract traces
    .Object@trace_array <- extract(stanfit_object,permuted=FALSE,inc_warmup=TRUE)
    .Object@trace       <- extract(stanfit_object)
    
    # record initial values for each chain
    .Object@init.values <- stanfit_object@inits
  }
  if(method=='MPD') {
    
    # initiate mpd fit
    cat('MPD optimisation\n')
    .Object@mpd <- optimizing(.Object,data=.Object@data,init=.Object@init.func, ...)
  }
  
  .Object
})
#}}
#{{ fit log-normal distribution to monte-carlo r values
setMethod("fit",signature=c("rprior","missing"),function(.Object, ...) { 
  
  x <- .Object@.Data
  if(!length(x)>2) stop('need >2 r values')
  
  # transform to normal
  y <- log(x)
  
  # estimate parameters of
  # normal distribution log(x)
  mu    <- mean(y)
  sigma <- sd(y)
  sigma2 <- sigma^2
  
  # estimate parameters of
  # log-normal distribution
  theta <- exp(mu + sigma2/2)
  nu <- exp(2*mu + sigma2)*(exp(sigma2) - 1)
  cv <- sqrt(exp(sigma2) - 1)
  
  # assign and return
  .Object@lognormal.par <- list('E[log(x)]'=mu,'SD[log(x)]'=sigma,'E[x]'=theta,'VAR[x]'=nu,'CV[x]'=cv)
  return(.Object)
  
})
#}
#}}
#}}}


