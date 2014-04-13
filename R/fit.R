
#{{{ fit bdm model to data
setGeneric("fit", function(.Object,data, ...) standardGeneric("fit"))
setMethod("fit",signature=c("bdm","edat"),function(.Object,data,init,chains,iter,warmup,thin, ...) {
  
  require(DEoptim)
  require(pscl)
  
  if(missing(data)) stop('No data object supplied\n')
  
  # number of data indices
  nix <- ifelse(length(dim(data$index))>1,dim(data$index)[2],1)
  
  # renormalise indices to geometric mean
  renorm <- function(x) { y<-x[x>0]; x[x>0] <- y/(prod(y)^(1/length(y))); x }
  if(nix>1) {
    data$index <- apply(data$index,2,renorm)
    cat('Re-normalised indices.\n')
  } else {
    data$index <- renorm(data$index)
    cat('Re-normalised index.\n')
  }
  
  # initial value functions
  # for r0 and logK
  get.r0 <- function() {
    
    # extract r0 from model_code
    m <- regexpr('r0.?~.?lognormal\\(log\\(.+?\\)',.Object@model_code)
    if(m>-1) {
      x <- regmatches(.Object@model_code,m)
      m <- regexpr('log\\(.+?\\)',x)
      m <- m + 4
      attributes(m)$match.length <- attributes(m)$match.length - 5
      x <- regmatches(x,m)
      init.r0 <- as.numeric(x)
    } else {
      m <- regexpr('r0.?~.?lognormal\\(.+?\\)',.Object@model_code)
      x <- regmatches(.Object@model_code,m)
      m <- regexpr('\\(.+?\\,',x)
      m <- m + 1
      attributes(m)$match.length <- attributes(m)$match.length - 2
      x <- regmatches(x,m)
      init.r0 <- exp(as.numeric(x))
    }
    
    init.r0
  }
  get.logK <- function() {
    
    ii <- data$index
    cc <- data$catch
    bm <- numeric(length(cc))
    
    init.r0 <- get.r0()
    
    bm[1] <- 1
    
    # estimate approximate logK numerically
    # using DEoptim
    obj <- function(logK) {
    
      for(t in 1:length(cc)) 
        bm[t+1] <- max(bm[t] + init.r0*bm[t]*(1 - bm[t]) - cc[t]/exp(logK),1e-3)
      bm <- bm[-length(bm)]
    
      q <- mean(apply(ii,2,function(x) exp(mean(log(x[x>0]/bm[x>0])))))
    
      -sum(apply(ii,2,function(x) sum(log(x[x>0]/(q*bm[x>0]))^2)))
    }
    
    init.logK <- as.numeric(DEoptim(obj,lower=3,upper=30,control = DEoptim.control(trace = FALSE))$optim$bestmem)
    init.logK
  }
  
  # specify initial values
  if(missing(init)) {
    
    init.r0   <- get.r0()
    init.logK <- get.logK()
        
  } else {
    
   if(!is.null(init$logK)) init.logK <- init$logK else init.logK <- get.logK()
   if(!is.null(init$r0))   init.r0   <- init$r0   else init.r0   <- get.r0()
   
  }
  
  # initial value function for fit()
  init.func <- function() { 
    
    init.values <- list(logK   = init.logK * rlnorm(1,0,0.1),
                        r0     = init.r0 * rlnorm(1,0,0.1),
                        sigRsq = rigamma(1,10,1),
                        sigQsq = rigamma(1,10,1),
                        x      = rbeta(data$T,5,3))
    
	  cat(round(unlist(init.values)[1:4],2),'\n')
    
    init.values
  }
  
  # check catch data
  if(any(data$catch<0) | any(is.na(data$catch))) stop('missing catch data is not allowed\n')
  
  # update .Object
  .Object@data <- list(T=data$T,N=data$N,index=data$index,c=data$catch)
  .Object@init.func <- init.func
  
  if(!missing(chains))    .Object@chains <- chains
  if(!missing(iter))      .Object@iter   <- iter
  if(!missing(warmup))    .Object@warmup <- warmup else .Object@warmup <- floor(.Object@iter/2)
  if(!missing(thin))      .Object@thin   <- thin

  # initiate mc-sampling
  cat('Initial value set for each chain:\n')
  cat('logK  r  sigRsq  sigQsq \n')
  .Object@fit <- sampling(.Object,data=.Object@data,init=.Object@init.func,iter=.Object@iter,chains=.Object@chains,warmup=.Object@warmup,thin=.Object@thin, ...)
  
  # extract traces
  .Object@trace <- extract(.Object@fit)
  #.Object@trace$logq  <- lapply(apply(.Object@trace$logq,2,function(x) list(x)),unlist)  
  
  .Object
})
#}}}

setMethod("fit",signature=c("rprior","missing"),function(.Object, ...) { .fitr(.Object) })


