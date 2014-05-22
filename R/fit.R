
#{{{ fit functions
setGeneric("fit", function(.Object,data, ...) standardGeneric("fit"))
#{{ fit bdm model to data
setMethod("fit",signature=c("bdm","edat"),function(.Object,data,init,chains,iter,warmup,thin,method="MCMC", ...) {
  
  if(missing(data)) 
    stop('No data object supplied\n')
  
  # number of data indices
  nix <- ifelse(length(dim(data$index))>1,dim(data$index)[2],1)
  
  # check data
  if(any(data$catch<0) | any(is.na(data$catch))) 
    stop('missing catch data is not allowed\n')
  .Object@data <- list(T=data$T,I=data$I,index=data$index,c=data$catch,sigmaO=data$sigmaO,sigmaP=data$sigmaP)
  
  # set initialisation function
  if(.Object@default_model) {
    
    # default initialisation function
    init.func <- function() { 
      init.values <- list(logK   = init.logK,
                          r      = init.r * rlnorm(1,log(1)-0.04/2,0.2),
                          xdev   = rep(1,data$T)) #rbeta(data$T,20,1))
      init.values
    }     
    
    if(missing(init)) {
      
      init.r    <- .getr(.Object)
      init.logK <- .getlogK(.Object)
  	
    } else {
      if(is.list(init)) {
    		if(!is.null(init$logK)) init.logK <- init$logK else init.logK <- .getlogK(.Object)
    		if(!is.null(init$r))    init.r    <- init$r    else init.r    <- .getr(.Object)
      }
    }
  } else {
    # non-default settings
  	if(is.function(init)) {
  		init.func <- init
  	} else stop('initialisation function must be supplied\n')
  }
    
  # update .Object
  .Object@init.func <- init.func
  
  if(!missing(chains))    .Object@chains <- chains
  if(!missing(iter))      .Object@iter   <- iter
  if(!missing(warmup))    .Object@warmup <- warmup else .Object@warmup <- floor(.Object@iter/2)
  if(!missing(thin))      .Object@thin   <- thin

  if(method=='MCMC') {
    
    # initiate mcmc-sampling
    cat('MCMC sampling\n')
    .Object@fit <- sampling(.Object,data=.Object@data,init=.Object@init.func,iter=.Object@iter,chains=.Object@chains,warmup=.Object@warmup,thin=.Object@thin, ...)
    
    # extract traces
    .Object@trace <- extract(.Object@fit)
  }
  if(method=='MPD') {
    
    # initiate mpd fit
    cat('MPD optimisation\n')
    .Object@mpd <- optimizing(.Object,data=.Object@data,init=.Object@init.func, ...)
  }
  
  .Object
})
#{ initial value functions for r and logK
.getr <- function(.Object) {
  
  # extract r from model_code
  
  m <- regexpr('r.?~.?lognormal\\(.+?\\)',.Object@model_code)
  x <- regmatches(.Object@model_code,m)
  
  m1 <- regexpr('\\(.+?\\,',x)
  m1 <- m1 + 1
  attributes(m1)$match.length <- attributes(m1)$match.length - 2
  x1 <- regmatches(x,m1)
  
  m2 <- regexpr('\\,.+?\\)',x)
  m2 <- m2 + 1
  attributes(m2)$match.length <- attributes(m2)$match.length - 2
  x2 <- regmatches(x,m2)
  
  mu <- as.numeric(x1)
  sigma <- as.numeric(x2)
  
  init.r <- exp(mu+sigma^2/2)
  
  init.r
}
.getlogK <- function(.Object) {
  
  # get logK through grid search
  
  rr <- .getr(.Object)
  
  ii <- .Object@data$index
  cc <- .Object@data$c
  tt <- length(cc)
  bm <- numeric(tt)
  
  obj <- function(logK) {
  
    bm[1] <- 1
    for(t in 1:tt) 
      bm[t+1] <- max(bm[t] + rr*bm[t]*(1 - bm[t]) - cc[t]/exp(logK),1e-3)
    bm <- bm[-length(bm)]
  
    q <- mean(apply(ii,2,function(x) exp(mean(log(x[x>0]/bm[x>0])))))
  
    -sum(apply(ii,2,function(x) sum(log(x[x>0]/(q*bm[x>0]))^2))) + log(bm[tt]/0.9) + sum(cc/(bm*exp(logK)) > 0.95)
  }
  
  np <- 100
  logK.llk <- numeric(np)
  logK.seq <- seq(3,30,length=np)
  for(i in 1:np) 
    logK.llk[i] <- obj(logK.seq[i])
  
  init.logK <- logK.seq[which.min(logK.llk)]
  
  init.logK
}
#}
#}}
#{{ fit log-normal distribution to monte-carlo r values
setMethod("fit",signature=c("rprior","missing"),function(.Object,plot=TRUE, ...) { 
  
  .Object <- .fitr(.Object)
  
  logmu    <- .Object@lognormal.par[['E[log(x)]']]
  logsigma <- .Object@lognormal.par[['SD[log(x)]']]
  
  if(plot) {
  	windows()
  	hist(.Object@.Data,freq=FALSE,xlab='r',ylab='',yaxt='n',main='')
  	curve(dlnorm(x,logmu,logsigma),col=2,add=T)
  }
  
  .Object
})
#{ fitting function
.fitr <- function(.Object) {
  
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
  
}
#}
#}}
#}}}


