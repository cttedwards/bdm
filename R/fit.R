
#{{{ fit functions
setGeneric("fit", function(.Object,data, ...) standardGeneric("fit"))
#{{ fit bdm model to data
setMethod("fit",signature=c("bdm","edat"),function(.Object,data,init,chains,iter,warmup,thin, ...) {
  
  require(pscl)
  
  if(missing(data)) stop('No data object supplied\n')
  
  # number of data indices
  nix <- ifelse(length(dim(data$index))>1,dim(data$index)[2],1)
  
  # check data
  if(any(data$catch<0) | any(is.na(data$catch))) stop('missing catch data is not allowed\n')
  .Object@data <- list(T=data$T,I=data$I,index=data$index,c=data$catch,sigmaO=data$sigmaO,sigmaP=data$sigmaP)
    
  # default initialisation function
  init.func.default <- function() { 
  	init.values <- list(logK   = init.logK + rbeta(1,1,2),
  			                r0     = init.r0 * rlnorm(1,0,0.2),
  			                xdev   = rbeta(data$T,5,3))
  	cat(round(unlist(init.values)[1:2],2),'\n')
	  init.values
  }     

  # specify initial values for 
  # initialisation function
  if(missing(init)) {
    
    init.r0   <- .getr0(.Object)
    init.logK <- .getlogK(.Object)
    init.func <- init.func.default
	
  } else {
  	if(is.list(init)) {
  		if(!is.null(init$logK)) init.logK <- init$logK else init.logK <- .getlogK(.Object)
  		if(!is.null(init$r0))   init.r0   <- init$r0   else init.r0   <- .getr0(.Object)
  		init.func <- init.func.default
  	}
  	if(is.function(init)) {
  		init.func <- init
  	}
  }
    
  # update .Object
  .Object@init.func <- init.func
  
  if(!missing(chains))    .Object@chains <- chains
  if(!missing(iter))      .Object@iter   <- iter
  if(!missing(warmup))    .Object@warmup <- warmup else .Object@warmup <- floor(.Object@iter/2)
  if(!missing(thin))      .Object@thin   <- thin

  # initiate mc-sampling
  cat('Initial values for each chain:\n')
  cat('logK  r\n')
  .Object@fit <- sampling(.Object,data=.Object@data,init=.Object@init.func,iter=.Object@iter,chains=.Object@chains,warmup=.Object@warmup,thin=.Object@thin, ...)
  cat('Completed MCMC sampling\n')
  
  # extract traces
  .Object@trace <- extract(.Object@fit)
  #.Object@trace$logq  <- lapply(apply(.Object@trace$logq,2,function(x) list(x)),unlist)  
  
  .Object
})
# initial value functions
# for r0 and logK
.getr0 <- function(.Object) {
  
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
.getlogK <- function(.Object) {
  
  # get logK from MPD fit
  init.r0   <- .getr0(.Object)
  init.logK <- optimizing(.Object,data=.Object@data,init=list(r0=init.r0,logK=25,xdev=rep(1,.Object@data$T)))$par['logK']
  cat('MPD estimate of logK:',round(init.logK,2),'\n\n')
  
  #require(DEoptim)
  #ii <- data$index
  #cc <- data$catch
  #bm <- numeric(length(cc))
  #
  #bm[1] <- 1
  #
  # estimate approximate logK numerically
  # using DEoptim
  #obj <- function(logK) {
  #
  #  for(t in 1:length(cc)) 
  #    bm[t+1] <- max(bm[t] + init.r0*bm[t]*(1 - bm[t]) - cc[t]/exp(logK),1e-3)
  #  bm <- bm[-length(bm)]
  #
  #  q <- mean(apply(ii,2,function(x) exp(mean(log(x[x>0]/bm[x>0])))))
  #
  #  -sum(apply(ii,2,function(x) sum(log(x[x>0]/(q*bm[x>0]))^2)))
  #}
  #
  #init.logK <- as.numeric(DEoptim(obj,lower=3,upper=30,control = DEoptim.control(trace = FALSE))$optim$bestmem)
  
  init.logK
}
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
#}}
#}}}


