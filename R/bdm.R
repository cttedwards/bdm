
#{{{
# class definition
setClass("bdm",contains="stanmodel",
                slots=list(data="list",            # edat object or list
                           init.func="function",   # initialisation function
                           init.values="list",     # initial values populated by init.func
                           chains="numeric",       # number of MCMC chains
                           iter="numeric",         # number of MCMC iterations per chain
                           warmup="numeric",       # number of iterations under adaptive sampling
                           thin="numeric",         # interval between recorded samples
                           nsamples="numeric",     # total number of posterior samples recorded
                           trace_array="array",    # array of posterior samples including warmup
                           trace="list",           # list of posterior samples without warmup and with chains mixed
                           mpd="list",             # mpd output from rstan::optimizing()
                           path="character",       # optional path to stan model code file for initialisaton of non-default model
                           run="character",        # optional label for this particular run
                           default_model="logical" # is the default fletcher-schaefer hybrid model retained?
                           )
         )
# initialisation function
setMethod("initialize","bdm",function(.Object,path,model.code,model.name,compile,default_model) {
  
  require(rstan)
  
  if(missing(model.name)) 
    model.name <- 'BDM'
  
  if(!missing(path)) {
    .Object@path <- path
    .Object@model_name <- model.name
    if(compile) {
      tmp <- stan_model(file=.Object@path)
      
      .Object@model_code <- tmp@model_code
      .Object@model_cpp  <- tmp@model_cpp
      .Object@dso        <- tmp@dso
    }
  }
  if(!missing(model.code)) {
    .Object@model_code <- model.code
    .Object@model_name <- model.name
    .Object@path       <- ifelse(default_model,'default_model','local_declaration')
    if(compile) {
      tmp <- stan_model(model_code=.Object@model_code)
      
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
# constructor
bdm <- function(path,model.code,model.name='BDM',compile=FALSE) {
  
  bdm_code <- '
    data {
      int T;
      int I;
      real index[T,I];
      real harvest[T];
      real n;
      real sigmaO[I];
      real sigmaP;
    }
    parameters {
      real<lower=3,upper=30> logK;
      real<lower=0,upper=2> r;
      real<lower=0> xdev[T];
    }
    transformed parameters {

      real x[T];
      real q[I];
      real H[T];

      // variance terms
      real sigmaOsq[I];
      real sigmaPsq;

      // fletcher-schaefer
      // parameters
      real dmsy;
      real h;
      real m;
      real g;
      
      dmsy <- pow((1/n),(1/(n-1)));
      h <- 2*dmsy;
      m <- r*h/4;
      g <- pow(n,(n/(n-1)))/(n-1);

      // variance terms
      for(i in 1:I)
        sigmaOsq[i] <- square(sigmaO[i]);
      sigmaPsq <- square(sigmaP);
	  
      // compute biomass dynamics
      x[1] <- 1.0 * xdev[1];
      H[1] <- fmin(exp(log(harvest[1]) - logK),0.99);
      for(t in 2:T){
        if(x[t-1]<=dmsy) x[t] <- (x[t-1] + r * x[t-1] * (1 - x[t-1]/h) - H[t-1]) * xdev[t-1];
        if(x[t-1]> dmsy) x[t] <- (x[t-1] + g * m * x[t-1] * (1 - pow(x[t-1],(n-1))) - H[t-1]) * xdev[t-1];
	      H[t] <- fmin(exp(log(harvest[t]) - logK),x[t]);
      }
      
      // compute catchability assuming 
      // constant sigmaO over time
      // and uniform prior on ln(q)
      {
        real err;
        real p;
        for(i in 1:I){
          err <- 0.0;
          p <- 0.0;
          for(t in 1:T){
            if(index[t,i]>0.0 && x[t]>0.0) {
              err <- err + log(index[t,i]/x[t]);
              p <- p + 1.0;
            }
          }
          if(p>2.0) { q[i] <- exp(err/p); // exp(sigmaOsq[i] * (p-2)/(2*p) + err/p);
  	      } else q[i] <- 0.0;
        }
      }
    } 
    model {
      
      // prior densities for
      // estimated parameters
      // ********************
      logK ~ uniform(3.0,30.0);
      r ~ lognormal(-1.0,0.40);
      
      // random deviations
      // *****************
      xdev ~ lognormal(log(1.0)-sigmaPsq/2,sigmaP);
      
      // observation equation
      // ********************
      for(i in 1:I){
        for(t in 1:T){
          if(index[t,i]>0.0 && x[t]>0.0 && q[i]>0.0)
            index[t,i] ~ lognormal(log(q[i]*x[t])-sigmaOsq[i]/2,sigmaO[i]);
          }
      }

      // apply penalty for H>0.95
      // ***************************
      for(t in 1:T){
      real H_; H_ <- H[t]/x[t];
        if(H_>0.95) {
          increment_log_prob(-log(H_/0.95) * (1/sigmaPsq));
        }
      }
    }
    generated quantities {

      real biomass[T];
      real depletion[T];
      real harvest_rate[T];
      real surplus_production[T];

      real current_biomass;
      real current_depletion;
      real current_harvest_rate;

      real biomass_at_msy;
      real harvest_rate_at_msy;

      real observed_index_biomass[T,I];
      real observed_index_depletion[T,I];
      real predicted_index[T,I];
      
      for(t in 1:T) {
        biomass[t] <- x[t] * exp(logK);
        depletion[t] <- x[t];
        harvest_rate[t] <- harvest[t]/exp(log(x[t]) + logK);
        if(x[t]<=dmsy) surplus_production[t] <- r * x[t] * (1 - x[t]/h) * xdev[t];
        if(x[t]> dmsy) surplus_production[t] <- g * m * x[t] * (1 - pow(x[t],(n-1))) * xdev[t];
      }

      current_biomass <- biomass[T];
      current_depletion <- x[T];
      current_harvest_rate <- harvest_rate[T];

      biomass_at_msy <- dmsy * exp(logK);
      harvest_rate_at_msy <- m / dmsy;

      for(i in 1:I){
        for(t in 1:T){
          if(index[t,i]>0.0) {
            observed_index_biomass[t,i]   <- (index[t,i]/q[i]) * exp(logK);
            observed_index_depletion[t,i] <- index[t,i]/q[i];
          }
          predicted_index[t,i] <- q[i]*x[t];
        }
      }
    }
  '

  if(!missing(path)) { new('bdm',path=path,model.name=model.name,compile=compile,default_model=FALSE)
	} else { if(!missing(model.code)) { new('bdm',model.code=model.code,model.name=model.name,compile=compile,default_model=FALSE)
	} else new('bdm',model.code=bdm_code,model.name=model.name,compile=compile,default_model=TRUE)
	}
}
#}}}
