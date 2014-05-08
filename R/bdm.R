
#{{{
setClass("bdm",contains="stanmodel",slots=list(data="list",init.func="function",chains="numeric",iter="numeric",warmup="numeric",thin="numeric",fit="stanfit",trace="list",path="character"))
setMethod("initialize","bdm",function(.Object,path,model.code,compile) {
  
  require(rstan)
  
  if(!missing(path)) {
    .Object@path <- path
    .Object@model_name <- 'BDM'
    if(compile) {
      tmp <- stan_model(file=.Object@path)
      
      .Object@model_code <- tmp@model_code
      .Object@model_cpp  <- tmp@model_cpp
      .Object@dso        <- tmp@dso
    }
  }
  if(!missing(model.code)) {
    .Object@model_code <- model.code
    .Object@model_name <- 'BDM'
    if(compile) {
      tmp <- stan_model(model_code=.Object@model_code)
      
      .Object@path       <- 'local declaration'
      .Object@model_cpp  <- tmp@model_cpp
      .Object@dso        <- tmp@dso
    }
  }
  
  .Object@chains <- 4
  .Object@iter   <- 2000
  .Object@warmup <- floor(.Object@iter/2)
  .Object@thin   <- 1
  
  .Object
  
})
# constructor
bdm <- function(path,model.code,compile=FALSE) {
  
  bdm_code <- '
    data {
      int T;
      int I;
      real index[T,I];
      real c[T];
      real sigmaO[I];
      real sigmaP;
    }
    parameters {
      real<lower=3,upper=30> logK;
      real<lower=0,upper=2> r0;
      real<lower=0> xdev[T];
    }
    transformed parameters {

      real x[T];
      real q[I];
      real H[T];
      
      real err;
      real p;
	  
      // compute biomass dynamics
      x[1] <- 1.0;
      H[1] <- fmin(exp(log(c[1]) - logK),0.99);
      for(t in 2:T){
        x[t] <- (x[t-1] + r0 * x[t-1] * (1 - x[t-1]) - H[t-1]) * xdev[t-1];
	      H[t] <- fmin(exp(log(c[t]) - logK),x[t]);
      }
      
      // compute catchability
      for(i in 1:I){
        err <- 0.0;
        p <- 0.0;
        for(t in 1:T){
          if(index[t,i]>0.0 && x[t]>0.0) {
            err <- err + log(index[t,i]/x[t]);
            p <- p + 1.0;
          }
        }
        if(p>0.0) { q[i] <- exp(err/p);
	      } else q[i] <- 0.0;
      }
    } 
    model {
      
      // prior densities for
      // estimated parameters
      // ********************
      logK ~ uniform(3.0,30.0);
      r0 ~ lognormal(-1.0,0.40);
      
      // random deviations
      // *****************
      xdev ~ lognormal(0,sigmaP);
      
      // observation equation
      // ********************
      for(i in 1:I){
        for(t in 1:T){
          if(index[t,i]>0.0 && x[t]>0.0 && q[i]>0.0)
            index[t,i] ~ lognormal(log(q[i]*x[t]),sigmaO[i]);
          }
      }

      // apply penalty for Hrel>0.95
      // ***************************
      for(t in 1:T){
      real Hrel; Hrel <- H[t]/x[t];
        if(Hrel>0.95) {
          lp__ <- lp__ - log(Hrel/0.95) * (1/sigmaP);
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

      real observed_index_biomass[T,I];
      real observed_index_depletion[T,I];
      real predicted_index[T,I];
      
      for(t in 1:T) {
	    biomass[t] <- x[t] * exp(logK);
	    depletion[t] <- x[t];
        harvest_rate[t] <- c[t]/exp(log(x[t]) + logK);
        surplus_production[t] <- r0 * x[t] * (1 - x[t]);
      }

      current_biomass <- biomass[T];
      current_depletion <- x[T];
      current_harvest_rate <- harvest_rate[T];

      for(i in 1:I){
        for(t in 1:T){
          if(index[t,i]>0.0) {
            observed_index_biomass[t,i]   <- (index[t,i]/q[i]) * exp(logK);
            observed_index_depletion[t,i] <- index[t,i]/q[i];
          }
          predicted_index[t,i] <- q[i]*x[t];
        }
      }
    }'

  if(!missing(path)) { new('bdm',path=path,compile=compile)
	} else { if(!missing(model.code)) { new('bdm',model.code=model.code,compile=compile)
	} else new('bdm',model.code=bdm_code,compile=compile)
	}
}
#}}}
