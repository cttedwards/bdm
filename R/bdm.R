
#{{{
setClass("bdm",contains="stanmodel",slots=list(data="list",init.func="function",chains="numeric",iter="numeric",warmup="numeric",thin="numeric",fit="stanfit",trace="list"))
setMethod("initialize","bdm",function(.Object,model.code,compile=FALSE) {
  
  if(!missing(model.code)) {
    .Object@model_code <- model.code
    .Object@model_name <- 'BDM'
    if(compile) {
      tmp <- stan_model(model_code=.Object@model_code)
      
      .Object@model_name <- 'BDM'
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
bdm <- function(compile=FALSE) {
  
  bdm_code_statespace <- '
    data {
      int T;
      int N;
      real index[T,N];
      real c[T];
    }
    parameters {
      real logK;
      real<lower=0,upper=2> r0;
      real<lower=0,upper=10> sigRsq;
      real<lower=0,upper=10> sigQsq;
      real<lower=0,upper=2> x[T];
    }
    transformed parameters {

      real q[N];
      real sigR;
      real sigQ;
      
      real err;
      real p;
      
      sigR <- sqrt(sigRsq);
      sigQ <- sqrt(sigQsq);
      
      // compute catchability
      for(n in 1:N){
        err <- 0.0;
        p <- 0.0;
        for(t in 1:T){
          if(index[t,n]>0.0) {
            err <- err + log(index[t,n]/x[t]);
            p <- p + 1.0;
          }
        }
        q[n] <- exp(err/p);
      }
    } 
    model {
      
      // prior densities for
      // estimated parameters
      logK ~ uniform(1.0,20.0);
      r0 ~ lognormal(log(0.3),0.2);
      
      // conjugate prior densities for
      // hyper-parameters
      sigRsq ~ inv_gamma(1.0,0.5);     
      sigQsq ~ inv_gamma(1.0,0.25);    
      
      // state equations
      x[1] ~ lognormal(log(1),sigQ);
      for(t in 2:T){
        real H;
        real mu;
        H <- fmin(exp(log(c[t-1]) - logK),x[t-1]);
        mu <- fmax(x[t-1] + r0 * x[t-1] * (1 - x[t-1]) - H,0.001);
        x[t] ~ lognormal(log(mu),sigQ);
      }
      
      // observation equation
      for(n in 1:N){
        for(t in 1:T){
          if(index[t,n]>0.0)
            index[t,n] ~ lognormal(log(q[n]*x[t]),sigR);
          }
      }
    }
    generated quantities {

      real harvest_rate[T];
      real surplus_production[T];

      real current_depletion;
      real current_harvest_rate;
      real current_surplus_production;

      real predicted_index[T,N];
      
      for(t in 1:T) {
        harvest_rate[t] <- c[t]/exp(log(x[t]) + logK);
        surplus_production[t] <- fmax(r0 * x[t] * (1 - x[t]),0.0);
      }

      current_depletion <- x[T];
      current_harvest_rate <- harvest_rate[T];
      current_surplus_production <- surplus_production[T];

      for(n in 1:N){
        for(t in 1:T){
          predicted_index[t,n] <- q[n]*x[t];
        }
      }

    }'

    new('bdm',bdm_code_statespace,compile)
}
#}}}
