## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.path = 'fig/bdm-newmodel-', fig.width=  6, tidy = FALSE, message = FALSE, warning = FALSE, collapse = TRUE, comment = "#>")

## ---- results='hide'-----------------------------------------------------
library(bdm)

## ----haknz-data, results='hide', fig.cap='Chatham rise hake data'--------
data(haknz)
dat <- bdmData(harvest = haknz$landings,index = cbind(haknz$survey, haknz$cpue), time = haknz$year, renormalise = TRUE)
plot(dat)

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#    new_model <- "
#      ...
#      parameters {
#        real<lower=3,upper=30> logK;
#        real<lower=0,upper=2> r;
#        real<lower=0> x[T];
#        real<lower=0> sigmap2;
#      }
#      ...
#      model {
#  
#        // prior densities for
#        // estimated parameters
#        // ********************
#        logK ~ uniform(3.0,30.0);
#        r ~ lognormal(-1.0,0.20);
#        sigmap2 ~ inv_gamma(0.001,0.001);
#  
#        ...
#      }
#      ...
#    "

## ---- eval=TRUE, echo=FALSE----------------------------------------------
  new_model <- "
    data {
      int T;
      int I;
      real index[T,I];
      real harvest[T];
      real n;
      real sigmao[T,I];
      real sigmap;
    }
    parameters {
      real<lower=3,upper=30> logK;
      real<lower=0,upper=2> r;
      real<lower=0> x[T];
      real<lower=0> sigmap2; 
    }
    transformed parameters {

      real q[I];

      // variance terms
      real sigmao2[T,I];

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
      for(t in 1:T)
        for(i in 1:I)
            sigmao2[t,i] <- square(sigmao[t,i]);
      
      // compute mpd catchability assuming 
      // variable sigmao over time assuming
      // uniform prior on q
      {
        real sum1;
        real sum2;
        real p;
        for(i in 1:I){
          sum1 <- 0.0;
          sum2 <- 0.0;
          p <- 0.0;
          for(t in 1:T){
            if(index[t,i]>0.0 && x[t]>0.0) {
              sum1 <- sum1 + log(index[t,i]/x[t])/sigmao2[t,i];
              sum2 <- sum2 + 1/sigmao2[t,i];
              p <- p + 1.0;
            }
          }
          if(p>2.0) { q[i] <- exp((0.5 * p + sum1) / sum2);
            } else q[i] <- 0.0;
        }
      }
    } 
    model {
      
      // prior densities for
      // estimated parameters
      // ********************
      logK ~ uniform(3.0,30.0);
      r ~ lognormal(-1.0,0.20);
      sigmap2 ~ inv_gamma(0.001,0.001);
      
      // state equation
      // **************
      {
        real H;
        real mu;
        x[1] ~ lognormal(log(1.0)-sigmap2/2, sqrt(sigmap2));
        for(t in 2:T) {
          H <- fmin(exp(log(harvest[t-1]) - logK),x[t-1]);
          if(x[t-1]<=dmsy) mu <- x[t-1] + r * x[t-1] * (1 - x[t-1]/h) - H;
          if(x[t-1]> dmsy) mu <- x[t-1] + g * m * x[t-1] * (1 - pow(x[t-1],(n-1))) - H;
          if(mu > 0.0) mu <- log(mu) - sigmap2/2;
          else mu <- log(0.01) - sigmap2/2;
          x[t] ~ lognormal(mu, sqrt(sigmap2));
        }
      }
      
      // observation equation
      // ********************
      {
        real mu;
        for(i in 1:I){
          for(t in 1:T){
            if(index[t,i]>0.0 && x[t]>0.0 && q[i]>0.0) {
              mu <- log(q[i]*x[t]) - sigmao2[t,i]/2;
              index[t,i] ~ lognormal(mu,sigmao[t,i]);
            }
          }
        }
      }

      // apply penalty for H>0.95
      // ************************
      for(t in 1:T){
      real H_; H_ <- harvest[t]/exp(log(x[t]) + logK);
        if(H_>0.95) {
          increment_log_prob(-log(H_/0.95) * (1/sigmap2));
        }
      }
    }
    generated quantities {

      real biomass[T];
      real depletion[T];
      real harvest_rate[T];
      real surplus_production[T];
      
      real epsilon_o[T,I];
      real epsilon_p[T];

      real current_biomass;
      real current_depletion;
      real current_harvest_rate;

      real msy;
      real biomass_at_msy;
      real harvest_rate_at_msy;

      real observed_index[T,I];
      real predicted_index[T,I];
      
      {
        real H;
        for(t in 2:T) {
          H <- fmin(exp(log(harvest[t-1]) - logK),x[t-1]);
          if(x[t-1]<=dmsy) epsilon_p[t-1] <- x[t]/(x[t-1] + r * x[t-1] * (1 - x[t-1]/h) - H);
          if(x[t-1]> dmsy) epsilon_p[t-1] <- x[t]/(x[t-1] + g * m * x[t-1] * (1 - pow(x[t-1],(n-1))) - H);
        }
        epsilon_p[T] <- lognormal_rng(log(1.0)-sigmap2/2, sqrt(sigmap2));
      }
      
      for(t in 1:T) {
        biomass[t] <- x[t] * exp(logK);
        depletion[t] <- x[t];
        harvest_rate[t] <- harvest[t]/exp(log(x[t]) + logK);
        if(x[t]<=dmsy) surplus_production[t] <- r * x[t] * (1 - x[t]/h) * epsilon_p[t];
        if(x[t]> dmsy) surplus_production[t] <- g * m * x[t] * (1 - pow(x[t],(n-1))) * epsilon_p[t];
      }

      current_biomass <- biomass[T];
      current_depletion <- x[T];
      current_harvest_rate <- harvest_rate[T];

      msy <- m * exp(logK);
      biomass_at_msy <- dmsy * exp(logK);
      harvest_rate_at_msy <- m / dmsy;

      for(i in 1:I){
        for(t in 1:T){
          observed_index[t,i] <- index[t,i];
          predicted_index[t,i] <- q[i]*x[t];
        }
      }

      for(t in 1:T){
        for(i in 1:I){
          epsilon_o[t,i] <- observed_index[t,i]/predicted_index[t,i];
        }
      }
    }
  "

## ---- results='hide'-----------------------------------------------------
mdl <- bdm(model.code = new_model)
mdl <- updatePrior(mdl, prior = list(par = 'r', meanlog = -0.91, sdlog = 0.40))
mdl <- compiler(mdl)

