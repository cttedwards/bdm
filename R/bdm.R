#' @rdname bdm-class
#' 
#' @param path path to stan code
#' @param model.code model code as a character string
#' @param model.name optional model name
#' 
#' @examples
#' # initialise default model object
#' mdl <- bdm()
#' 
#' # inspect Stan model code
#' \dontshow{
#' mdl
#' }
#' 
#' @include bdm-class.R
#' 
#' @export
bdm <- function(path, model.code, model.name = '') {
    if (!missing(path)) { 
        model.name <- ifelse(model.name == '', sub("\\.[^.]*$", "", basename(path)), model.name)
        new('bdm', path = path, model.name = model.name)
    } else { 
        if (!missing(model.code)) { 
            model.name <- ifelse(model.name == '', deparse(substitute(model.code)), model.name)
            if (substr(model.name, start = 0, stop = 1) == "\"") 
                stop('please supply a model.name\n')
            new('bdm', model.code = model.code, model.name = model.name)
        } else {
            model.name <- ifelse(model.name == '', 'default', model.name)
            new('bdm', model.code = bdm_default, model.name = model.name)
        }
    }
}
#{{{
# default model
bdm_default <- '
data {
    int T;
    int I;
    real index[T,I];
    real harvest[T];
    real n;
    real sigmao[T,I];
    //real sigmap;
}
parameters {
    real<lower=1> logK;
    real<lower=0> r;
    real<lower=0> x[T];
    real<lower=0> sigmap;
}
transformed parameters {

    real q[I];
    
    // variance terms
    real sigmao2[T,I];
    real sigmap2;
    
    // fletcher-schaefer
    // parameters
    real dmsy;
    real h;
    real m;
    real g;
    
    dmsy = pow((1/n),(1/(n-1)));
    h = 2*dmsy;
    m = r*h/4;
    g = pow(n,(n/(n-1)))/(n-1);
    
    // variance terms
    for(t in 1:T)
        for(i in 1:I)
            sigmao2[t,i] = square(sigmao[t,i]);
    sigmap2 = square(sigmap);
    
    // compute mpd catchability assuming 
    // variable sigmao over time assuming
    // uniform prior on q
    {
        real sum1;
        real sum2;
        real p;
        for(i in 1:I){
            sum1 = 0.0;
            sum2 = 0.0;
            p = 0.0;
            for(t in 1:T){
                if(index[t,i]>0.0 && x[t]>0.0) {
                    sum1 = sum1 + log(index[t,i]/x[t])/sigmao2[t,i];
                    sum2 = sum2 + 1/sigmao2[t,i];
                    p = p + 1.0;
                }
            }
            if(p>2.0) { q[i] = exp((0.5 * p + sum1) / sum2);
            } else q[i] = 0.0;
        }
    }
} 
model {
    
    // prior densities for
    // estimated parameters
    // ********************
    logK ~ uniform(1.0,12.0);
    r ~ lognormal(-1.0,0.20);
    sigmap ~ exponential(10.0);
    
    // state equation
    // **************
    {
        real H;
        real mu;
        x[1] ~ lognormal(log(1.0)-sigmap2/2,sigmap);
        for(t in 2:T) {
            H = fmin(exp(log(harvest[t-1]) - logK),x[t-1]);
            if(x[t-1]<=dmsy) mu = x[t-1] + r * x[t-1] * (1 - x[t-1]/h) - H;
            if(x[t-1]> dmsy) mu = x[t-1] + g * m * x[t-1] * (1 - pow(x[t-1],(n-1))) - H;
            if(mu > 0.0) mu = log(mu) - sigmap2/2;
            else mu = log(0.01) - sigmap2/2;
            x[t] ~ lognormal(mu,sigmap);
        }
    }
    
    // observation equation
    // ********************
    {
        real mu;
        for(i in 1:I){
            for(t in 1:T){
                if(index[t,i]>0.0 && x[t]>0.0 && q[i]>0.0) {
                    mu = log(q[i]*x[t]) - sigmao2[t,i]/2;
                    index[t,i] ~ lognormal(mu,sigmao[t,i]);
                }
            }
        }
    }
    
    // apply penalty for H>0.95
    // ************************
    for(t in 1:T){  
        real H_; H_ = harvest[t]/exp(log(x[t]) + logK);
        if(H_>0.95) {
            target += -log(H_/0.95) * (1/sigmap2);
        }
    }
}
generated quantities {

    real rPrior;
    real logKprior;
    real sigmapPrior;
    
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
    real depletion_at_msy;
    real biomass_at_msy;
    real harvest_rate_at_msy;
    
    real current_biomass_over_bmsy;
    real current_depletion_over_dmsy;
    real current_harvest_rate_over_hmsy;
    
    real observed_index[T,I];
    real predicted_index[T,I];
    
    {
        real H;
        for(t in 2:T) {
            H = fmin(exp(log(harvest[t-1]) - logK),x[t-1]);
            if(x[t-1]<=dmsy) epsilon_p[t-1] = x[t]/(x[t-1] + r * x[t-1] * (1 - x[t-1]/h) - H);
            if(x[t-1]> dmsy) epsilon_p[t-1] = x[t]/(x[t-1] + g * m * x[t-1] * (1 - pow(x[t-1],(n-1))) - H);
        }
        epsilon_p[T] = lognormal_rng(log(1.0)-sigmap2/2,sigmap);
    }
    
    for(t in 1:T) {
        biomass[t] = x[t] * exp(logK);
        depletion[t] = x[t];
        harvest_rate[t] = harvest[t]/exp(log(x[t]) + logK);
        if(x[t]<=dmsy) surplus_production[t] = r * x[t] * (1 - x[t]/h) * epsilon_p[t];
        if(x[t]> dmsy) surplus_production[t] = g * m * x[t] * (1 - pow(x[t],(n-1))) * epsilon_p[t];
    }
    
    current_biomass = biomass[T];
    current_depletion = x[T];
    current_harvest_rate = harvest_rate[T];
    
    msy = m * exp(logK);
    depletion_at_msy = dmsy;
    biomass_at_msy = dmsy * exp(logK);
    harvest_rate_at_msy = m / dmsy;
    
    current_biomass_over_bmsy = current_biomass / biomass_at_msy;
    current_depletion_over_dmsy = current_depletion / depletion_at_msy;
    current_harvest_rate_over_hmsy = current_harvest_rate / harvest_rate_at_msy;
    
    for(i in 1:I){
        for(t in 1:T){
            observed_index[t,i] = index[t,i];
            predicted_index[t,i] = q[i]*x[t];
        }
    }
    
    for(t in 1:T){
        for(i in 1:I){
            epsilon_o[t,i] = observed_index[t,i]/predicted_index[t,i];
        }
    }
    
    logKprior = uniform_rng(1.0,12.0);
    rPrior = lognormal_rng(-1.0,0.20);
    sigmapPrior = exponential_rng(10.0);
}
'
#}}}
