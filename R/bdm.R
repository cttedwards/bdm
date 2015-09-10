#' @rdname bdm-class
#' 
#' @param path path to stan code
#' @param model.code model code as a character string
#' @param model.name optional model name
#' 
#' @examples
#' library(bdm)
#' 
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
    }
    parameters {
        real<lower=3,upper=30> logK;
        real<lower=0,upper=2> r;
    }
    transformed parameters {
    
        real q[I];
        real sigmao[T,I];
        real sigmao2[T,I];
        real<lower=0,upper=2> x[T];
        
        // variance terms
        // ***************
        for(t in 1:T) {
            for(i in 1:I) {
                sigmao[t,i]  <- 0.2;
                sigmao2[t,i] <- square(sigmao[t,i]);
            }
        }
        
        // compute mpd catchabilities assuming 
        // variable sigmao over time and
        // uniform prior
        // ************************************
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
        
        // state equation
        // ***************
        {
            real H;
            x[1] <- 1.0;
            for(t in 2:T) {
                H <- fmin(exp(log(harvest[t-1]) - logK), x[t-1]);
                x[t] <- x[t-1] + r * x[t-1] * (1 - x[t-1]) - H;
            }
        }
    } 
    model {
    
        // prior densities for
        // estimated parameters
        // *********************
        logK ~ uniform(3.0,30.0);
        r ~ lognormal(-1.0,0.20);
        
        // observation equation
        // *********************
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
    }
    '
#}}}
