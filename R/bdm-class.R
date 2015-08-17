#' @title bdm-class definition
#' 
#' @description Bayesian biomass dynamic model (bdm) class definition
#'
#{{{
# class definition
setClass("bdm",contains = "stanmodel",
         slots = list(data = "list",          # edat object or list
                    init.func = "function",   # initialisation function
                    init.values = "list",     # initial values populated by init.func
                    chains = "numeric",       # number of MCMC chains
                    iter = "numeric",         # number of MCMC iterations per chain
                    warmup = "numeric",       # number of iterations under adaptive sampling
                    thin = "numeric",         # interval between recorded samples
                    nsamples = "numeric",     # total number of posterior samples recorded
                    trace_array = "array",    # array of posterior samples including warmup
                    trace = "list",           # list of posterior samples without warmup and with chains mixed
                    mpd = "list",             # mpd output from rstan::optimizing()
                    path = "character",       # optional path to stan model code file for initialisaton of non-default model
                    run = "character",        # optional label for this particular run
                    default_model = "logical" # is the default fletcher-schaefer hybrid model retained?
         )
)
#}}}