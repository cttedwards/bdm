#' Class definition for \code{bdm} object
#' 
#' This is the primary object class for the \pkg{bdm} package, containting all the information necessary to fit the model. It inherits from the \code{rstan::stanmodel} class and therefore includes the \code{stan} model code and compiled DSO.
#'
#' @slot data a \code{list} of input data
#' @slot init.func Object of class \code{function} which must return initial values of \eqn{r}, \eqn{ln(K)} and depletion \eqn{x} for each chain. Default function uses \code{\link{getr}}, \code{\link{getlogK}} and \code{\link{getx}} to get values for \eqn{r}, \eqn{logK} and depletion \eqn{x}, then initialises each chain with a small degree of variation around these points.
#' @slot init.values List of initial values populated by \code{init.func}.
#' @slot chains The number of chains. Defaults to \code{chains = 4}.
#' @slot iter The number of samples for each chain. Defaults to \code{iter = 2000}.
#' @slot warmup The 'burnin' period for each chain. Defaults to \code{warmup = floor(iter/2/thin)}.
#' @slot thin The period of sampling from each chain. Defaults to \code{thin = 1}.
#' @slot nsamples The resultant number of samples retained from the sample run. Equal to \code{((iter-warmup)*chains)/thin}.
#' @slot trace \code{list} holding the processed samples from \code{\link[rstan:extract]{extract(..., permuted = TRUE, inc_warmup = FALSE)}}.
#' @slot trace_array \code{array} holding the processed samples from \code{\link[rstan:extract]{extract(..., permuted = FALSE, inc_warmup = TRUE)}}. This should rarely be needed for direct access but is used for diagnostic plots.
#' @slot mpd List containing output from maximum posterior density fit using \code{\link[rstan:sampling]{optimizing}}.
#' @slot path Optional path to stan model text file for initialisation of non-default model. This is analagous to the \code{file} argument in \code{\link[rstan:stan_model]{stan_model}}.
#' @slot run \code{character} string containing an optional label for this particular run.
#' @slot default_model \code{logical} value indicating whether the default model is retained in this particular intialisation. This specifically disables functions that are designed to work only on the default model.
#' @slot model_name \code{character} string giving the model name.
#' @slot model_code the model code in the Stan modelling language.
#' @slot model_cpp translation of Stan code into C++.
#' @slot dso Object of class \code{cxxdso} holding the compiled C++ code as a dynamic shared object.
#' 
#' @seealso See the \href{https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started}{RStan Quick Start Guide} to ensure that \code{\link{rstan}} is installed correctly before compilation. See \code{\link[rstan]{stanmodel}} for details of the superclass. See \code{\link[rstan:sampling]{sampling}} for further details on \code{chains}, \code{iter}, \code{warmup} and \code{thin}.
#'
#{{{
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