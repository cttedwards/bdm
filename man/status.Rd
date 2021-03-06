% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outputs.R
\name{status}
\alias{status}
\alias{status,bdm-method}
\alias{refpoints}
\alias{refpoints,bdm-method}
\title{Extract status and reference point estimates}
\usage{
status(object, ...)

\S4method{status}{bdm}(object, pars)

refpoints(object, ...)

\S4method{refpoints}{bdm}(object, pars)
}
\arguments{
\item{object}{a \code{bdm} model object}

\item{...}{additional arguments to the generic function}

\item{pars}{a \code{character} vector specifiying the parameters to be returned}
}
\value{
Returns a \code{list} containing the specified elements, each of which is a vector equal to the number of posterior samples.
}
\description{
These functions can be used to extract output values following a model fit.
The \code{status} function will look for and return model elements \code{current_biomass}, \code{current_depletion} and \code{current_harvest_rate}. 
The \code{refpoints} function will look for and return model elements \code{msy}, \code{depletion_at_msy}, \code{biomass_at_msy} and \code{harvest_rate_at_msy}.
These are returned by the default \pkg{bdm} model, but the function will also work if a user-specified model contains potentially different elements in the \code{'generated quantities {}'} block of the code, as long as they are specified in the \code{pars} argument.
}
\examples{
data(albio)
dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = rownames(albio))

\dontrun{
# initialize and fit default model
mdl <- bdm()
mdl <- compiler(mdl)
mdl <- sampler(mdl, dat)

# get median status estimates
lapply(status(mdl), median)

# get median reference point estimates
lapply(refpoints(mdl), median)
}

}
