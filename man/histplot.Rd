% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histplot.R
\name{histplot}
\alias{histplot}
\alias{histplot.bdm}
\title{Plot posterior histograms}
\usage{
histplot(object, ...)

\method{histplot}{bdm}(object, pars = c("r", "logK", "lp__"), inc_warmup = FALSE, ...)
}
\arguments{
\item{object}{a \code{bdm} class object.}

\item{...}{additional arguments to the generic function}

\item{pars}{character vector of model parameters to be plotted. Defaults to \code{pars = c('r','logK','lp__')}.}

\item{inc_warmup}{logical value indicating whether MCMC warmup should be included in the plot.}
}
\value{
Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package.
}
\description{
Plots histograms of posterior samples from an MCMC chain contained within a \code{bdm} class object.
}
