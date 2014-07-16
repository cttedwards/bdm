\name{fit}
\alias{fit}
\title{
Fit biomass dynamic model to catch and abundance data.
}
\description{
Implement a Bayesian fit of the biomass dynamic model using the function \code{\link[rstan:sampling]{sampling}} in package \code{\link{rstan}}.
}
\usage{
fit(x,data,init,chains = 4,iter = 2000,warmup = floor(iter/2),thin = 1,run,method="MCMC", ...)
}
\arguments{
  \item{x}{An object of class \code{bdm} containing the compiled Stan model.}
  \item{data}{An object of class \code{edat} containing the data. See details.}
  \item{init}{A list of initial values for the estimated parameters or a function with no arguments that returns these values when called.}
  \item{chains}{A positive integer specifying number of parrallel markov chains.}
  \item{iter}{A positive integer specifying how many sample iterations for each chain (including warmup).}
  \item{warmup}{A positive integer specifying the number of warmup ('burnin') iterations during which the proposal disbributions are adapated to optimise the retention rate.}
  \item{thin}{A positive integer specifying the period for saving samples.}
  \item{run}{An optional character string to label the particular run.}
  \item{method}{The fitting method, either \code{"MCMC"} (the default) or \code{"MPD"}, implemented by the functions \code{\link[rstan:optimizing]{optimizing}} or \code{\link[rstan:sampling]{sampling}} respectively.}
  \item{\dots}{Additional arguments to sampling function. See \code{\link[rstan:sampling]{sampling}}.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{bdm} class object containing output samples stored in slot \code{trace} as a named list. 
}
\references{
The Stan Development Team (2013) Stan Modeling Language User's Guide and Reference Manual. \url{http://mc-stan.org/}.
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rstan}} and example in \code{\link{bdm}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
