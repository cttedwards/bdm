\name{project}
\alias{project}
\title{
Projection function for \code{bdm} class object
}
\description{
Projects biomass forward in time under constant catch or harvest rate scenarios, sampling parameter uncertainty from posterior distributions.
}
\usage{
project(object,harvest.project,time.project,harvest_rate=TRUE)
}
\arguments{
  \item{object:}{A \code{bdm} class object produced by \code{\link[bdm:fit]{fit}}.}
	\item{harvest.project:}{A numeric value or vector, specifying a range of constant catches or harvest rates, with one value for each scenario.}
  \item{time.project:}{Number of time steps for projection.}
  \item{harvest_rate:}{A logical value indicating whether values in \code{harvest.project} are catches or harvest rates.}
}
\details{
Projections are stochastic and sample fully from posterior parameter distributions. Auto-correlation is currently included in the process error with a value of \eqn{\rho=0.43}.
}
\value{
A list containing:
\item{run:}{Character string containing run label in \code{object@run}.}
\item{scenarios:}{Input values from \code{harvest.project}.}
\item{time:}{Time dimension labels.}
\item{nsamples:}{Number of posterior samples. Equal to \code{object@nsamples}.}
\item{biomass:}{Array of biomass values over iterations, time and scenario.}
\item{depletion:}{Array of depletion (\eqn{x}) values over iterations, time and scenario.}
\item{epsilon_p:}{Matrix of process error terms over iteration and time. These are the same for each projection scenario.}
\item{harvest:}{Array of catches over iterations, time and scenario.}
\item{harvest_rate:}{Array of harvest rates over iterations, time and scenario.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\note{
%%  ~~further notes~~
}

\seealso{

}
\examples{

}
