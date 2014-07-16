\name{sigma}
\alias{sigma}
\alias{sigmao}
\alias{sigmap}
\title{
Scale parameter for process and observation errors 
}
\description{
Returns or assigns log-normal standard deviations for process and observation errors from an \code{edat} object.
}
\usage{
## observation error
sigmao(object)
sigmao(object) <- data

## process error
sigmap(object)
sigmap(object) <- data
}
\arguments{
  \item{object:}{An \code{edat} class object}
	\item{data:}{A numeric value or vector}
}
\details{
A state-space formulation of the biomass dynamic model requires both process and observation errors. These are specified on input since they can rarely be estimated from ecological or fisheries data. Appropriate values are usually in the range \eqn{0.05\le\sigma_p\le0.15} and \eqn{0.10\le\sigma_o\le0.3} for process and observation errors respectively.
}
\value{
Accessor function returns a numeric value. Assignment function populates \code{edat} object.
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
\code{\linkS4class{edat}}
}
\examples{

}
