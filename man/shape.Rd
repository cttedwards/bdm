\name{shape}
\alias{shape}
\alias{shape<-}
\title{
Shape of the production function
}
\description{
Returns shape information from an \code{edat} object or assigns a value to that object
}
\usage{
shape(object,par)
shape(object,par) <- data
}
\arguments{
  \item{object:}{An \code{edat} class object}
  \item{par:}{A character string of either \code{'n'} or \code{'phi'}. Defaults to \code{'n'}. See Details.}
	\item{data:}{A numeric value}
}
\details{
The shape parameter determines the depletion at MSY, and can be specified directly as \eqn{n} or via the parameter \eqn{\phi}:
\deqn{
\phi = (1/n)^(1/(n-1))
}
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
