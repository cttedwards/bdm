\name{nmort}
\alias{nmort}
\alias{nmort<-}
\title{
Natural mortality-at-age
}
\description{
Returns natural mortality data from an \code{rdat} object or assigns data to that object
}
\usage{
nmort(x)
nmort(x) <- data
}
\arguments{
  \item{x}{an \code{rdat} class object}
	\item{data}{a numeric vector or list object containing the components \code{mu} and \code{cv}, or \code{range}}
	\item{mu}{the single parameter \code{M} which can be either a mean value across ages or a numeric vector representing natural mortality from age 1 to \code{length(M)}}
	\item{cv}{optional coefficient of variation}
  \item{range}{optional vector of length two with upper and lower bounds for \code{M} (ignored if \code{mu} is specified)}
}
\details{
When \code{M} is of length one, then it is assumed constant across all ages. When a vector is input it is assumed to represent natural mortality from age 1 to \code{length(M)}. Older ages are set at \code{M[length(M)]}. When a \code{cv} is supplied, each error value is sampled from a log-normal distribution with \eqn{\sigma = \sqrt{ln(1+cv^2)}} and applied consistently across all ages.
}
\value{
Accessor function returns a matrix of natural mortality-at-age across iterations. Assignment function populates the matrix across iterations. If no \code{cv} is provided then each iteration is identical.
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
\code{\linkS4class{rdat}}, \code{\link{survivorship}}
}
\examples{
# vector input
dat <- rdat(amax=30,iter=1)
nmort(dat) <- c(0.1,0.1,0.2)
nmort(dat)

# single value with uncertainty
dat <- rdat(amax=30,iter=10)
nmort(dat) <- list(mu=0.2,cv=0.25)
nmort(dat)

# range of values
dat <- rdat(amax=30,iter=10)
nmort(dat) <- list(range=c(0.1,0.3))
nmort(dat)
}
