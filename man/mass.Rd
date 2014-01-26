\name{mass}
\alias{mass}
\alias{mass<-}
\title{
Mass-at-age data
}
\description{
Returns mass data from an \code{rdat} object or assigns data to that object
}
\usage{
mass(x)
mass(x) <- data
}

\arguments{
	\item{x}{an \code{rdat} class object}
	\item{data}{a list object containing the components \code{mu} and \code{cv}}
  \item{mu}{a list containing parameters \code{a} and \code{b} representing their mean values}
	\item{cv}{optional list containing parameters \code{a} and \code{b} representing their coefficients of variation}
}
\details{
Mass-at-age is calculated as \deqn{Mass=aAge^b.} When a \code{cv} is supplied, \code{a} and \code{b} are sampled from a log-normal distribution with \eqn{\sigma = \sqrt{ln(1+cv^2)}}.
}
\value{
Accessor function returns a matrix of mass-at-age across iterations. Assignment function populates the matrix across iterations. If no \code{cv} is provided then each iteration is identical.
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
\code{\linkS4class{rdat}}
}
\examples{
# single iteration
dat <- rdat(amax=30,iter=1)
mass(dat) <- list(mu=list(a=9.63e-6,b=3.173))
mass(dat)

# multiple stochastic iterations
dat <- rdat(amax=30,iter=10) # or iterate(dat,iter=10)
mass(dat) <- list(mu=list(a=9.63e-6,b=3.173),cv=list(a=0.05,b=0.05))
mass(dat)
}
