\name{size}
\alias{size}
\alias{size<-}
\title{
Size-at-age
}
\description{
Returns size (length) data from an \code{rdat} object or assigns data to that object
}
\usage{
size(x)
size(x) <- data
}
\arguments{
  \item{x}{an \code{rdat} class object}
  \item{data}{a list object containing the components \code{mu} and \code{cv}}
	\item{mu}{a list containing the parameters \code{Linf}, \code{k} and \code{t0} representing their mean values}
	\item{cv}{optional list containing parameters \code{Linf}, \code{k} and \code{t0} representing their coefficients of variation}
}
\details{
Size-at-age is calculated using the von Bertalanffy growth function: \deqn{Size = max(Linf * (1 - exp(-k*(Age-t0))),0).} When a \code{cv} is supplied, \code{Linf}, \code{k} and \code{t0} are sampled from a log-normal distribution with \eqn{\sigma = \sqrt{ln(1+cv^2)}}.
}
\value{
Accessor function returns a matrix of size-at-age across iterations. Assignment function populates the matrix across iterations. If no \code{cv} is provided then each iteration is identical.
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
size(dat) <- list(mu=list(Linf=100,k=0.08,t0=-0.5))
size(dat)

# multiple stochastic iterations
dat <- rdat(amax=30,iter=10) # or iterate(dat,iter=10)
size(dat) <- list(mu=list(Linf=100,k=0.08,t0=-0.5),cv=list(Linf=0.1,k=0.2,t0=0.5))
size(dat)
}
