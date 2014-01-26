\name{rprior-class}
\Rdversion{1.1}
\docType{class}
\alias{rprior-class}

\title{Class \code{"rprior"}}
\description{
A class of object containing a prior distribution for the intrinsic growth rate \eqn{r}. It includes both a numeric vector for values generated using Monte Carlo methods and a list of parameters describing the associated log-normal distribution.  
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{\linkS4class{numeric}} containing a vector of \eqn{r} values}
    \item{\code{lognormal.par}:}{Object of class \code{\linkS4class{list}} containing values for mu, sigma, theta and nu.}
  }
}
\section{Constructor}{
  \describe{
    \item{\code{signature(x = missing)}}{Create an empty object.}
    \item{\code{signature(x = numeric)}}{If \code{length(x)>1} then create an object containing values of \code{x}, otherwise create a vector of zero's of length equal to \code{x}.}
   }
  New \code{rprior} objects can be created directly through \code{rprior(x)} or in an analagous manner using \code{new('rprior',x)}.
}
\section{Methods}{
  \describe{
    \item{fit_rprior}{}
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
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
# create empty object
r <- rprior()

# create object containing
# vector of r values
iter <- 100
mu <- 0.1
cv <- 0.2
sd <- sqrt(log(1+cv^2))
x <- rlnorm(iter,log(mu)-sd^2/2,sd)
r <- rprior(x)
}
\keyword{classes}
