\name{rcalc}
\alias{rcalc}
\title{
Calculate intrinsic growth rate
}
\description{
Calculate the intrinsic growth rate or distribution of growth rates from life-history data in an \code{rdat} object
}
\usage{
rcalc(x)
}
\arguments{
  \item{x}{an \code{rdat} class object}
}
\details{
Life history parameters are converted into an estimate for the intrinsic rate of population increase using a numeric solution to the Euler-Lotka equation.
}
\value{
Returns an object of class \code{rprior} which contains a vector of \eqn{r} values.
}
\references{
McAllister, M. K., Pikitch, E. K., & Babcock, E. A. (2001). Using demographic methods to construct Bayesian priors for the intrinsic rate of increase in the Schaefer model and implications for stock rebuilding. Canadian Journal of Fisheries and Aquatic Sciences, 58, 1871-–1890.
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\linkS4class{rdat}}, \code{\linkS4class{rprior}}
}
\examples{
# Example calculation of r for New Zealand bluenose

## initialise life-history data object for calculation of r
dat <- rdat(amax=60,iter=200)

## then life-history vectors can be assigned to each iteration
## with or without uncertainty
nmort(dat)    <- list(mu=list(M=0.1))
size(dat)     <- list(mu=list(Linf=92.5,k=0.071,t0=-0.5),cv=list(Linf=0.1,k=0.2,t0=0.5))
mass(dat)     <- list(mu=list(a=9.63e-6,b=3.173))
maturity(dat) <- list(mu=list(acrit=17))
sr(dat)       <- list(type='BH',mu=list(h=0.9))

## we can then calculate an rprior distribution
## using monte-carlo sampling
hist(rr <- rcalc(dat))

}
