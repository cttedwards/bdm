\name{bdm-package}
\alias{bdm-package}
\docType{package}
\title{
Bayesian biomass dynamic model of population dynamics
}
\description{
Fit a logistic surplus production model to time series of harvest and abundance data using Bayesian methods, including calculation of a prior for the intrinsic rate of increase. 
}
\details{
\tabular{ll}{
Package: \tab bdm \cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-01-07\cr
License: \tab GPL-3 \cr
Depends: \tab R(>= 3.0.1), methods, rstan, DEoptim, pscl\cr
}
The maximum rate of intrinsic growth is estimated from life history data using methods described in McAllister et al. (2001). This can then used as a prior for \code{r}. Parameterisation of a discrete logistic surplus production model:
\deqn{B_{t+1} = B_t + r_tB_t(1 - \frac{B_t}{K}) - C_t}
is then acheived using Stan, a C++ package for obtaining Bayesian inference using the No-U-turn sampler, a variant of Hamiltonian Monte Carlo. The statistical formulation is that of a state-space model, as described by Meyer and Millar (1999), which includes log-normal process and observation error terms with estimated variances \eqn{\sigma_Q^2} and \eqn{\sigma_R^2} respectively. Catchability is assumed to be equal to its maximum likelihood value. 
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\references{
The Stan Development Team (2013) Stan Modeling Language User's Guide and Reference Manual. \url{http://mc-stan.org/}.

McAllister, M. K., Pikitch, E. K., & Babcock, E. A. (2001). Using demographic methods to construct Bayesian priors for the intrinsic rate of increase in the Schaefer model and implications for stock rebuilding. Canadian Journal of Fisheries and Aquatic Sciences, 58, 1871-–189.

Meyer, R., & Millar, R. B. (1999). BUGS in Bayesian stock assessments. Canadian Journal of Fisheries and Aquatic Sciences, 56, 1078-1086.

Millar, R. B., & Meyer, R. (2000). Non-linear state space modelling of fisheries biomass dynamics by using Metropolis-Hastings within-Gibbs sampling. Journal of the Royal Statistical Society: Series C (Applied Statistics), 49(3), 327-342.
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS ~~
%~~ in the R documentation directory ~~
\keyword{package}
\seealso{
\code{\link[rstan:rstan-package]{rstan}}, \code{\link{rcalc}}
}
\examples{
library(bdm)

# load South Atlantic albacore data
data(albacore)
dat <- edat(catch=albacore$catch,index=albacore$cpue)

# initialise object
mdl <- bdm()

# update r-prior according to 
# Meyer and Millar (1999)
mdl <- update_bdm(mdl,list(a=-1.38,b=0.51,par='r'))

# inspect model code
mdl

# compile and run model
mdl <- compile_bdm(mdl)
mdl <- fit(mdl,dat)

# sample matrices
names(mdl@trace)

# inspect default trace and histogram outputs
plot(mdl)

}
