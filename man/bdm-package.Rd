\name{bdm-package}
\alias{bdm-package}
\docType{package}
\title{
Bayesian biomass dynamic model of population dynamics
}
\description{
Fit a generalised surplus production model to time series of harvest and abundance data using Bayesian methods, including calculation of a prior for the intrinsic rate of increase. 
}
\details{
\tabular{ll}{
Package:\tab bdm\cr
Version:\tab 1.0\cr
Date:\tab 2014-06-01\cr
License:\tab GNU General Public License Version 3\cr
Depends:\tab R(>= 3.1.0), methods, rstan, ggplot2, reshape2, plyr\cr
Imports:\tab kobe(>= 1.3.2)\cr
}
The maximum rate of intrinsic growth is estimated from life history data using methods described in McAllister et al. (2001). The generalised production model is described in McAllister et al. (2000). Estimation is achieved using Stan, a C++ package for obtaining Bayesian inference using the No-U-turn sampler, a variant of Hamiltonian Monte Carlo. 
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\references{
The Stan Development Team (2013) Stan Modeling Language User's Guide and Reference Manual. \url{http://mc-stan.org/}.

McAllister, M. K., Pikitch, E. K., & Babcock, E. A. (2001). Using demographic methods to construct Bayesian priors for the intrinsic rate of increase in the Schaefer model and implications for stock rebuilding. Canadian Journal of Fisheries and Aquatic Sciences, 58, 1871-–189

M. K. McAllister, E. A. Babcock, E. K. Pikitch, and M. H. Prager. (2000) Application of a non-equilibrium generalized production model to south and north atlantic swordfish: combining Bayesian and demographic methods for parameter estimation. Collected Volume of Scientific Papers ICCAT, 51(5):1253-1550.

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

# load indian ocean albacore data
data(albio)
dat <- edat(harvest=albio$catch,index=albio$cpue,year=rownames(albio))

# initialise object
mdl <- bdm()

# update data and priors using values from Meyer and Millar (CJFAS 1999)
# error terms
sigmao(dat) <- 0.11
sigmap(dat) <- 0.06
# intrinsic growth rate
mdl <- update_bdm(mdl,list(a=-1.38,b=0.51,par='r'))

# inspect model code
mdl

# compile and run model
mdl <- compile_bdm(mdl)
mdl <- fit(mdl,dat)

# sample matrices
names(mdl@trace)

# inspect default trace and histogram outputs
traceplot(mdl)
histplot(mdl)

# examine residual distributions
hist(residuals(mdl)$total_error)
hist(residuals(mdl)$process_error)


}
