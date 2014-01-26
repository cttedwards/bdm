\name{rdat-class}
\Rdversion{1.1}
\docType{class}
\alias{rdat-class}

\title{Class \code{"rdat"}}
\description{
A class of object that contains life history data necessary for calculation of the intrinsic rate of increase (\eqn{r}).
}
\section{Slots}{
  \describe{
    \item{\code{iter}:}{Object of class \code{"numeric"} giving the number of Monte-Carlo iterations used for generating a distribution of \eqn{r} values}
    \item{\code{amax}:}{Object of class \code{"numeric"} giving the maximum age for each life-history data vector }
    \item{\code{sr}:}{Object of class \code{"character"} indicating the \code{type} of stock-recruitment relationship used, either Beverton-Holt (\code{'BH'}) or Ricker (\code{'RK'})}
    \item{\code{lhdat}:}{\code{"list"} object of data matrices containing life-history data-at-age, specifically, mass, maturity, natural mortality and survivorship, as well as a vector of steepness values}
  }
}
\section{Constructor}{
  \describe{
    \item{\code{signature(amax = "numeric",iter = "numeric"):}}{Empty object with correct age and iteration dimensions.}
    \item{\code{signature(amax = "numeric",iter = "numeric",nmort = "list",growth = "list",mass = "list",sr = "list",maturity = "list"):}}{Optionally, life-history data can be input during initialisation using the \code{list(mu=list(),cv=list())} syntax. See Methods for further details.}
  }
  Although it is possible to initialise objects using \code{new('rdat', ...)} the recommended procedure is to through a call to \code{rdat(amax,iter)}. Specific life-history data can then be included through the assignment functions \code{\link{nmort}}, \code{\link{growth}}, \code{\link{mass}}, \code{\link{sr}}, \code{\link{maturity}}.
}
\section{Methods}{
  \describe{
    \item{mass, maturity, nmort, size, sr, survivorship:}{Accessor and assignment functions. See the appropriate documentation in \code{\link{mass}}, \code{\link{maturity}}, \code{\link{nmort}}, \code{\link{size}}, \code{\link{sr}} and \code{\link{survivorship}}}
    \item{iterate:}{\code{signature(x = "rdat", niter = "numeric")}. Add iteration dimension to existent \code{rdat} object. See \code{\link{iterate}}.}
    \item{rcalc:}{\code{signature(x = "rdat")}. See documentation for \code{\link{rcalc}} on how to calculate \eqn{r}}
  }
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\note{
%%  ~~further notes~~
}
\seealso{
\code{\linkS4class{rprior}}
}
\examples{
# initialise rdat object
amax <- 100
iter <- 1
dat <- rdat(amax,iter)

# assign life-history data
nmort(dat)    <- list(mu=list(M=0.18))
size(dat)     <- list(mu=list(Linf=106.5,k=0.229,t0=0.01)) 
mass(dat)     <- list(mu=list(a=1.7e-9,b=3.328))
sr(dat)       <- list(type='BH',mu=list(h=0.9))
maturity(dat) <- list(mu=list(acrit=8))
}
\keyword{classes}
