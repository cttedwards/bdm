\name{update_bdm}
\alias{update_bdm}
\title{
Update prior distributions in \code{bdm} object
}
\description{
Modifies model code in \code{\link{bdm}} to give alternative paramaterisations for priors of the intrinsic growth rate (\eqn{r}), observation error variance (\eqn{\sigma_R^2}), process error variance (\eqn{\sigma_Q^2}) or the natural log of the carrying capacity (\eqn{ln(K)}). Distributions are parameterised directly, with the exception that the prior for \eqn{r} can be modified by supplying a \code{\link{rprior}} object containing monte-carlo samples.
}
\usage{
update_bdm(x,y,compile,plot)
}
\arguments{
  \item{x}{a \code{bdm} class object}
  \item{y}{either a \code{\link{rprior}} class object containing monte-carlo samples from the prior distribution of \eqn{r} or a list of values containing parameters for the relevant distribution in the form \code{list(a,b,par)}. See Details.}
  \item{compile}{Logical. Should \code{bdm} object be compiled? Defaults to FALSE.}
  \item{plot}{Logical. Should parametric distribution be plotted? Defaults to FALSE.}
}
\details{
\describe{
\item{r:}{log-normal distribution specified as \code{y=list(a=logmu,b=logsigma,par='r')}}
\item{logK:}{uniform distribution specified as \code{y=list(a=lower,b=upper,par='logK')}}
\item{sigRsq:}{inverse gamma distribution specified as \code{y=list(a=alpha,b=beta,par='sigRsq')}}
\item{sigQsq:}{inverse gamma distribution specified as \code{y=list(a=alpha,b=beta,par='sigQsq')}}
}
}
\value{
Returns an object of class \code{bdm} which contains un updated prior distribution for the specified parameter.
}
\references{
Meyer, R., & Millar, R. B. (1999). BUGS in Bayesian stock assessments. Canadian Journal of Fisheries and Aquatic Sciences, 56, 1078-1086.
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\linkS4class{bdm}}, \code{\linkS4class{rprior}}
}
\examples{
# initialise object
x <- bdm()

# create rprior object and input
r <- rprior(rlnorm(1e5,log(0.1)-0.02,0.2))
x <- update_bdm(x,r,plot=TRUE) 

# inspect modified code
x

# alternatively input parameters directly
# e.g. Meyer and Millar (1999)
x <- update_bdm(x,list(a=-1.38,b=0.51,par='r'),plot=TRUE)
x <- update_bdm(x,list(a=1.7,b=0.01,par='sigRsq'),plot=TRUE)
x <- update_bdm(x,list(a=3.8,b=0.01,par='sigQsq'),plot=TRUE)
x

}
