\name{sr}
\alias{sr}
\alias{sr<-}
\title{
Stock-recruitment relationship
}
\description{
Returns stock-recruitment relationship from an \code{rdat} object or assigns relationship to that object
}
\usage{
sr(x)
sr(x) <- data
}

\arguments{
  \item{x}{an \code{rdat} class object}
  \item{data}{a list object containing the components \code{type}, \code{mu} and \code{cv}}
  \item{type}{can be either \code{'BH'} or \code{'RK'} indicating the Beverton-Holt and Ricker curves respectively}
  \item{mu}{a list containing the single steepness parameter \code{h} which represents the mean value}
  \item{cv}{a list containing the single steepness parameter \code{h} which represents the coefficient of variation}
}
\details{
The recruitment functions are used to calculte the recruits per spawner, using steepness and the equilibrium spawning biomass per recruit \eqn{SBPR}. For the Beverton-Holt stock recruitment function recruits per spawner is \deqn{Recruits Per Spawner = 4h/(SBPR * (1 - h)),} and for the Ricker function it is \deqn{Recruits Per Spawner = h^1.25 / (SBPR * exp(ln(0.2)/0.8)).} To introduce uncertainty the steepness is represented as: \deqn{h_i = 0.2 + rbeta(\alpha,\beta)(h^{MAX} - 0.2)} where \eqn{h^{MAX}=1} for the Beverton-Holt function and \eqn{h^{MAX}\approx 165} for the Ricker function. The \eqn{\alpha} and \eqn{\beta} parameters of the \eqn{Beta} distribution are found using a numerical search algorithm to give mean and uncertainty values approximately equal to those input. 
}
\value{
Accessor function returns a vector of steepness across iterations. Assignment function populates the vector across iterations. If no \code{cv} is provided then each iteration is identical.}
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
sr(dat) <- list(type='BH',mu=list(h=0.75))
sr(dat)

# multiple stochastic iterations
dat <- rdat(amax=30,iter=10) # or iterate(dat,iter=10)
sr(dat) <- list(type='BH',mu=list(h=0.75),cv=list(h=0.1))
sr(dat)
}
