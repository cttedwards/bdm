\name{maturity}
\alias{maturity}
\alias{maturity<-}
\title{
Maturity-at-age
}
\description{
Returns maturity data from an \code{rdat} object or assigns data to that object
}
\usage{
maturity(x)
maturity(x) <- data
}
\arguments{
  \item{x}{an \code{rdat} class object}
	\item{data}{a list object containing the components \code{mu} and \code{cv}}
	\item{mu}{a list containing parameters \code{acrit} and optionally \code{delta} representing their mean values}
	\item{cv}{optional list containing parameters \code{acrit} and \code{delta} representing their coefficients of variation}
}
\details{
If only \code{acrit} is supplied then maturity is knife-edged at that age. If both \code{acrit} and \code{delta} are supplied then maturity-at-age is calculated as a logistic ogive: \deqn{Maturity = 1/(1+exp((acrit-Age)/delta)).} For the logistic ogive, but not for the knife-edged ogive, when a \code{cv} is supplied \code{acrit} and \code{delta} are sampled from a log-normal distribution with \eqn{\sigma = \sqrt{ln(1+cv^2)}}.
}
\value{
Accessor function returns a matrix of maturity-at-age across iterations. Assignment function populates the matrix across iterations. If no \code{cv} is provided then each iteration is identical.
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
# knife-edged ogive
dat <- rdat(amax=30,iter=1)
maturity(dat) <- list(mu=list(acrit=10))
maturity(dat)

# logistic ogive with uncertainty
dat <- rdat(amax=30,iter=10) # or iterate(dat,iter=10)
maturity(dat) <- list(mu=list(acrit=1,delta=3),cv=list(acrit=0.01,delta=0.01))
maturity(dat)
}
