\name{survivorship}
\alias{survivorship}
\title{
Survivorship-at-age
}
\description{
Returns survivorship data from an \code{rdat} object
}
\usage{
survivorship(x)
}

\details{
Survivorship is calculated from natural mortality data as: \deqn{Survivorship = exp(-sum(M[1:Age]))}
}
\value{
Returns a matrix of survivorship-at-age across iterations.
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
\code{\linkS4class{rdat}}, \code{\link{nmort}}
}
\examples{
dat <- rdat(amax=30,iter=10)
nmort(dat) <- list(mu=list(M=c(0.1,0.1,0.2)),cv=list(M=0.05))
survivorship(dat)
}
