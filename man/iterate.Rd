\name{iterate}
\alias{iterate}
\title{
Add iterative dimension to an \code{rdat} object
}
\description{
Iterate across an \code{rdat} object
}
\usage{
x <- iterate(x,niter)
}
\arguments{
  \item{x}{an \code{rdat} class object}
  \item{niter}{integer indicating number of iterations to add}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an \code{rdat} object with life-history data replicated \code{niter} times}
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
# initialise data object for calculation of r
dat <- rdat(amax=30,iter=1)

# to calculate monte-carlo iterations first either 
# re-initialise 'rdat' object using more iterations or 
# iterate existing object:
dat <- iterate(dat,iter=200)
}
