\name{residuals.bdm}
\alias{residuals.bdm}
\title{
Extract residuals from \code{bdm} model fit.
}
\description{
Extract process and observation error residuals from \code{bdm} object following execution of \code{\link{fit}}.
}
\usage{
## S3 method for class 'bdm'
residuals(object,log = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object:}{An object of class \code{bdm}.}
  \item{log:}{Whether to return natural log-transform of residuals.}
}
\details{
Residuals take the form of a ratio. For the process error this is a ratio of the estimated value of \eqn{x} over the value expected from a deterministic execution of the production function in the previous time step. For the observation error it is the observed value over the product \eqn{qx}, which is more accurately referred to as the total error, since it includes both observation and process error components. The logarithm of these ratios should be normally distributed. 
}
\value{
A list containing:
\item{total_error:}{Matrix of \eqn{\epsilon_o} values by time and iteration.}
\item{process_error:}{Matrix of \eqn{\epsilon_p} values by time and iteration.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\note{
Residuals are contained in the matrices \code{object@trace[['epsilon_o']]} and \code{object@trace[['epsilon_o']]}. This function will access these matrices and is intended for application to the default model only (i.e. \code{object@default_model==TRUE}). If the default model is not being used, then it should still output values for \code{epsilon_o} and \code{epsilon_p} if this function is to be used.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See example in \code{\link{bdm}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
