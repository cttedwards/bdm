\name{plot.bdm}
\alias{plot.bdm}
\title{
Plot diagnostics from \code{bdm} model fit.
}
\description{
Produce traceplots and histograms for parameters in \code{bdm} object following execution of \code{\link{fit}}.
}
\usage{
plot.bdm(x, par)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x:}{An object of class \code{bdm}}
  
  \item{par:}{
A character vector giving the parameters to be plotted. Defaults to \code{par = c('logK','r0','sigRsq','sigQsq','q','lp__')}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
