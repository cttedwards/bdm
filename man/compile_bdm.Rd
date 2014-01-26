\name{compile_bdm}
\alias{compile_bdm}
\title{
Compile Stan model code in \code{bdm} object.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compile_bdm(.Object, model_code)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.Object}{
Object of class \code{bdm}
}
  \item{model_code}{
Character string describing model code. See Examples.
}
}
\details{
%% ~~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\href{https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started}{RStan Quick Start Guide}.
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\note{
See the \href{https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started}{RStan Quick Start Guide} to ensure that \code{\link{rstan}} is installed correctly before compilation.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

library(bdm)

# initialise object
mdl <- bdm(compile=FALSE)

# inspect default code
mdl

# compile
mdl <- compile_bdm(mdl)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
