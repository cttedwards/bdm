\name{bdm-class}
\Rdversion{1.1}
\docType{class}
\alias{bdm-class}
\alias{bdm}

\title{Class \code{"bdm"}}
\description{
This class extends the \code{\linkS4class{stanmodel}} class, including extra slots for the empirical data and outputs from the sampler produced by \code{\link{fit}}. 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"list"} containing empirical data on catches and abundance.}
    \item{\code{init.func}:}{Object of class \code{"function"} which returns initial values for each chain.}
    \item{\code{chains}:}{Object of class \code{"numeric"} specifying the number of chains.}
    \item{\code{iter}:}{Object of class \code{"numeric"} specifying the number of samples for each chain.}
    \item{\code{warmup}:}{Object of class \code{"numeric"} specifying the 'burnin' period for each chain.}
    \item{\code{thin}:}{Object of class \code{"numeric"} specifying the period of sampling from each chain.}
    \item{\code{fit}:}{Object of class \code{"\linkS4class{stanfit}"} holding the raw output from \code{\link[rstan:sampling]{sampling}}.}
    \item{\code{trace}:}{Object of class \code{"list"} holding the processed samples from \code{\link[rstan:extract]{extract}}.}
    \item{\code{model_name}:}{Object of class \code{"character"} giving the model name. Defaults to "BDM".}
    \item{\code{model_code}:}{Object of class \code{"character"} holding the model code in the Stan modelling language.}
    \item{\code{model_cpp}:}{Object of class \code{"list"} translation of Stan code into C++.}
    \item{\code{dso}:}{Object of class \code{"cxxdso"} holding the compiled C++ code as a dynamic shared object.}
  }
}
\section{Constructor}{
  \describe{
    \item{\code{signature(compile = "logical"):}}{Construct a \code{bdm} object with or without the model compiled into a DSO.}
  }
  New \code{bdm} objects can be created directly through \code{bdm(compile=FALSE)} or in an analagous manner using \code{new('edat',compile=FALSE)}. If compilation is not done at initialisation of the object, it can be achieved using \code{\link{compile_bdm}}. 
  
  See the \href{https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started}{RStan Quick Start Guide} to ensure that \code{\link{rstan}} is installed correctly before compilation. 
}
\section{Methods}{
  \describe{
    \item{fit:}{\code{signature(x = "bdm")}. Fit model to the data. See \code{\link{fit}}.}
    \item{plot:}{\code{signature(x = "bdm")}. Plot diagnostics from model fit. See \code{\link{plot.bdm}}.}
	 }
}
\references{
The Stan Development Team (2013) Stan Modeling Language User's Guide and Reference Manual. \url{http://mc-stan.org/}.

\href{https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started}{RStan Quick Start Guide}.
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\link[rstan:rstan-package]{rstan-package}}, \code{\link[bdm:bdm-package]{bdm-package}}
}
\examples{
# initialise object
mdl <- bdm()
# inspect Stan model code
mdl@model_code
# compile model
mdl <- compile_bdm(mdl)
}
\keyword{classes}
