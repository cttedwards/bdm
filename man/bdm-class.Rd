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
    \item{\code{init.func}:}{Object of class \code{"function"} which must return initial values of $r$, $logK$ and $x$ for each chain. Default function fixes $r$ at value contained in \code{model_code} and performs a grid search to estimate $logK$, then initialises each chain with a small degree of variation around these points.}
    \item{\code{init.values}:}{List of initial values populated by \code{init.func}.}
    \item{\code{chains = 4}:}{Object of class \code{"numeric"} specifying the number of chains. See \code{\link[rstan:sampling]{sampling}}.}
    \item{\code{iter = 2000}:}{Object of class \code{"numeric"} specifying the number of samples for each chain. See \code{\link[rstan:sampling]{sampling}}.}
    \item{\code{warmup = floor(iter/2/thin)}:}{Object of class \code{"numeric"} specifying the 'burnin' period for each chain. See \code{\link[rstan:sampling]{sampling}}.}
    \item{\code{thin = 1}:}{Object of class \code{"numeric"} specifying the period of sampling from each chain. See \code{\link[rstan:sampling]{sampling}}.}
    \item{\code{nsamples = ((iter-warmup)*chains)/thin}:}{The resultant number of samples retained from the sample run.}
    \item{\code{trace}:}{Object of class \code{"list"} holding the processed samples from \code{\link[rstan:extract]{extract(..., permuted = TRUE, inc_warmup = FALSE)}}.}
    \item{\code{trace_array}:}{Object of class \code{"array"} holding the processed samples from \code{\link[rstan:extract]{extract(..., permuted = FALSE, inc_warmup = TRUE)}}. This should rarely be needed for direct access but is used for diagnostic trace plots.}
    \item{\code{mpd}:}{List containing output from maximum posterior density fit using \code{\link[rstan:sampling]{optimizing}}.}
    \item{\code{path}:}{Optional path to stan model text file for initialisation of non-default model. This is analagous to the \code{file} argument in \code{\link[rstan:stan_model]{stan_model}}.}
    \item{\code{run}:}{Character string containing an optional label for this particular run.}
    \item{\code{default_model}:}{Object of class \code{"logical"} indicating whether the default model is retained in this particular intialisation. This specifically disables functions that are designed to work only on the default model outputs and should not be set directly by the user.}
    \item{\code{model_name}:}{Object of class \code{"character"} giving the model name. Defaults to "BDM".}
    \item{\code{model_code}:}{Object of class \code{"character"} holding the model code in the Stan modelling language.}
    \item{\code{model_cpp}:}{Object of class \code{"list"} translation of Stan code into C++.}
    \item{\code{dso}:}{Object of class \code{"cxxdso"} holding the compiled C++ code as a dynamic shared object.}
  }
}
\section{Constructor}{
  \describe{
    \item{\code{bdm(path,model.code,model.name='BDM',compile=FALSE, ...):}}{Construct a \code{bdm} object with or without the model compiled into a DSO. The \code{path} argument specifices a connection to a text file containing stan model code. The \code{model.code} argument specificies a character string in the current working environment. See \code{\link[rstan:stan_model]{stan_model}}. If either is not suplied the default model is implemented. If compilation is not done at initialisation of the object, it can be achieved using \code{\link{compile_bdm}}.}
  }
  See the \href{https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started}{RStan Quick Start Guide} to ensure that \code{\link{rstan}} is installed correctly before compilation. 
}
\section{Methods}{
  \describe{
    \item{\code{fit(x = "bdm")}:}{Fit model to the data. See \code{\link{fit}}.}
    \item{\code{traceplot(x = "bdm")}:}{Plot sample traces from model fit. See \code{\link{traceplot}}.}
    \item{\code{histplot(x = "bdm")}:}{Plot sample histograms from model fit. See \code{\link{histplot}}.}
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
# initialise default model object
mdl <- bdm()
# inspect Stan model code
mdl@model_code
# compile model
mdl <- compile_bdm(mdl)
}
\keyword{classes}
