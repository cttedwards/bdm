\name{edat-class}
\Rdversion{1.1}
\docType{class}
\alias{edat}

\title{Class \code{"edat"}}
\description{
A \code{\link{list}} object that contains empirical catch and abundance data necessary for biomass dynamic model fit. Initialisation checks that the data are in a suitable format.
}
\section{List items}{
  \describe{
    \item{\code{T}:}{Number of discrete time steps in data.}
    \item{\code{I}:}{Number of abundance indices.}
    \item{\code{index}:}{Matrix of abundance index values, with \code{nrow(index)==T} and \code{ncol(index)==I}.}
    \item{\code{harvest}:}{Numeric vector of catch values, with \code{length(harvest)==T}}
    \item{\code{time}:}{Character vector of time lables e.g. year values.}
    \item{\code{n = 2}:}{Shape parameter \eqn{n} for generalised surplus production model.}
    \item{\code{sigmap = 0.05}:}{Log-normal process error scale parameter \eqn{\sigma_p}}
    \item{\code{sigmao = structure(rep(0.2,I),.Dim=I)}:}{Log-normal observation error scale parameter \eqn{\sigma_o}}
    }
}
\section{Constructor}{
  \describe{
    \item{\code{edat(index,harvest,time,n,sigmap,sigmao,renormalise=TRUE):}}{Create an \code{edat} class object containing catch and/or index data. List items \code{T} and \code{I} are populated automatically. Both \code{index = missing} and \code{harvest = missing} are allowed. Index \code{NA} values will be replaced by -1, however missing catch values will cause the fitting routine to fail. If \code{renormalise=TRUE} the indices are renormalised to an arithmetic mean of one.}
    }
}
\section{Methods}{
  \describe{
    \item{\code{sigmap(x = "edat")}:}{Access or assign process error scale parameter.}
    \item{\code{sigmao(x = "edat")}:}{Access or assign observation error scale parameter.}
    \item{\code{shape(x = "edat",par="character")}:}{Access or assign shape of the production curve, which is either the shape parameter \eqn{n} or the depletion at MSY relative to K (\eqn{\phi}). See \code{\link[bdm:shape]{shape}.}
    }
}}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
Charles T T Edwards \email{charles.edwards@niwa.co.nz}
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\link[bdm:sigmap]{sigmap}}, \code{\link[bdm:sigmao]{sigmao}}, \code{\link[bdm:shape]{shape}}
}
\examples{
data(albio)
dat <- edat(harvest=albio$catch,index=albio$cpue,time=rownames(albio))
sigmao(dat)
(sigmao(dat) <- 0.15)
shape(dat)
(shape(dat,'phi') <- 0.4)
}
\keyword{classes}
