\name{edat-class}
\Rdversion{1.1}
\docType{class}
\alias{edat}

\title{Class \code{"edat"}}
\description{
A \code{\link{list}} object that contains empirical catch and abundance data necessary for biomass dynamic model fit.
}
\section{List items}{
  \describe{
    \item{T:}{Number of years of data.}
    \item{N:}{Number of abundance indices.}
    \item{index:}{Matrix of abundance index values, with \code{nrow(index)==T}} and \code{ncol{index}==N}.
    \item{catch:}{Numeric vector of catch values, with \code{length(catch)==T}}
    }
}
\section{Constructor}{
  \describe{
    \item{\code{signature(index = "matrix",catch = "numeric"):}}{Create an \code{edat} class object containing catch and/or index data. List items \code{T} and \code{N} are populated automatically. Both \code{index = missing} and \code{catch = missing} are allowed. Index \code{NA} values will be replaced by -1. Catch \code{NA} values will also be converted to -1, however missing catch values are not allowed and will cause the fitting routine to fail.}
    }
   New \code{edat} objects can be created directly through \code{edat(index,catch)} or in an analagous manner using \code{new('edat',index,catch)}. 
}
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

}
\examples{
data(albacore)
edat(catch=albacore$catch,index=albacore$cpue)
}
\keyword{classes}
