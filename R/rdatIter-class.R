#' Class containing life-history information needed to calculate \eqn{r}
#' 
#' This is an S4 object class similar to \code{\link{rdat}}. It differs from \code{rdat} in that life-history data are stored as vectors (rather than matrices) and therefore correspond to a single monte-carlo interation only. The vectorisation is necessary for efficient estimation of \eqn{r} using \code{\link{rcalc}}.
#'
#' Initialisation is usually through a call to \code{\link{iteration}}, which will extract a single iteration from an \code{rdat} object.
#' 
#' @slot amax the maximum age for each life-history data vector
#' @slot sr the type of stock-recruitment relationship used, either Beverton-Holt (\code{'BH'}) or Ricker (\code{'RK'})
#' @slot lhdat list of numeric vectors containing life-history data-at-age, specifically, mass, maturity, natural mortality and survivorship, as well as a steepness value
#' 
#' @seealso \code{\link{rdat}}, \code{\link{iteration}}, \code{\link{rcalc}}
#' 
#' @export
#' 
setClass("rdatIter", slots = list(amax = "numeric", sr = "character", lhdat = "list"))