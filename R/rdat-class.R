#' Class containing life-history information needed to calculate \eqn{r}
#' 
#' This is an S4 object class that contains the life-history information necessary to calculate the intrinsic growth rate \eqn{r} using the Euler-Lotka equation. 
#'
#' Initialisation is usually through a call to \code{rdat(amax,iter)}. Using this approach specific life-history data can then be included through the assignment functions \code{\link{nmort}}, \code{\link{growth}}, \code{\link{mass}}, \code{\link{sr}} and \code{\link{maturity}}.
#' 
#' @slot iter the number of Monte-Carlo iterations used for generating a distribution of \eqn{r} values
#' @slot amax the maximum age for each life-history data vector
#' @slot sr the type of stock-recruitment relationship used, either Beverton-Holt (\code{'BH'}) or Ricker (\code{'RK'})
#' @slot lhdat list of data matrices containing life-history data-at-age, specifically, mass, maturity, natural mortality and survivorship, as well as a vector of steepness values
#' 
setClass("rdat", slots = list(iter = "numeric", amax = "numeric", sr = "character", lhdat = "list"))