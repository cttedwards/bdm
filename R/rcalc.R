#'
#' Calculate the intrinsic growth rate
#' 
#' A value for the intrinsic growth rate \eqn{r} is calculated using life-history data as a numerical solution to the Euler-Lotka equation.
#' 
#' @include rdat-class.R
#' @include rdatIter-class.R
#' @include getr.R
#' 
#' @export
#' 
setGeneric("rcalc", function(.Object, ...) standardGeneric("rcalc"))
#'
#' @rdname rcalc
#' 
#' @param .Object an \code{rdat} object
#' 
#' @return A \code{\link{rprior}} class object containing a vector of values for \eqn{r}.

#' @export
#' 
setMethod("rcalc", signature = "rdat", function(.Object) {
  
  n <- .Object@iter
  prior <- new('rprior',n)
  
  for (i in 1:n)
    prior[i] <- rcalc(iteration(.Object, i))
  
  prior@lognormal.par <- getr(prior)
  
  return(prior)
  
})
setMethod("rcalc", signature = "rdatIter", function(.Object) {
  
  # survivorship
  l <- .Object@lhdat[['survivorship']]
  
  # spawning biomass per recruit
  SBPR  <- sum(.Object@lhdat[['survivorship']] * .Object@lhdat[['mass']] * .Object@lhdat[['maturity']])
  #cat('SBPR:',SBPR,'\n')
  
  # recruits per unit of spawning biomass
  if (.Object@sr == 'BH') {
    alpha <- (4 * .Object@lhdat[['h']])/(SBPR * (1 - .Object@lhdat[['h']]))
  } else if (.Object@sr == 'RK') {
    alpha <- .Object@lhdat[['h']]^1.25 / (SBPR * exp(log(0.2)/0.8))
  } else stop("sr must be either 'BH' or 'RK'\n")
  #cat('alpha:',alpha,'\n')
  
  # female fecundity at age 
  # (recruited mature biomass per unit of spawning biomass)
  m <- alpha * .Object@lhdat[['mass']] * .Object@lhdat[['maturity']]
  
  # minimise Euler-Lotka equation
  obj <- function(r) sum(exp(-r * 1:.Object@amax) * l * m) - 1
  r <- uniroot(obj,interval = c(0,10))$root
  
  # return intrinsic growth rate
  return(r)
  
})


#}}}
