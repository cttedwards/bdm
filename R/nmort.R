#'
#' Access or assign natural mortality at age
#' 
#' This function can be used to access or assign the natural mortality at age in an \code{\link{rdat}} object. 
#' 
#' Assignment can be of a list or numeric vector. When a list is assigned it should contain a value for \code{mu}, which is the mean natural mortality at age and may or may not be constant across ages.
#' 
#' If the list also contains \code{cv} then log-normal uncertainty is added around the mortality at age. If the list contains \code{range} then the uncertainty is bounded at these values. If \code{range} is provided but not \code{cv}, and if mortality is constant across ages, then a uniform uncertainy is implemented.
#' 
#' @param object a \code{\link{rdat}} object
#' @param value either a named \code{list} or \code{numeric} vector 
#' \describe{
#'  \item{\code{numeric}}{a single numeric or vector of values}
#'  \item{\code{list}}{a list object containing the components \code{mu}, \code{cv} and \code{range}}
#'  \item{\code{mu}}{a single numeric or vector of values representing the mean natural mortality}
#'  \item{\code{cv}}{optional single numeric value representing the coefficient of variation}
#'  \item{\code{range}}{optional vector of length two containing the lower and upper bounds}
#' }
#' 
#' @return Accessor function returns a matrix of maturity at age across iterations. Assignment function populates the matrix across iterations.
#'
#' @examples
#' # M at age vector input
#' dat <- rdat(amax = 10,iter=1)
#' nmort(dat) <- c(0.1,0.1,0.2)
#' nmort(dat)
#' 
#' # constant M with log-normal uncertainty
#' dat <- rdat(amax=10, iter = 20)
#' nmort(dat) <- list(mu = 0.2, cv = 0.25)
#' nmort(dat)
#' 
#' # constant M with bounded log-normal uncertainty
#' dat <- rdat(amax = 10, iter = 20)
#' nmort(dat) <- list(mu = 0.2, cv = 0.5, range = c(0.1,0.3))
#' nmort(dat)
#' 
#' # constant M with uniform uncertainty
#' dat <- rdat(amax = 10,iter = 20)
#' nmort(dat) <- list(range = c(0.1,0.3))
#' nmort(dat)
#' 
#' # M at age with log-normal uncertainty
#' dat <- rdat(amax=10, iter = 20)
#' nmort(dat) <- list(mu = c(0.1,0.1,0.2), cv = 0.25)
#' nmort(dat)
#' 
#' # M at age with bounded log-normal uncertainty
#' dat <- rdat(amax = 10, iter = 20)
#' nmort(dat) <- list(mu = c(0.1,0.1,0.2), cv = 0.25, range = c(0.1,0.3))
#' nmort(dat)
#' 
#' @include rdat-class.R
#' 
#{{{ accessor function
#' @export
setGeneric("nmort", function(object, ...) standardGeneric("nmort"))
#' @rdname nmort
setMethod("nmort",signature(object = "rdat"), function(object) return(object@lhdat$M)
)
#}}}

#{{{ assignment functions
#' @rdname nmort
#' @export
setGeneric("nmort<-", function(object, value) standardGeneric("nmort<-"))
#{{ list
#' @rdname nmort
setMethod("nmort<-",
          signature(object = "rdat",value = "list"),
          function(object, value) {
            
            if (!is.null(value$mu)) {
              M.mu <- value$mu
              if (length(M.mu) < object@amax)
                M.mu[(length(M.mu) + 1):object@amax] <- rep(M.mu[length(M.mu)],object@amax - length(M.mu))
              
              if (!is.null(value$cv)) {
                M.sd <- sqrt(log(1 + value$cv^2)) 
                object@lhdat[['M']] <- apply(object@lhdat[['M']],2,function(y) M.mu * rlnorm(1,-M.sd^2/2,M.sd))
                if (!is.null(value$range)) {
                  low <- value$range[1]
                  upp <- value$range[2]
                  object@lhdat[['M']] <- apply(object@lhdat[['M']],2,function(y) {yy <- y; yy[which(yy < low)] <- low; yy[which(yy > upp)] <- upp; yy})
                }
              } else {
                object@lhdat[['M']] <- apply(object@lhdat[['M']],2,function(y) M.mu)
              }
            } else { 
              if (!is.null(value$range)) {
                low <- value$range[1]
                upp <- value$range[2]
                object@lhdat[['M']] <- apply(object@lhdat[['M']],2,function(y) rep(runif(1,low,upp),object@amax))
              } else {
                stop('must supply mean or range of values\n')
              }
            }
            
            # calculate survivorship (assuming constant M-at-age)
            object@lhdat[['survivorship']] <- object@lhdat[['M']]
            object@lhdat[['survivorship']] <- apply(object@lhdat[['survivorship']],2,function(y) {  m <- y; 
                                                                                          y[1] <- 1;
                                                                                          for (a in 2:object@amax)
                                                                                            y[a] <- y[a - 1]*exp(-m[a - 1]);
                                                                                          y
                                                                                         })
            # calculate survivorship (for age dependent M)
            #for(i in 1:object@iter)
            #  for(a in 1:object@amax)
            #    object@lhdat[['survivorship']][a,i] <- exp(-sum(object@lhdat[['M']][1:a,i]))
            
            object
          }
)
#}}
#{{ numeric
#' @rdname nmort
setMethod("nmort<-",
          signature(object = "rdat", value = "numeric"),
          function(object, value) {
            
            M.mu <- value
            if (length(M.mu) < object@amax)
              M.mu[(length(M.mu) + 1):object@amax] <- rep(M.mu[length(M.mu)],object@amax - length(M.mu))
            
            object@lhdat[['M']] <- apply(object@lhdat[['M']],2,function(y) M.mu)
                        
            # recalculate survivorship
            for (i in 1:object@iter)
              for (a in 1:object@amax)
                object@lhdat[['survivorship']][a,i] <- exp(-sum(object@lhdat[['M']][1:a,i]))
            
            object
          }
)
#}}
#}}}
