#' 
#' @rdname rprior-class
#' 
#' @param x either an integer specifiying the length of an empty vector or a vector of \eqn{r} values
#' 
#' @examples
#' # create object containing
#' # vector of r values
#' iter <- 100
#' mu <- 0.1
#' cv <- 0.2
#' sd <- sqrt(log(1+cv^2))
#' x <- rlnorm(iter,log(mu)-sd^2/2,sd)
#' r <- rprior(x)
#' 
#' @export
#'
#' @include rprior-class.R
#' 
#{{{
rprior <- function(x, ...) new("rprior", x, ...)
#}}}
setMethod("initialize", "rprior", function(.Object, x) {
    
    if (missing(x)) .Object@.Data <- numeric()
    else {
        if (length(x) > 1) .Object@.Data <- as.numeric(x)
        else               .Object@.Data <- numeric(x)
    }
    .Object
})