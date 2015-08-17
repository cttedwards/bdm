#' @title Create rprior object
#' 
#' @description Initialise rprior class object
#' 
#' @export
#' 
#' @include rprior-initialize.R
#'
#{{{
# constructor
rprior <- function(x = 0L) new("rprior", x)
#}}}
