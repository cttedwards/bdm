#' @title Create rdat object
#' 
#' @description Initialise rdat class object
#' 
#' @export
#' 
#' @include rdat-initialize.R
#'
#{{{
# constructor
rdat <- function(amax, iter = 1, ...) new("rdat", amax, iter, ...)
#}}}
