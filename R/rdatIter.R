#' @title Create rdatIter object
#' 
#' @description Initialise rdatIter class object
#' 
#' @export
#' 
#' @include rdatIter-initialize.R
#'
#{{{
# constructor
rdatIter <- function(amax, sr, ...) new("rdatIter", amax, sr, ...)
#}}}
