#' @title rprior-class definition
#' 
#' @description rprior class definition for bdm packaged
#'
setClass("rprior", contains = "numeric", slots = list(lognormal.par = "list"))