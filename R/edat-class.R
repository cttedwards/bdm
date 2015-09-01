#'
#' Class containing input data for model run
#' 
#' This is an S4 object class that extends the \code{list} base type and allows some error checking to be performed during construction to ensure formatting is appropriate for a \pkg{bdm} model run
#' 
#' @slot .Data \code{list} containing input data
#' @slot names names for each list item
#' \describe{
#'   \item{\code{T}}{Number of discrete time steps in data}
#'   \item{\code{I}}{Number of abundance indices}
#'   \item{\code{index}}{Matrix of abundance index values, with \code{nrow(index)==T} and \code{ncol(index)==I}.}
#'   \item{\code{harvest}}{Numeric vector of catch values, with \code{length(harvest)==T}}
#'   \item{\code{time}}{Character vector of time lables e.g. year values.}
#'   \item{\code{n}}{Shape parameter \eqn{n} for generalised surplus production model.}
#'   \item{\code{sigmap}}{Log-normal process error scale parameter \eqn{\sigma_p}}
#'   \item{\code{sigmao}}{Matrix of log-normal observation error scale parameters \eqn{\sigma_o} with dimensions equal to \code{index}}
#' }
#' 
setClass("edat", contains = "list", slots = list(names = "character"))