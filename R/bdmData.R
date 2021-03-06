#'
#' @rdname bdmData-class
#' 
#' @param index Matrix of abundance index values
#' @param harvest Numeric vector of catch values
#' @param time Character vector of time values
#' @param n Shape parameter for generalised production function
#' @param sigmao Matrix of observation error values with dimensions equal to \code{index}
#' @param sigmap Numeric value specifying process error
#' @param renormalise Logical value indicating whether indices should be renormalised to an arithmetic mean of one
#' 
#' @examples
#' # load Indian Ocean albacore data
#' data(albio)
#' 
#' # create data object
#' dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = rownames(albio))
#' 
#' @seealso Use \code{\link{sigmao}} and \code{\link{sigmap}} to set or extract the \eqn{\sigma} values. Use \code{\link{shape}} to set or extract the shape of the production function. 
#' 
#' @include bdmData-class.R
#' 
#' @export
bdmData <- function(index, harvest, time, n, sigmao, sigmap, renormalise = FALSE) {
    new("bdmData", index, harvest, time, n, sigmao, sigmap, renormalise)
}

setMethod("initialize", signature = "bdmData", definition = function(.Object, index, harvest, time, n, sigmao, sigmap, renormalise) {
  
  .Object@.Data <- vector('list',8)
  names(.Object) <- c('T','I','index','harvest','time','n','sigmao','sigmap')
  
  if (!missing(index)) {
    if (any(!is.na(index))) {
    
      index[which(is.na(index))] <- -1
      if (is.vector(index)) index <- as.matrix(index)
      
      colnames(index) <- NULL
      rownames(index) <- NULL
	  
      .Object$T <- T.index <- nrow(index)
      .Object$I <- ncol(index)
      
      .Object$index <- index
      
      # renormalise indices to mean of one
      if (renormalise) {
        #renorm <- function(x) { y<-x[x>0]; x[x>0] <- y/(prod(y)^(1/length(y))); x } # geometric mean
		renorm <- function(x) { y <- x[x > 0]; x[x > 0] <- y/mean(y); x }		# arithmetic mean
        if (.Object$I > 1) {
          .Object$index <- apply(index,2,renorm)
          message('Re-normalised indices')
        } else {
          .Object$index <- renorm(index)
          message('Re-normalised index')
        }
      }
    }
  }
  
  if (!missing(harvest)) {
    if (any(!is.na(harvest))) {
    
      harvest[which(is.na(harvest))] <- 0
      
      .Object$T       <- T.harvest <- length(harvest)
      .Object$harvest <- harvest
    }
  }
  
  if (!missing(index) & !missing(harvest)) 
    if (any(!is.na(index)) & any(!is.na(harvest)))
      if (T.index != T.harvest) 
        stop('index and catch (harvest) must have the same time dimension\n')
  
  if (!missing(time)) {
    .Object$time <- time
  } else .Object$time <- 1:length(harvest)
  
  .Object$n <- 2
  if (!missing(n))
    .Object$n <- n
  
  sigmao.dim    <- c(.Object$T,.Object$I)
  sigmao.length <- .Object$T * .Object$I
  if (!missing(sigmao)) {
    if (length(sigmao) < sigmao.length) {
		if (sigmao.length %% length(sigmao) != 0)
			stop('dimensions for sigmao do not match dimensions for indices\n')
      sigmao <- matrix(sigmao, sigmao.dim[1], sigmao.dim[2], byrow = TRUE)
    }
	.Object$sigmao <- structure(sigmao, .Dim = sigmao.dim)
  } else .Object$sigmao <- structure(rep(0.2, sigmao.length),.Dim = sigmao.dim)
  .Object$sigmao[.Object$index == -1] <- -1
  
  if (!missing(sigmap)) {
    if (length(sigmap) > 1)
      warning('length of sigmap is >1: only first value used\n')
	  .Object$sigmap <- structure(sigmap[1], .Dim = NULL)
  } else .Object$sigmap <- structure(0.05, .Dim = NULL)

  .Object
})

