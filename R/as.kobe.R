#'
#' Convert a \code{bdm} object to a format suitable for the \pkg{kobe} package
#' 
#' The \pkg{kobe} package can be used to produce diagnostic outputs concerning status of the stock relative to MSY-based reference points.
#' 
#' @include bdm-class.R
#' 
#' @export
#{{{ as.kobe functions
setGeneric("as.kobe", function(.Object, ...) standardGeneric("as.kobe"))
#'
#' @param .Object a \code{bdm} class object
#' @param projection an optional \code{list} containing results from a call to \code{\link{project}}
#' @param what,prob,year,nwrms see documentation for the \pkg{kobe} package
#' 
#' @examples
#' # see vignette
#' vignette('bdm-examples')
#' 
#' @rdname as.kobe
#{{ convert bdm object into kobe dataframe with or without projections
setMethod("as.kobe",signature = c("bdm"),function(.Object, projection,
                                                  what=c("sims","trks","pts","smry","wrms")[1],
                                                  prob=c(0.75,0.5,.25), year = NULL, nwrms=10) {
      
      
    if (missing(projection)) {
      
      res <- .read_bdm(.Object)
      res <- .io_bdm(res,what = what,prob = prob,nwrms = nwrms,year = year)
      
    } else {

      res <- .read_bdm_projection(.Object,projection)
      
      res <- lapply(res,FUN = .io_bdm, what = what, prob = prob, nwrms = nwrms, year = year)
                       
      res <- list(trks = plyr::ldply(res, function(x) x$trks,.id='projection_value'),
               pts = plyr::ldply(res, function(x) x$pts,.id='projection_value'),
               smry = plyr::ldply(res, function(x) x$smry,.id='projection_value'),
               wrms = plyr::ldply(res, function(x) x$wrms,.id='projection_value'),
               sims = plyr::ldply(res, function(x) x$sims,.id='projection_value'))
      }

    
    if (length(what)==1)
       return(res[[what]])
    else
       return(res[what]) 
  })
#{ read MCMC stanfit output and return data.frame with headers: iter,year,stock,harvest,bmsy,fmsy 
.read_bdm <- function(.Object){
  
  years <- .Object@data$time
  niter <- .Object@nsamples
  iters <- 1:niter
  
  res <- .Object@trace$biomass
  dimnames(res) <- list(iter = iters, year = years)
  
  res <- reshape2::melt(res,value.name = 'stock')
  res <- data.frame(res,harvest = reshape2::melt(.Object@trace$harvest_rate)$value)
  
  bmsy <- .Object@trace$biomass_at_msy
  fmsy <- .Object@trace$harvest_rate_at_msy
  
  ref <- data.frame(iter = iters, bmsy = bmsy, fmsy = fmsy)
  
  res <- merge(res,ref,by = "iter")
  
  res$stock   <- res$stock/res$bmsy
  res$harvest <- res$harvest/res$fmsy
  
  if (length(.Object@run) > 0) {
    run <- .Object@run
    res <- data.frame(run = run,res)
  }

  return(res)
}
#}
#{ read MCMC stanfit output and projection (list)
.read_bdm_projection <- function(.Object,projection){
  
  years <- projection$time
  niter <- projection$nsamples
  iters <- 1:niter
  
  nscenario <- length(projection$scenarios)
  
  res <- list()
  for (s in 1:nscenario) {
  
	  res.sc <- projection$biomass[,,s]
	  dimnames(res.sc) <- list(iter = iters,year = years)
	  
	  res.sc <- reshape2::melt(res.sc,value.name = 'stock')
	  res.sc <- data.frame(res.sc,harvest = reshape2::melt(projection$harvest_rate[,,s])$value)
	  
	  bmsy <- .Object@trace$biomass_at_msy
	  fmsy <- .Object@trace$harvest_rate_at_msy
	  
	  ref <- data.frame(iter = iters,bmsy = bmsy,fmsy = fmsy)
	  
	  res.sc <- merge(res.sc,ref,by = "iter")
	  
	  res.sc$stock   <- res.sc$stock/res.sc$bmsy
	  res.sc$harvest <- res.sc$harvest/res.sc$fmsy
    
	  if (length(.Object@run) > 0) {
	    run <- .Object@run
	    res.sc <- data.frame(run = run,res.sc)
	  }
  
	  res[[s]] <- res.sc
  }
  
  names(res) <- projection$scenarios
  
  return(res)
}
#}
#{ formatting
.io_bdm <- function(res,prob,what,year,nwrms){
    
    if (is.null(year)) pts <- max(res$year)
      
    trks. = NULL
    pts.  = NULL
    smry. = NULL
    wrms. = NULL
    sims. = NULL
        
    if ("trks" %in% what) { 
      stock   = plyr::ddply(res,.(year),function(x) quantile(x$stock,    prob, na.rm=TRUE))
      harvest = plyr::ddply(res,.(year),function(x) quantile(x$harvest,  prob, na.rm=TRUE))
      trks. = data.frame(reshape2::melt(stock,id.vars = "year"),"harvest" = reshape2::melt(harvest,id.vars = "year")[,3])
      names(trks.)[c(2,3)] = c("Percentile","stock")}
    
    if ("pts" %in% what)
      pts. = res[res$year %in% pts,]
    
    if ("sims" %in% what)
      sims. = res
    
    if ("smry" %in% what) {
        
        kobe.quadrant <- function(stock, harvest) {
            
            b =   pmax(pmin(as.integer(stock),  1),0)
            f = 1-pmax(pmin(as.integer(harvest),1),0)
            p = f*b
            collapsed = (1-b)*(1-f)
            
            red    = collapsed
            green  = p
            yellow = 1-p-collapsed
            
            overFished  = 1-b
            overFishing = 1-f  
            
            data.frame(red=red,green=green,yellow=yellow,overFished=overFished,overFishing=overFishing)
        }
    
       smry. <- plyr::ddply(data.frame(res,kobe.quadrant(as.numeric(res$stock),as.numeric(res$harvest))),
                           .(year), function(x) data.frame(stock      = median(x$stock,       na.rm=TRUE),
                                                           harvest    = median(x$harvest,     na.rm=TRUE),
                                                           red        = mean(  x$red,         na.rm=TRUE),
                                                           yellow     = mean(  x$yellow,      na.rm=TRUE),
                                                           green      = mean(  x$green,       na.rm=TRUE),
                                                           overFished = mean(  x$overFished,  na.rm=TRUE),
                                                           overFishing= mean(  x$overFishing, na.rm=TRUE)))
    }
    
    if ("wrms" %in% what)
      wrms. = res[res$iter %in% sample(unique(res$iter),nwrms),c("iter","year","stock","harvest")]
    
    return(list(trks = trks., pts = pts., smry = smry., wrms = wrms., sims = sims.))

}
#}
#}}
#}}}
