
setClass("rdat.iter",representation(amax="numeric",sr="character",lhdat="list"))
setMethod("initialize","rdat.iter",function(.Object,amax,sr, ...) {
  
  if(!missing(amax)) {
    
    .Object@amax <- amax
    .Object@lhdat[['survivorship']] <- vector('numeric',amax)
    .Object@lhdat[['M']]            <- vector('numeric',amax)
    .Object@lhdat[['size']]         <- vector('numeric',amax)
    .Object@lhdat[['mass']]         <- vector('numeric',amax)
    .Object@lhdat[['maturity']]     <- vector('numeric',amax)
    
  }
  
  if(!missing(sr)) {
    .Object@sr           <- sr[['type']]
    .Object@lhdat[['h']] <- numeric(1)
  }
  
  .Object
  
})
