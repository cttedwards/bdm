#'
#' @title Extract Status estimates
#' 
#' @export
#{{{ refrence point accessor function
setGeneric("status", function(object, ...)
  standardGeneric("status"))
setMethod("status",signature("bdm"),
          function(object) {
            return(list(current_biomass=object@trace$current_biomass,
                        current_depletion=object@trace$current_depletion,
                        current_harvest_rate=object@trace$current_harvest_rate)
                   )
          }
)
#}}}
