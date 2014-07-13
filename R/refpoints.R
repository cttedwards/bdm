
#{{{ refrence point accessor function
setGeneric("refpoints", function(object, ...)
  standardGeneric("refpoints"))
setMethod("refpoints",signature("bdm"),
          function(object) {
            return(list(msy=object@trace$msy,
                        depletion_at_msy=object@trace$dmsy,
                        biomass_at_msy=object@trace$biomass_at_msy,
                        harvest_rate_at_msy=object@trace$harvest_rate_at_msy)
                   )
          }
)
#}}}
