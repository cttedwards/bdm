
#{{{ accessor function
setGeneric("survivorship", function(x, ...)
  standardGeneric("survivorship"))
setMethod("survivorship",signature(x="rdat"),
          function(x) return(x@lhdat$survivorship)
)
#}}}
