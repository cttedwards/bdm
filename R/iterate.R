
#{{{ iteration of existing rdat object
setGeneric("iterate", function(x,iter, ...)
  standardGeneric("iterate"))
setMethod("iterate",signature(x="rdat",iter="numeric"),
          function(x,iter) new('rdat',rdat=x,iter=iter)
)
#}}}
