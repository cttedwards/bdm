#'
#' @title Process error function
#' 
#' @export
#{{{ accessor function
setGeneric("sigmao", function(x, ...)
  standardGeneric("sigmao"))
setMethod("sigmao",signature(x="edat"),
          function(x) return(x[['sigmao']])
)
#}}}

#{{{ assignment function
setGeneric("sigmao<-", function(x,i,j, ...,value) standardGeneric("sigmao<-"))
#{{ numeric
setMethod("sigmao<-",
          signature(x = "edat", value = "numeric"),
          function(x,i,j, ...,value) {
            
              sigmao.dim    <- c(x$T, x$I)
              sigmao.length <- x$T * x$I
              
              if(length(value) < sigmao.length) {
                  if(sigmao.length %% length(value) != 0)
                      stop('dimensions for sigmao do not match dimensions for indices\n')
                  value <- matrix(value, sigmao.dim[1], sigmao.dim[2], byrow = TRUE)
              }
            x$sigmao <- structure(value, .Dim = sigmao.dim)   
            
            .Object$sigmao[.Object$index == -1] <- -1
            
            x
          }
)
#}}
#{{ numeric
setMethod("sigmao<-",
          signature(x="edat",value="matrix"),
          function(x,i,j, ...,value) {
              
              if(any(dim(value) != dim(x$index))) {
                warning('dimensions do not match index dimensions\n')
              } else {
                x$sigmao <- structure(value, .Dim = dim(x$index))        
              }
              
              x
          }
)
#}}
#}}}
