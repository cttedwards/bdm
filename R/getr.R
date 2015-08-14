getr <- function(.Object) {
    
    # extract r from model_code
    
    m <- regexpr('r.?~.?lognormal\\(.+?\\)',.Object@model_code)
    x <- regmatches(.Object@model_code,m)
    
    m1 <- regexpr('\\(.+?\\,',x)
    m1 <- m1 + 1
    attributes(m1)$match.length <- attributes(m1)$match.length - 2
    x1 <- regmatches(x,m1)
    
    m2 <- regexpr('\\,.+?\\)',x)
    m2 <- m2 + 1
    attributes(m2)$match.length <- attributes(m2)$match.length - 2
    x2 <- regmatches(x,m2)
    
    mu <- as.numeric(x1)
    sigma <- as.numeric(x2)
    
    init.r <- exp(mu+sigma^2/2)
    
    init.r
}
