#' @title Initialize rdat object
#' 
#' @description Method to initialize rdat class object
#' 
#' @include rdat-class.R
#'
# initialisation function
setMethod("initialize","rdat",function(.Object, amax, iter, nmort, growth, mass, sr, maturity, rdat) {
    
    if (!missing(rdat)) {
        
        .Object@amax <- rdat@amax
        .Object@sr   <- rdat@sr
        
        if (!missing(iter)) {
            
            .Object@iter                    <- iter
            .Object@lhdat[['M']]            <- matrix(rep(rdat@lhdat[['M']],iter),rdat@amax,iter)
            .Object@lhdat[['h']]            <- matrix(rep(rdat@lhdat[['h']],iter),1,iter)
            .Object@lhdat[['survivorship']] <- matrix(rep(rdat@lhdat[['survivorship']],iter),rdat@amax,iter)
            .Object@lhdat[['size']]         <- matrix(rep(rdat@lhdat[['size']],iter),rdat@amax,iter)
            .Object@lhdat[['mass']]         <- matrix(rep(rdat@lhdat[['mass']],iter),rdat@amax,iter)
            .Object@lhdat[['maturity']]     <- matrix(rep(rdat@lhdat[['maturity']],iter),rdat@amax,iter)
            
        }
    } else {
        if (!missing(amax)) {
            
            .Object@amax <- amax
            
            if (!missing(iter)) {
                
                .Object@iter                    <- iter
                .Object@lhdat[['M']]            <- matrix(NA,amax,iter)
                .Object@lhdat[['h']]            <- matrix(NA,1,iter)
                .Object@lhdat[['survivorship']] <- matrix(NA,amax,iter)
                .Object@lhdat[['size']]         <- matrix(NA,amax,iter)
                .Object@lhdat[['mass']]         <- matrix(NA,amax,iter)
                .Object@lhdat[['maturity']]     <- matrix(0,amax,iter)
                
                if (!missing(nmort)) {
                    
                    M.mu <- nmort$mu$M
                    if (length(M.mu)<amax)
                        M.mu[(length(M.mu)+1):amax] <- rep(M.mu[length(M.mu)],amax-length(M.mu))
                    
                    if (!is.null(nmort$cv$M)) {
                        M.sd <- sqrt(log(1+nmort$cv$M^2)) 
                        for(i in 1:iter) 
                            .Object@lhdat[['M']][,i]  <- M.mu * rlnorm(1,-M.sd^2/2,M.sd)
                        
                    } else {
                        .Object@lhdat[['M']] <- apply(.Object@lhdat[['M']],2,function(x) M.mu)
                    }
                    
                    # recalculate survivorship
                    for(i in 1:iter)
                        for(a in 1:amax)
                            .Object@lhdat[['survivorship']][a,i] <- exp(-sum(.Object@lhdat[['M']][1:a,i]))
                        
                }
                
                if (!missing(sr)) {
                    
                    if (!is.null(sr$type)) 
                        .Object@sr <- sr$type
                    else stop('must specify type of stock-recruitment function (either BH or R)\n')
                    
                    if (!is.null(sr$cv$h)) {
                        
                        if (.Object@sr == 'BH') hmax <- 1
                        if (.Object@sr == 'R')  hmax <- 165
                        
                        bdist.mu <- function(alpha,beta) alpha/(alpha+beta)
                        bdist.sd <- function(alpha,beta) sqrt(alpha*beta / ((alpha+beta)^2 * (alpha+beta+1)))
                        
                        sr.mu <- function(alpha,beta) 0.2 + bdist.mu(alpha,beta) * (hmax - 0.2)
                        sr.cv <- function(alpha,beta) bdist.sd(alpha,beta) * (hmax - 0.2) / sr.mu(alpha,beta)
                        
                        obj <- function(par) {
                            (sr.mu(par[1],par[2]) - sr$mu$h)^2 + (sr.cv(par[1],par[2]) - sr$cv$h)^2 
                        }
                        
                        par.opt <- optim(c(3,2),obj)$par
                        .Object@lhdat[['h']][] <- 0.2 + rbeta(iter,par.opt[1],par.opt[2]) * (hmax - 0.2)
                        
                    } else {
                        if (!is.null(sr$mu$h))
                            .Object@lhdat[['h']][] <- sr$mu$h
                    }
                }
                
                if (!missing(growth)) {
                    
                    Linf.mu <- growth$mu$Linf
                    k.mu    <- growth$mu$k
                    t0.mu   <- growth$mu$t0
                    
                    if (!is.null(growth$cv$Linf)) {
                        Linf.sd <- sqrt(log(1+growth$cv$Linf^2))
                        Linf <- Linf.mu * rlnorm(iter,-Linf.sd^2/2,Linf.sd) 
                    } else Linf <- rep(Linf.mu,iter)
                    
                    if (!is.null(growth$cv$k)) {
                        k.sd <- sqrt(log(1+growth$cv$k^2))
                        k <- k.mu * rlnorm(iter,-k.sd^2/2,k.sd)
                    } else k <- rep(k.mu,iter)
                    
                    if (!is.null(growth$cv$t0)) {
                        t0.sd <- sqrt(log(1+growth$cv$t0^2))
                        t0 <- t0.mu * rlnorm(iter,-t0.sd^2/2,t0.sd)
                    } else t0 <- rep(t0.mu,iter)
                    
                    for(i in 1:iter)
                        for(a in 1:amax)
                            .Object@lhdat[['size']][a,i] <- max(Linf[i] * (1 - exp(-k[i]*(a-t0[i]))),0)
                    
                    if (!missing(mass)) {
                        
                        a.mu <- mass$mu$a
                        b.mu <- mass$mu$b
                        
                        if (!is.null(mass$cv$a)) {
                            a.sd <- sqrt(log(1+mass$cv$a^2))
                            aa <- a.mu * rlnorm(iter,-a.sd^2/2,a.sd) 
                        } else aa <- rep(a.mu,iter)
                        
                        if (!is.null(mass$cv$b)) {
                            b.sd <- sqrt(log(1+mass$cv$b^2))
                            bb <- b.mu * rlnorm(iter,-b.sd^2/2,b.sd)
                        } else bb <- rep(b.mu,iter)
                        
                        for(i in 1:iter)
                            for(a in 1:amax)
                                .Object@lhdat[['mass']][a,i] <- aa[i] * .Object@lhdat[['size']][a,i]^bb[i]
                    }
                }
                
                if (!missing(maturity)) {
                    
                    acrit.mu <- maturity$mu$acrit
                    delta.mu <- maturity$mu$delta
                    
                    if (!is.null(delta.mu)) {
                        
                        if (!is.null(maturity$cv$acrit)) {
                            acrit.sd <- sqrt(log(1+maturity$cv$acrit^2))
                            acrit <- acrit.mu * rlnorm(iter,-acrit.sd^2/2,acrit.sd)
                        } else acrit <- rep(acrit.mu,iter)
                        
                        if (!is.null(maturity$cv$delta)) {
                            delta.sd <- sqrt(log(1+maturity$cv$delta^2))
                            delta <- delta.mu * rlnorm(iter,-delta.sd^2/2,delta.sd)
                        } else delta <- rep(delta.mu,iter)
                        
                        for(i in 1:iter)
                            for(a in 1:amax)
                                .Object@lhdat[['maturity']][a,i] <- 1/(1+exp((acrit[i]-a)/delta[i]))
                    } else {
                        acrit.mu <- as.integer(acrit.mu)
                        if (acrit.mu<=amax)
                            .Object@lhdat[['maturity']][acrit.mu:amax,] <- 1
                    }
                }
            }
        }
    }
    
    .Object
})
