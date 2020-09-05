## ----include=FALSE-------------------------------------------------------
library(knitr)
library(ggplot2)
library(bdm)
opts_chunk$set(fig.path='fig/bdm-introduction-',
               fig.align='center',
               size='footnotesize',
               fig.width=12,
               fig.height=6,
               out.width='0.8\\textwidth',
               tidy=TRUE,
               tidy.opts=list(keep.blank.line=TRUE, width.cutoff=80),
               message=FALSE,
               warning=FALSE)

## ----include=FALSE,cache=FALSE-------------------------------------------
r <- 0.1
K <- 100
b <- seq(0,K,1e-3)
rlim <- function(b) dbdt(b) * (1/b)

# empty dataframe
dfr <- data.frame() 

# Schaefer model
#dbdt <- function(b) r * b * (1 - b/K)
#dfr <- rbind(dfr,data.frame(biomass=b,
#                  production=dbdt(b),
#                  growth=rlim(b),
#                  model='Schaefer'))
# P-T model
#dbdt <- function(b) r/p * b * (1 - (b/K)^p)
#p <- 0.2
#dfr <- rbind(dfr,data.frame(biomass=b,
#                            production=dbdt(b),
#                            growth=rlim(b),
#                            model='Pella-Tomlinson'))

# fletcher model
dbdt <- function(b) g * m * b/K * (1 - (b/K)^(n-1))
n <- 1.1881
bmsy <- (1/n)^(1/(n-1)) * K
h <- 2*bmsy
m <- r*h/4
g <- (n^(n/(n-1)))/(n-1)
dfr <- rbind(dfr,data.frame(biomass=b,
                            production=dbdt(b),
                            growth=rlim(b),
                            model='Fletcher'))

# Fletcher-Schaefer model
dbdt <- function(b) {
  bf <- b[which(b>bmsy)]
  bs <- b[which(b<=bmsy)]
  c(r * bs * (1 - bs/h),g * m * bf/K * (1 - (bf/K)^(n-1)))
}
dfr <- rbind(dfr,data.frame(biomass=b,
                            production=dbdt(b),
                            growth=rlim(b),
                            model='Fletcher-Schaefer'))
# Beverton-Holt model
dbdt <- function(b) alpha * b / (1 + beta * b) - M * b
phi <- 0.4
M <- 0.1
objective <- function(alpha) phi - (sqrt(alpha/M)-1)/((alpha/M)-1)
alpha <- uniroot(objective,interval=c(0,10))$root
beta  <- 1/K * (alpha/M - 1) 
dfr <- rbind(dfr,data.frame(biomass=b,
                            production=dbdt(b),
                            growth=rlim(b),
                            model='Beverton-Holt'))

## ----productionfunctions,echo=FALSE,fig.cap='Production functions for different models assuming $\\phi=0.4$'----
ggplot(dfr) + geom_line(aes(x=biomass,y=production,col=model)) + labs(x='Biomass',y='Surplus Production',col='Model')

## ----intrinsicgrowth,echo=FALSE,fig.cap='Growth rate per capita as a function of biomass'----
ggplot(dfr) + geom_line(aes(x=biomass,y=growth,col=model)) + labs(x='Biomass',y='Production per unit biomass',col='Model')

