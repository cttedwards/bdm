---
title: "Example applications of `bdm`"
author: "Charles T T Edwards (NIWA, Wellington, New Zealand)"
date: "2015-09-08"
output:
  rmarkdown::html_vignette:
    fig_caption: yes
    toc: yes
vignette: >
  %\VignetteIndexEntry{bdm-examples}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---



Example applications of the `bdm` R-package are given here, based on data from fisheries in New Zealand.


```r
library(bdm)
```

# Chatham Rise Hake

The model is fitted to data from the chatham rise hake fishery in New Zealand, which consists of catches, a commerical abundance index and a survey index. The data are used to initialise an empirical data object (`edat`) that, by default, renormalises the indices to a arithmetic mean of one. See `?edat` for details.


```r
data(hakcr)
dat <- edat(harvest = hakcr$catch, index = cbind(hakcr$survey, hakcr$cpue), 
    time = rownames(hakcr), renormalise = TRUE)
plot(dat)
```

![Chatham rise hake data](fig/bdm-examples-hakcr-data-1.png) 

## Development of a prior for $r$
Life history data are available, allowing us to populate an object of the `rdat` class. Monte-carlo samples are generated, and application of the `rcalc` function to this class of object calcuates values of $r$ for each iteration, producing an object of the `rprior` class. Finally, there is a `fit` function used to parameterise a log-normal distribution, and thereby to create an informative prior for the intrinsic growth rate. The estimated parameter values can be found in `object@lognormal.par`.


```r
# initialise lh data object for calculation of r with uncertainty
lhdat <- rdat(amax = 100, iter = 200)

# then life-history vectors can be assigned to each iteration with or
# without uncertainty
nmort(lhdat) <- list(mu = 0.18)
maturity(lhdat) <- c(0, 0.01, 0.02, 0.06, 0.14, 0.28, 0.5, 0.72, 0.86, 0.94, 
    0.98, 0.99, 1)
size(lhdat) <- list(mu = list(Linf = 106.5, k = 0.229, t0 = 0.01))
mass(lhdat) <- list(mu = list(a = 1.88e-09, b = 3.305))
sr(lhdat) <- list(type = "BH", mu = 0.9, cv = 0.1)

# calculate r prior and fit log-normal distribution
r <- rcalc(lhdat)
plot(r)
```

![Prior for $r$ generated from life-history data for chatham rise hake.](fig/bdm-examples-hakrprior-1.png) 

## Estimation of depletion
The model is initialised using the `bdm` command, with no arguments. Arguments can be supplied in the form or alternative model code, but by default initialisation loads a generalised surplus production model, which will be used for this example. Following initialisation of the model, the prior information on $r$, contained in the `rprior` object, can be loaded using the `updatePrior` function, which takes as input the `bdm` and `rprior` objects. This function updates the model code directly, following which it must be compiled.


```r
mdl <- bdm()
mdl <- updatePrior(mdl, r)
mdl <- compile(mdl)
```

The default generalised surplus production model allows the depletion at maxium sustainable yield (MSY) to be fixed by the user. The depletion at MSY ($\phi$) forms part of the model inputs encoded in the `edat` object and can be accessed or assigned using the `shape` function. For chatham rise hake we assume that MSY occurs at 40\% of the carrying capacity (i.e. $\phi=0.4$). 


```r
shape(dat) <- 0.4
```

The model can then be run using the `fit` function, which makes use of the R-package `rstan` to implement an MCMC fit.




































