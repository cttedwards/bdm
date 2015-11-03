---
title: "Example applications of `bdm`"
author: "Charles T T Edwards (NIWA, Wellington, New Zealand)"
date: "2015-11-03"
output:
  rmarkdown::html_vignette:
    fig_caption: yes
    toc: yes
vignette: >
  %\VignetteIndexEntry{bdm-examples}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---



Some example applications of the `bdm` R-package are given here, based on data from fisheries in New Zealand.


```r
library(bdm)
```

# Chatham Rise Hake

The model is fitted to data from the chatham rise hake fishery in New Zealand, which consists of catches, a commerical abundance index and a survey index. The data are used to initialise an empirical data object (`bdmData`) that checks the data are in an appropriate format. See `?bdmData` for details.


```r
data(haknz)
dat <- bdmData(harvest = haknz$landings, index = cbind(haknz$survey, haknz$cpue), 
    time = haknz$year, renormalise = TRUE)
plot(dat)
```

![Chatham rise hake data](fig/bdm-examples-haknz-data-1.png) 

## Development of a prior for $r$
Life history data are available, allowing us to populate an object of the `lhm` class, which is obtained from the `lhm` (life-history module) package, and is used to store and manipulate life-history data. Monte-carlo samples are generated, and application of the `lhm::rCalc` function to this class of object calcuates values of $r$ for each iteration, producing an object of the `prior` class. The `prior` class contains a numeric vector with an additional slot for holding parameters of the associated distribution. Currenly only the log-normal distribution is supported, with paramters stored in `object@lognormal.par`.


```r
library(lhm)

# initialise lhm data object for calculation of r with uncertainty
rdat <- lhm(amax = 100, iter = 200)

# then life-history vectors can be assigned to each iteration with or
# without uncertainty
nmort(rdat) <- list(mu = 0.18)
maturity(rdat) <- c(0, 0.01, 0.02, 0.06, 0.14, 0.28, 0.5, 0.72, 0.86, 0.94, 
    0.98, 0.99, 1)
size(rdat) <- list(mu = list(Linf = 106.5, k = 0.229, t0 = 0.01))
mass(rdat) <- list(mu = list(a = 1.88e-09, b = 3.305))
sr(rdat) <- list(type = "BH", mu = 0.9, cv = 0.1)

# calculate r prior and fit log-normal distribution
r <- rCalc(rdat)
plot(r)
```

![Prior for $r$ generated from life-history data for chatham rise hake.](fig/bdm-examples-hakrprior-1.png) 

## Estimation of depletion
The model is initialised using the `bdm` command, with no arguments. Arguments can be supplied in the form or alternative model code, but by default initialisation loads a generalised surplus production model, which will be used for this example. Following initialisation of the model, the prior information on $r$, contained in the `prior` object, can be loaded using the `updatePrior` function, which takes as input the `bdm` and `prior` objects. This function updates the model code directly, following which it must be compiled.


```r
mdl <- bdm()
mdl <- updatePrior(mdl, r)
mdl <- compiler(mdl)
```

Note that it is also possible to update the priors in the model code by using a list argument to `updatePrior` (see `?updatePrior` for details).

The default generalised surplus production model allows the depletion at maxium sustainable yield (MSY) to be fixed by the user. The depletion at MSY ($\phi$) forms part of the model inputs encoded in the `bdmData` object and can be accessed or assigned using the `shape` function. For chatham rise hake we assume that MSY occurs at 40\% of the carrying capacity (i.e. $\phi=0.4$). 


```r
shape(dat) <- 0.4
```

The model can then be run using the `sampler` function, which makes use of the R-package `rstan` to implement an MCMC sampling routine.


```r
mdl <- sampler(mdl, dat, run = "haknz")
```

Trace, posterior histogram and cumulative density plots can be produced using the `traceplot`, `histplot` and `cumsumplot` functions.


```r
traceplot(mdl)
```

![Traceplots for chatham rise hake fit.](fig/bdm-examples-haktrace-1.png) 

```r
histplot(mdl, par = c("r", "logK", "q"))
```

![Posterior histograms for chatham rise hake fit.](fig/bdm-examples-hakhist-1.png) 

```r
cumsumplot(mdl, par = c("r", "logK"))
```

![Cumulative posterior denisty plot for chatham rise hake fit.](fig/bdm-examples-hakcumsum-1.png) 

## Model outputs

We can plot model outputs using the `dynplot` function, which by default plots the `depletion` trajectory over time. Currently it can also be used to visualise `biomass`, `surplus_production`, and the `harvest_rate`.


```r
dynplot(mdl)
```

![Estimated depletion for chatham rise hake.](fig/bdm-examples-hakdep-1.png) 

Plotting functions `traceplot`, `histplot` and `dynplot` return graphical objects from the `ggplot2` package and can be modified before they are printed. For example, to add a title to the plot of surplus production and change the y-axis one could type:


```r
library(ggplot2)
gg <- dynplot(mdl, pars = "surplus_production")
gg <- gg + ggtitle("Surplus Production")
print(gg)
```

We can use the function `refpoints` to extract the reference point information from the fitted model object as a `list` that contains `msy`, `depletion_at_msy` (which is equal to $\phi$), `biomass_at_msy` and `harvest_rate_at_msy`. These reference points are functions of $r$, $K$ and $\phi$, and therefore (with the exception of $\phi$ itself) have a posterior distribution. We can extract the median values and write them in a table using `pander::pandoc.table`: 


```r
library(pander)
pandoc.table(data.frame(lapply(refpoints(mdl), median)))
```


---------------------------------------------------------------
 msy   depletion_at_msy   biomass_at_msy   harvest_rate_at_msy 
----- ------------------ ---------------- ---------------------
1761         0.4              16421              0.1066        
---------------------------------------------------------------

Similarly, the function `status` can be used to access posterior distributions of `current_biomass`, `current_depletion` and `current_harvest_rate`, which correspond to the final assessment year, i.e. `dat$time[dat$T]`. We could easily plot these as histograms, using for example:


```r
sta <- status(mdl)
dfr <- reshape2::melt(sta)
gg <- ggplot(dfr) + geom_histogram(aes(x = value)) + facet_wrap(~L1, scales = "free_x")
print(gg)
```

![Current status estimates for chatham rise hake.](fig/bdm-examples-unnamed-chunk-7-1.png) 
Note that the depletion is given relative to $K$. This is a more robust estimate of status than status relative to $MSY$. Nevertheless we could extract the status relative to $MSY$ based reference points by using a combination of the `refpoints()` and `status()` functions.

## The `kobe` package

It is possible to produce kobe plots by using the `as.kobe` function, which creates a `data.frame` in the appropriate format for the `kobe` package. 


```r
assmt <- as.kobe(mdl)
```

The Kobe risk assessment framework has been developed by scientists at the International Commission for the Conservation of Atlantic Tunas, as a means of presenting status advice. The stock is assessed relative to $B_{MSY}$ and $H_{MSY}$. If $B < B_{MSY}$ then it is classified as overfished. If $H > H_{MSY}$ then overfishing is taking place. Status can be visualised as a kobe phase plot, which is produced using the `kobe::kobePhase` function.


```r
library(kobe)
kobePhase(subset(assmt, year == max(assmt$year))) + geom_point(aes(stock, harvest), 
    alpha = 0.1)
```

![Kobe plot of current stock status.](fig/bdm-examples-kobeplot-1.png) 

The `as.kobe` function can also be used to produce a summary of stock status over time; specifically the probability that the stock is in the green, red and yellow quadrants of the phase plane.


```r
assmt <- as.kobe(mdl, what = "smry")
pandoc.table(assmt)
```

The status estimates of the stock can be obtained in a similar manner to the reference points above.


```r
pandoc.table(data.frame(lapply(status(mdl), median)))
```


------------------------------------------------------------
 current_biomass   current_depletion   current_harvest_rate 
----------------- ------------------- ----------------------
      16980             0.4153               0.05595        
------------------------------------------------------------

## Projections

Based on these we can perform projections exploring alternative constant harvest rate options. The `project` function returns a `list` (see `?project`) which can be manipulated to summarise the results. For example:


```r
# project forward under a variety of constant harvest rate scenarios
mdl.project <- project(mdl, harvest = c(0.01, 0.025, 0.05), time = 100)

# generate depletion results table
dep.project <- apply(mdl.project$depletion, 2:3, median)
pandoc.table(apply(mdl.project$depletion, 2:3, median)[nrow(dep.project), ])
```


--------------------
  1      2      3   
------ ------ ------
0.9074 0.8024 0.6502
--------------------

```r

# display kobe plot
proj <- as.kobe(mdl, mdl.project)
kobePhase(subset(proj, year == max(proj$year))) + geom_point(aes(stock, harvest, 
    col = projection_value))
```

![plot of chunk hakproj](fig/bdm-examples-hakproj-1.png) 


