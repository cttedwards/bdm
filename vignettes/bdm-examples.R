## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.path = 'fig/bdm-examples-', fig.width = 6, tidy = TRUE, tidy.opts = list(blank = TRUE, width.cutoff = 75), message = FALSE, warning = FALSE, collapse = TRUE, comment = "#>")

## ---- results='hide'-----------------------------------------------------
library(bdm)

## ----haknz-data, results='hide', fig.cap='Chatham rise hake data'--------
data(haknz)
dat <- bdmData(harvest = haknz$landings,index = cbind(haknz$survey, haknz$cpue), time = haknz$year, renormalise = TRUE)
plot(dat)

## ----hakrprior, results='hide', fig.cap='Prior for $r$ generated from life-history data for chatham rise hake.'----
library(lhm)

# initialise lhm data object for calculation of r with uncertainty
rdat <- lhm(ainf = 100, iter = 200)

# then life-history vectors can be assigned to each iteration
# with or without uncertainty
nmort(rdat)    <- list(mu = 0.18)
maturity(rdat) <- c(0.0,0.01,0.02,0.06,0.14,0.28,0.50,0.72,0.86,0.94,0.98,0.99,1.00)
size(rdat)     <- list(mu = list(Linf = 106.5, k = 0.229, t0 = 0.01))
mass(rdat)     <- list(mu = list(a = 1.88e-9, b = 3.305))
sr(rdat)       <- list(type = 'BH', mu = 0.90, cv = 0.10)

# calculate r prior and fit log-normal distribution
r <- rCalc(rdat)
plot(r)

## ---- results='hide'-----------------------------------------------------
mdl <- bdm()
mdl <- updatePrior(mdl, r)
mdl <- compiler(mdl)

## ------------------------------------------------------------------------
shape(dat) <- 0.4

## ---- results='hide'-----------------------------------------------------
mdl <- sampler(mdl, dat, run = 'haknz')

## ----haktrace, fig.cap='Traceplots for chatham rise hake fit.'-----------
traceplot(mdl)

## ----hakhist, fig.cap='Posterior histograms for chatham rise hake fit.'----
histplot(mdl,par = c('r','logK','q'))

## ----hakcumsum, fig.cap='Cumulative posterior denisty plot for chatham rise hake fit.'----
cumsumplot(mdl,par = c('r','logK'))

## ----hakdep, fig.cap='Estimated depletion for chatham rise hake.'--------
dynplot(mdl)

## ---- fig.show='hide'----------------------------------------------------
library(ggplot2)
gg <- dynplot(mdl, pars = 'surplus_production')
gg <- gg + ggtitle('Surplus Production')
print(gg)

## ----hakrefpts, results = "asis"-----------------------------------------
library(pander)
pandoc.table(data.frame(lapply(refpoints(mdl),median)))

## ---- fig.cap='Current status estimates for chatham rise hake.'----------
sta <- status(mdl)
dfr <- reshape2::melt(sta)
gg <- ggplot(dfr) + 
    geom_histogram(aes(x = value)) + 
    facet_wrap(~L1, scales = 'free_x')
print(gg)

## ------------------------------------------------------------------------
assmt <- as.kobe(mdl)

## ----kobeplot, results="hide",fig.cap='Kobe plot of current stock status.'----
library(kobe)
kobePhase(subset(assmt,year == max(assmt$year))) +
  geom_point(aes(stock,harvest), alpha = 0.1)

## ----kobesummary, results = "hide"---------------------------------------
assmt <- as.kobe(mdl, what = 'smry')
pandoc.table(assmt)

## ----hakstatus, results = "asis"-----------------------------------------
pandoc.table(data.frame(lapply(status(mdl),median)))

## ----hakproj, results = "asis"-------------------------------------------
# project forward under a variety of constant harvest rate scenarios
mdl.project <- project(mdl, harvest = c(0.01, 0.025, 0.05), time = 100)

# generate depletion results table
dep.project <- apply(mdl.project$depletion, 2:3, median)
pandoc.table(apply(mdl.project$depletion, 2:3, median)[nrow(dep.project),])

# display kobe plot
proj <- as.kobe(mdl, mdl.project)
kobePhase(subset(proj, year == max(proj$year))) +
  geom_point(aes(stock, harvest, col = projection_value))

