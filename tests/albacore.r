
# INDIAN OCEAN ALBACORE

##############
# START HERE #
##############

rm(list=ls())

# install and load bdm package
#install.packages("bdm",contriburl="file://niwa.local/Groups/wellington/niwafisheries/R/",type="win.binary",repos=NULL)
install.packages("C:/PROJECTS/SOFTWARE/OpenSource/bdm_1.0.zip", repos = NULL)
install.packages("C:/PROJECTS/SOFTWARE/OpenSource/kobe_1.3.2.zip", repos = NULL)

library(bdm)
library(kobe)
library(ggplot2)

# load data
data(albacore)
dat <- edat(harvest=albacore$catch,index=albacore$cpue,year=rownames(albacore))

# initialise object
mdl <- bdm()

# update data and priors using values from Meyer and Millar (CJFAS 1999)
# error terms
sigmao(dat) <- sqrt(0.0086/(1.71 - 1))
sigmap(dat) <- sqrt(0.0102/(3.79 - 1))
# intrinsic growth rate
mdl <- update_bdm(mdl,list(a=-1.38,b=0.51,par='r'))

# inspect code
mdl

# compile
mdl <- compile_bdm(mdl)

# mcmc fit
mdl <- fit(mdl,dat,iter=2000,thin=10,run='run_1')
traceplot(mdl,pars=c('r','logK'))
histplot(mdl,pars=c('r','logK'))

# kobe plots from vignette

# extract data from bdm object
assmt <- kobeBdm(mdl,what=c('sims','trks','pts'))
head(assmt[['sims']])

# plot stock trajectories by iteration
ggplot(assmt[['sims']]) +
  geom_hline(aes(yintercept=1),col="red",size=2) +
  geom_line( aes(year,stock,group=iter,col=iter)) +
  theme(legend.position="none")

# plot stock and harvest trajectories and percentiles
ggplot(assmt[['trks']]) +
  geom_line(aes(year,stock, linetype=Percentile),col="blue") +
  geom_line(aes(year,harvest,linetype=Percentile),col= "red") +
  scale_linetype_manual(values=c(2,1,2)) +
  coord_cartesian(ylim=c(0,3))

# kobe plot of terminal year
kp <- kobePhase(assmt[['pts']]) + geom_point(aes(stock,harvest))
print(kp)

# add contours
kp + geom_path(aes(x,y,group=level),colour="blue",data=kobeProb(assmt[['pts']]$stock,assmt[['pts']]$harvest,prob=c(0.75,.5,.25)))

# show stock trajectory across kobe plot
kobePhase(ylim=c(0,max(2,assmt[['pts']]$harvest))) + 
  geom_path(aes(stock,harvest),data=subset(assmt[['trks']],Percentile=='50%'),col="blue",size=1) + 
  geom_path(aes(x,y,group=level),colour="blue",data=kobeProb(assmt[['pts']]$stock,assmt[['pts']]$harvest,prob=c(0.75,.5,.25))) +
  geom_point(aes(stock,harvest), data=subset(assmt[['trks']],Percentile=='50%' & year==unique(assmt[['pts']]$year)),col="blue",size=4)

#######
# END #
#######

