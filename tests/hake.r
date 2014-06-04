
# NEW ZEALAND HAKE (Chatham rise)

##############
# START HERE #
##############

rm(list=ls())

# install and load bdm package
#install.packages("bdm",contriburl="file://niwa.local/Groups/wellington/niwafisheries/R/",type="win.binary",repos=NULL)
install.packages("C:/PROJECTS/SOFTWARE/OpenSource/bdm_1.0.zip", repos = NULL)

library(bdm)

# load data
data(hake)

# create empirical data.frame
dat <- edat(catch=hake$catch,index=cbind(hake$survey,hake$cpue),year=rownames(hake))

# inspect r prior
hist(r)

# get stan code and compile
mdl <- bdm()
mdl <- update_bdm(mdl,list(a=r@lognormal.par[['E[log(x)]']],b=r@lognormal.par[['SD[log(x)]']],par='r'))
mdl <- compile_bdm(mdl)

# inspect code
mdl

# mcmc fit
mdl <- fit(mdl,dat,iter=20000,thin=10)
traceplot(mdl)
histplot(mdl)

#######
# END #
#######



