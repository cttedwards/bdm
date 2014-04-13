
# INDIAN OCEAN ALBACORE

##############
# START HERE #
##############

rm(list=ls())

# install and load bdm package
install.packages("bdm",contriburl="file://niwa.local/Groups/wellington/niwafisheries/R/",type="win.binary",repos=NULL)

library(bdm)
help(package='bdm')

# load data
data(albacore)
dat <- edat(catch=albacore$catch,index=albacore$cpue)

# initialise object
mdl <- bdm()

# update priors in model from Meyer and Millar (CJFAS 1999)
mdl <- update_bdm(mdl,list(a=-1.38,b=0.51,par='r'))
mdl <- update_bdm(mdl,list(a=1.7,b=0.01,par='sigRsq'))
mdl <- update_bdm(mdl,list(a=3.8,b=0.01,par='sigQsq'))

# inspect code
mdl

# compile
mdl <- compile_bdm(mdl)

# run model
mdl <- fit(mdl,dat,warmup=10000,iter=40000,thin=10,chains=4)

# inspect default trace and histogram outputs
plot(mdl)
# inspect derived-parameters
plot(mdl,par=c('current_depletion','current_surplus_production'))

#################################
# PLOT MODEL INPUTS AND OUTPUTS #
#################################

library(ggplot2)
library(reshape2)

# plot data

dfr <- data.frame(dat$index,dat$catch,as.numeric(row.names(albacore)))
colnames(dfr) <- c('index','catch','time')
dfr[dfr<0] <- NA

pdf(file='albacore_data.pdf',width=14)
par(mar=c(5,5,1,5))
barplot(dfr$catch,ylim=range(0,dfr$catch*1.1),ylab='Catch (\'000 tonnes)',xlab='Year',cex.lab=2)
par(new=TRUE); plot(index~time,dfr,col=1,ylim=range(0,dfr$index,na.rm=T),type='b',yaxt='n',ylab='',xlab='',pch=19,lwd=2)
axis(4,)
mtext('Abundance (kg/100 hooks)',4,line=3,cex=2)
dev.off()


# predicted vs observed index values

predicted_index <- mdl@trace$predicted_index 

dfr <- data.frame()
for(i in 1:dim(predicted_index)[3])
  dfr <- rbind(dfr,data.frame(data=mdl@data$index[,i],
                              med=apply(predicted_index[,,i],2,quantile,0.50),
                              upp=apply(predicted_index[,,i],2,quantile,0.95),
                              low=apply(predicted_index[,,i],2,quantile,0.05),
                              index=as.factor(i)))
dfr <- subset(dfr,data>0)

pdf(file='albacore_fit1.pdf')
ggplot(dfr,aes(data,med,col=index)) + 
  geom_point() + 
  geom_errorbar(aes(ymax=upp,ymin=low),width=0.05) + 
  geom_abline(intercept=0,slope=1) +
  stat_smooth(method="lm",se=FALSE,linetype='longdash',fullrange=TRUE) +
  theme(axis.text=element_text(size=15),axis.title=element_text(size=20),legend.title=element_text(size=15),legend.text=element_text(size=15)) +
  labs(x='Observation',y='Predicted Value',col='Index')
dev.off()

# predicted biomass and index/q values

tmpq <- lapply(apply(mdl@trace$q,2,function(x) list(x)),unlist)
tmp <- list()
for(i in 1:length(tmpq)) {
  
  tmp[[i]] <- sapply(mdl@data$index[,i],function(x) { x[x<0] <- NA; x / tmpq[[i]] }) 

}
tmp <- lapply(tmp,function(x) apply(x,2,median))

dfr <- data.frame()
for(i in 1:length(tmp))
  dfr <- rbind(dfr,data.frame(data=tmp[[i]],
                              med=apply(mdl@trace$x,2,quantile,0.50),
                              upp=apply(mdl@trace$x,2,quantile,0.95),
                              low=apply(mdl@trace$x,2,quantile,0.05),
                              year=as.numeric(row.names(albacore)),
                              index=as.factor(i)))

pdf(file='albacore_fit2.pdf',width=12)
ggplot(dfr,aes(x=year)) +
  geom_point(aes(y=data,col=index)) + 
  geom_line(aes(y=med)) +
  geom_line(aes(y=upp)) +
  geom_line(aes(y=low)) +
  ylim(0,1.5) +
  theme(axis.text=element_text(size=15),axis.title=element_text(size=20),strip.text=element_text(size=15)) +
  labs(x='Time',y='Biomass')
dev.off()


# biomass, harvest rate and surplus production

x <- mdl@trace$x
dimnames(x) <- list(iter = 1:dim(x)[1],year=as.numeric(row.names(albacore)))
dfr <- melt(x,value.name='biomass')
dfr <- cbind(dfr,sp=melt(mdl@trace$surplus_production)$value)
dfr <- cbind(dfr,hr=melt(mdl@trace$harvest_rate)$value)

pdf(file='albacore_predict.pdf',width=12)
ggplot(dfr,aes(x=year)) + 
  stat_summary(aes(y=hr),fun.y=function(x) median(x),geom='bar',fill='blue',alpha=0.5) +
  stat_summary(aes(y=sp),fun.ymin=function(x) quantile(x,0.05),fun.ymax=function(x) quantile(x,0.95),geom='ribbon',alpha=0.5,fill='red') +
  stat_summary(aes(y=sp),fun.y=function(x) median(x),geom='line',lwd=1,col='red') +
  stat_summary(aes(y=biomass),fun.ymin=function(x) quantile(x,0.05),fun.ymax=function(x) quantile(x,0.95),geom='ribbon',alpha=0.3) +
  stat_summary(aes(y=biomass),fun.y=function(x) median(x),geom='line',lwd=1.5) +
  theme(axis.text=element_text(size=15),axis.title=element_text(size=20),legend.title=element_text(size=15),legend.text=element_text(size=15)) +
  labs(x='Year',y='Predicted Value')
dev.off()

# current depletion

dfr <- melt(mdl@trace$current_depletion)
pdf(file='albacore_depletion.pdf')
ggplot(dfr,aes(value)) + 
  geom_histogram(aes(y=..density..),fill='white') +
  geom_density(fill='darkgreen',alpha=0.1) + xlim(0,1) +
  theme(axis.text.x=element_text(size=15),axis.title.x=element_text(size=20),axis.text.y=element_text(size=0)) +
  geom_vline(xintercept=median(dfr$value),col='darkgreen',lwd=1) +
  labs(x='Current Depletion',y='')
dev.off()

#######
# END #
#######

