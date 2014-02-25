
# ESTIMATE INTRINSIC GROWTH RATE (r) FROM LIFE HISTORY PARAMETERS
# FOR INPUT INTO BDM
  
library(bdm)

############
# BLUENOSE #
############

# initialise lh data object for calculation of r
dat <- new('rdat',amax=60,iter=1)

# to calculate monte-carlo iterations first either 
# re-initialise 'rdat' object using more iterations or 
# iterate existing object:
dat <- iterate(dat,iter=200)

# then life-history vectors can be assigned to each iteration
# with or without uncertainty
nmort(dat)    <- list(mu=list(M=0.1))
size(dat)     <- list(mu=list(Linf=92.5,k=0.071,t0=-0.5),cv=list(Linf=0.1,k=0.2,t0=0.5))
mass(dat)     <- list(mu=list(a=9.63e-6,b=3.173))
maturity(dat) <- list(mu=list(acrit=17))
sr(dat)       <- list(type='BH',mu=list(h=0.9))

# we can then calculate an rprior distribution
rcalc(dat)

# it is also possible to perform data-initialisation 
# procedures in one go.

# with a single iteration:

dat <- new('rdat',
           amax=60,
           iter=1,
           nmort = list(mu=list(M=0.1)),
           growth=list(mu=list(Linf=92.5,k=0.071,t0=-0.5)),
           mass=list(mu=list(a=9.63e-6,b=3.173)),
           maturity=list(mu=list(acrit=17)),
           sr=list(type='BH',mu=list(h=0.9))
)

# or with 200 iterations and uncertainty around 
# all the parameter inputs

dat <- new('rdat',
            amax=60,
            iter=200,
            nmort = list(mu=list(M=0.08),cv=list(M=0.25)),
            growth=list(mu=list(Linf=92.5,k=0.071,t0=-0.5),cv=list(Linf=0.1,k=0.2,t0=0.5)),
            mass=list(mu=list(a=9.63e-6,b=3.173),cv=list(a=0.05,b=0.05)),
            maturity=list(mu=list(acrit=17,delta=3.4),cv=list(acrit=0.01,delta=0.01)),
            sr=list(type='BH',mu=list(h=0.75),cv=list(h=0.1))
            )

# calculate r prior distribution
hist(rr <- rcalc(dat))
mean(rr)


#################
# ORANGE ROUGHY #
#################

# lh data for calculation of r
dat <- new('rdat',
           amax=130,
           iter=1,
           nmort=list(mu=list(M=0.045)),
           growth=list(mu=list(Linf=37.78,k=0.059,t0=-0.491)),
           mass=list(mu=list(a=8.0e-2,b=2.75)),
           sr=list(type='BH',mu=list(h=0.75)),
           maturity=list(mu=list(acrit=35.67,delta=1.55))
           )

# calculate r
rcalc(dat)

##############
# BLACK OREO #
##############

# lh data for calculation of r
dat <- new('rdat',
           amax=70,
           iter=1,
           nmort=list(mu=list(M=0.044)),
           growth=list(mu=list(Linf=38.2,k=0.05,t0=-0.4)),
           mass=list(mu=list(a=7.8e-2,b=3.27)),
           sr=list(type='BH',mu=list(h=0.75)),
           maturity=list(mu=list(acrit=37.7,delta=0.46))
)

# calculate r
rcalc(dat)


#########
# HAKE  #
#########

# generate lh data directly
dat <- new('rdat',
            amax=100,
            iter=1,  
            nmort=list(mu=list(M=0.18)),
            growth=list(mu=list(Linf=106.5,k=0.229,t0=0.01)), 
            mass=list(mu=list(a=1.7e-9,b=3.328)),
            sr=list(type='BH',mu=list(h=0.9)),
            maturity=list(mu=list(acrit=8))
)

# calculate r
rcalc(dat)

# generate a parametric log-normal prior with assumed cv=0.2
n <- 1000
r <- new('rprior',n)

cv <- 0.2
sd <- sqrt(log(1+cv^2))

r[] <- rlnorm(n,log(rcalc(dat))-sd^2/2,sd)
hist(r)






