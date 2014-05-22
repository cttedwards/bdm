
# test log-normal error probability assumptions

# assume both process and observation error
# share the same (large) sigma
sigma <- 0.4
sigma2 <- sigma^2

# large number of years (iterations)
nyr <- 1e4

# dynamics
xdevs <- rlnorm(nyr,log(1)-sigma2/2,sigma)
x <- (1-sin(seq(0,pi/3,length=nyr))) * xdevs
mean(xdevs) # == 1
min(x) # > 0

# catchability
q <- 0.9

# simulate observations
I <- rlnorm(length(x),log(q*x)-sigma2/2,sigma)
mean(I - q*x) # == 0
min(I) # > 0
qqnorm(log(I/(q*x))) # normal

# plot subset
loc <- sort(sample(nyr,200))
plot(x[loc],type='l')
points((I/q)[loc],col=2)


