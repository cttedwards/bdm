
# test log-normal error probability assumptions

rm(list=ls())

# process and observation error
sigmap  <- 0.2
sigmap2 <- sigmap^2

sigmao  <- 0.4
sigmao2 <- sigmao^2

# large number of years (iterations)
nyr <- 1e4

# dynamics
xdevs <- rlnorm(nyr,log(1)-sigmap2/2,sigmap)
x <- (1-sin(seq(0,pi/3,length=nyr))) * xdevs
mean(xdevs) # E[xdevs] == 1;
min(x) # > 0

# catchability
q <- 1.0

# simulate observations
I <- rlnorm(length(x),log(q*x)-sigmao2/2,sigmao)
mean(I - q*x) # E[I] = E[q.B] => E[I] - [q.B] == 0
min(I) # > 0
qqnorm(log(I/(q*x))) # I/(q.B) is log-normal, therefore log(I/(q.B)) is normal

# check analytical derivation of q
# mle
loglx <- expression(-c-((log(x) - log(q * B) + sigma^2/2)^2)/(2*sigma^2))
D(loglx,'q')

(qhat <- exp(mean(log(I/x)) + sigmao2/2))

mean(I - qhat*x) # == 0

# mpd with uniform prior on log(q)
loglx <- expression(-c-((log(x) - log(q * B) + sigma^2/2)^2)/(2*sigma^2) + log(1/q))
D(loglx,'q')

(qhat <- exp(mean(log(I/x)) - sigmao2/2))

mean(I - qhat*x) # != 0

# plot subset
loc <- sort(sample(nyr,200))
plot(x[loc],type='l')
points((I/q)[loc],col=2)


