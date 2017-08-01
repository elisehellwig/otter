datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rethinking)
rstan_options(auto_write = TRUE)

otr <- read.csv(file.path(datapath, 'otterclean.csv'))
otr$cpop <- scale(otr$pop)
attributes(otr$cpop) <- NULL

#########################################################

mod1 <- map(
    alist(pop ~ dnorm(mu, sigma),
          mu <- a + by*year,
          a ~ dnorm(0, 50),
          by ~ dnorm(0, 1),
          sigma ~ dunif(0, 100)
    ), data=otr, start=alist(a=4, by=0.3, sigma=2.12))


otr$regionid <- coerce_index(otr$region)

mod2 <- map(
    alist(pop ~ dnorm(mu, sigma),
          mu <- a + by[regionid]*year,
          a ~ dnorm(4, 50),
          by[regionid] ~ dnorm(0.3, 1),
          sigma ~ dunif(0, 100)
    ), data=otr, start=alist(a=4, by=0.3, sigma=2.12))


mod3 <- map2stan(
    alist(pop ~ dnorm(mu, sigma),
          mu <- a + by[regionid]*year,
          by[regionid] ~ dnorm(b, sigma),
          a ~ dnorm(4, 10),
          b ~ dnorm(0, 10),
          sigma ~ dunif(0,20)
    ), data=otr, iter=16000, warmup=8000, chains=4, control = list(adapt_delta = 0.999),
    cores=3)



mod4 <- map2stan(
    alist(pop ~ dnorm(mu, sigma),
          mu <- a + b + b_loc[location]*year,
          b_loc[location] ~ dnorm(0, sigma_loc),
          a ~ dnorm(4, 10),
          b ~ dnorm(0, 5),
          sigma ~ dunif(0, 100),
          sigma_loc ~ cauchy(0,1)
    ), data=otr, warmup=8000, iter=16000,  chains=4, cores = 3,
    control = list(adapt_delta = 0.8, max_treedepth = 10))

post4 <- extract.samples(mod4)


mod5 <- map2stan(
    alist(pop ~ dnorm(mu, sigma),
          mu <- a + a_loc[location] + (b + b_loc[location] )*year,
          c(a_loc,b_loc)|location ~ dnorm(b, sigma),
          a ~ dnorm(0, 10),
          b ~ dnorm(0, 10),
          sigma ~ dunif(0, 100)
    ), data=otr, iter=8000, chains=4, cores = 3,
    control = list(adapt_delta = 0.8))






