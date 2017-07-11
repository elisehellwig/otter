datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rethinking)
rstan_options(auto_write = TRUE)

otr <- read.csv(file.path(datapath, 'otterclean.csv'))

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


mod2 <- map2stan(
    alist(pop ~ dnorm(mu, sigma),
          mu <- a + by[regionid]*year,
          a ~ dnorm(4, 50),
          by[regionid] ~ dnorm(0.3, 1),
          sigma ~ dunif(0, 100)
    ), data=otr, start=alist(a=4, by=0.3, sigma=2.12))





