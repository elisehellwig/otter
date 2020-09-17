datapath <- '/Users/echellwig/Google Drive/OtherPeople/otterData/'
options(stringsAsFactors = FALSE)
library(rethinking)
rstan_options(auto_write = TRUE)

otr <- read.csv(file.path(datapath, 'otterclean2019.csv'))
otr$cpop <- scale(otr$pop)
attributes(otr$cpop) <- NULL

#########################################################
la <- lapply(unique(otr$location), function(loc) {
    lm(pop ~ year, data=otr[otr$location==loc, ])
})

pop2 <- pop

mod1 <- quap(
    alist(pop ~ dnorm(mu, sigma),
          mu <- a + by*year,
          a ~ dnorm(0, 1),
          by ~ dnorm(0, 1),
          sigma ~ dexp(1)
    ), data=otr)

otrlist <- list(pop=otr$pop,
                year=otr$year,
                loc=coerce_index(otr$location))

mod2 <- ulam(
    alist(pop ~ dnorm(mu, sigma),
          mu <- a + a_loc[loc] + b*year,
          a_loc[loc] ~ dnorm(0, sigma_loc),
          a ~ dnorm(4, 10),
          b ~ dnorm(0, 5),
          sigma ~ dexp(1),
          sigma_loc ~ cauchy(0,1)
    ), data=otrlist, chains=1)

mod2 <- ulam(
    alist(pop ~ dnorm(mu, sigma),
          mu <- a + a_loc[location] + b*year,
          a_loc[location] ~ dnorm(0, sigma_loc),
          a ~ dnorm(4, 10),
          b ~ dnorm(0, 5),
          sigma ~ dexp(1),
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






