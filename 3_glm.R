datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rethinking)
rstan_options(auto_write = TRUE)

otr <- read.csv(file.path(datapath, 'otterclean.csv'))

#########################################################

mod1 <- map(
    alist(pop ~ dpois(lambda),
          log(lambda) <- a + by*year,
          a ~ dnorm(0, 100),
          by ~ dnorm(0, 1),
          bl ~ dnorm(0, 1)
    ), data=otr)




mod2 <- map2stan(alist(pop ~ dpois(lambda),
                       log(lambda) <- a + a_loc[location] + by*year,
                       a_loc[location] ~ dnorm(0, sigma_loc),
                       a ~ dnorm(0, 100),
                       by ~ dnorm(0, 1),
                       sigma_loc ~ dcauchy(0, 1)
    ), data=otr, warmup=2000 , iter=7000 , chains=4 , cores=3)
