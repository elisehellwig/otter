datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rethinking)
rstan_options(auto_write = TRUE)

otr <- read.csv(file.path(datapath, 'otterclean.csv'))
otr$location <- as.factor(otr$location)
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


mod3 <- map2stan(alist(pop ~ dpois(lambda),
                       log(lambda) <- a_loc[location] + by_loc[location]*year,
                       c(a_loc, by_loc)[location] ~ dmvnorm2(c(a, by), 
                                                             sigma_loc, Rho),
                       a ~ dnorm(0, 10),
                       by ~ dnorm(0, 10),
                       sigma_loc ~ dcauchy(0, 2),
                       Rho ~ dlkjcorr(2)
), data=otr, warmup=1000 , iter=5000 , chains=4 , cores=3)




parlist <- c('sigma_loc[1]', 'sigma_loc[2]','Rho[2,1]',
             paste0('a_loc[', 2:3, ']'), paste0('by_loc[', 9:10, ']'))



mod4 <- map2stan(alist(pop ~ dpois(lambda),
                       log(lambda) <- a + by*year + by_region[region]*year,
                       by_region[region] ~ dnorm(0, sigma_region),
                       a ~ dnorm(0, 100),
                       by ~ dnorm(0, 1),
                       sigma_region ~ dcauchy(0, 1)
), data=otr, warmup=2000 , iter=7000 , chains=4 , cores=3)





