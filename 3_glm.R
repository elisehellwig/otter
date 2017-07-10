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




mod2 <- map2stan()
