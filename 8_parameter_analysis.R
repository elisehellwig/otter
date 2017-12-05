datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = TRUE)
library(rstan)
library(ggplot2)
library(rethinking)
library(randomForest)
library(glmnet)
source('functions.R')

av <- read.csv(file.path(datapath, 'allvars.csv'))



####################################################################
####################################################################

firstp <- c('Latitude','region')

### Random forest
declinePformula <- createformula(av, 'declineP', firstp)
declinerf <- randomForest(declinePformula, data=av)


betaformula <- createformula(av, 'beta', firstp)
betarf <- randomForest(betaformula, data=av)


alphaformula <- createformula(av, 'alpha', firstp)
alpharf <- randomForest(alphaformula, data=av)


metricformula <- createformula(av, 'metric', firstp)
metricrf <- randomForest(metricformula, data=av)



##############################################################
#shrinkage methods
shrinkcols <- c('declineP', 'beta','alpha', 'SFOdistance','SFOtime',
                'PopDensity','logDensity','Longitude','Latitude')
avs <- av[,shrinkcols]
binaryregion <- convertbinary(av$region)
binaryhabitat <- convertbinary(av$habitat)
binaryaccess <- convertbinary((av$Access))
avs <- cbind(avs, binaryregion, binaryhabitat, binaryaccess)


declinelass <- glmnet(x=as.matrix(avs[,4:ncol(avs)]), y=avs$declineP, 
                      family='gaussian', alpha=1)


##############################################################
#bayesian

DPfixed <- map2stan(
    alist(declineP ~ dnorm(mu, sigma),
          mu <- a + bl*Latitude,
          a ~ dnorm(7.6, 10),
          bl ~ dnorm(0, 10),
          sigma ~ dunif(0,20)
    ), 
    data=av, iter=4000, warmup=2000, chains=4, cores=3,
    control = list(adapt_delta = 0.999, max_treedepth=15))








DPreg <- map2stan(
    alist(declineP ~ dnorm(mu, sigma),
          mu <- a + by[regionid]*year,
          by[regionid] ~ dnorm(b, sigma),
          a ~ dnorm(4, 10),
          b ~ dnorm(0, 10),
          sigma ~ dunif(0,20)
    ), data=av, iter=16000, warmup=8000, chains=4, control = list(adapt_delta = 0.999),
    cores=3)

