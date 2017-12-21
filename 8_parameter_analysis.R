datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = TRUE)
library(rstan)
library(ggplot2)
library(rethinking)
library(randomForest)
<<<<<<< HEAD:10_parameter_analysis.R
library(car)
source('functions.R')
=======
library(glmnet)
source('functions.R')

>>>>>>> 3e43f67d1937046c24cea96d71069129d4cc282f:8_parameter_analysis.R
av <- read.csv(file.path(datapath, 'allvars.csv'))



####################################################################
####################################################################
<<<<<<< HEAD:10_parameter_analysis.R
## too many parameters to do normal regression with so we start with
## machine learning!
firstp <- 10
=======

firstp <- c('Latitude','region')
>>>>>>> 3e43f67d1937046c24cea96d71069129d4cc282f:8_parameter_analysis.R

### Random forest
set.seed(1928340)
declinePformula <- createformula(av, 'declineP', firstp)
declinerf <- randomForest(declinePformula, data=av)
drf <- randomForest(declineP ~ region + Latitude, data=av)


betaformula <- createformula(av, 'beta', firstp)
betarf <- randomForest(betaformula, data=av)
brf <- randomForest(beta ~ region + Latitude, data=av)


alphaformula <- createformula(av, 'alpha', firstp)
alpharf <- randomForest(alphaformula, data=av)
arf <- randomForest(alpha ~ region + Latitude, data=av)

metricformula <- createformula(av, 'metric', firstp)
metricrf <- randomForest(metricformula, data=av)
mrf <- randomForest(metric ~ region + Latitude, data=av)


##############################################################
##############################################################
#sanity checks
binaryregion <- convertbinary(av$region)
avb <- cbind(av, binaryregion)

latP <- lm(declineP ~ Latitude, data=avb) #latitude is more predictive than
rP <- lm(declineP ~ Latitude, data=avb)   #region. it's the coast vs everything
                                          # else

<<<<<<< HEAD:10_parameter_analysis.R
latbeta <- lm(beta ~ Latitude, data=avb) #just like before latitude is the 
rbeta <- lm(beta ~ region, data=avb)     #better predictor
=======
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






>>>>>>> 3e43f67d1937046c24cea96d71069129d4cc282f:8_parameter_analysis.R

latalpha <- lm(alpha ~ Latitude, data=avb) #latitude is the predictor but
                                           #SFO distance is also correlated.

<<<<<<< HEAD:10_parameter_analysis.R
Anova(latalpha, type='II')
=======
DPreg <- map2stan(
    alist(declineP ~ dnorm(mu, sigma),
          mu <- a + by[regionid]*year,
          by[regionid] ~ dnorm(b, sigma),
          a ~ dnorm(4, 10),
          b ~ dnorm(0, 10),
          sigma ~ dunif(0,20)
    ), data=av, iter=16000, warmup=8000, chains=4, control = list(adapt_delta = 0.999),
    cores=3)
>>>>>>> 3e43f67d1937046c24cea96d71069129d4cc282f:8_parameter_analysis.R

##############################################################
## Region vs. Latitude: can we tell which one is better??

avb$latitude <- scale(avb$Latitude)

latalpha <- map(
    alist(
    alpha ~ dnorm(mu, sigma),
    mu <- a + b_lat*Latitude,
    a ~ dnorm(4.2, 10),
    b_lat ~ dnorm(0,1),
    sigma ~ dunif(0, 10)
    ), data=avb 
)


regbeta <- map(
    alist(
        alpha ~ dnorm(mu, sigma),
        mu <- a + bcst*coast +  bin*inland + brys*pointreyes,
        a ~ dnorm(4.2, 10),
        c(bcst, bin, brys) ~ dnorm(0,1),
        sigma ~ dunif(0, 10)
    ), data=avb 
)


reglatbeta <- map(
    alist(
        alpha ~ dnorm(mu, sigma),
        mu <- a + blat*latitude + bcst*coast +  bin*inland + brys*pointreyes,
        a ~ dnorm(4.2, 10),
        c(blat, bcst, bin, brys) ~ dnorm(0,1),
        sigma ~ dunif(0, 10)
    ), data=avb 
)

compare(regbeta, latbeta, reglatbeta)
