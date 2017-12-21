datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = TRUE)
library(rstan)
library(ggplot2)
library(rethinking)
library(randomForest)
library(car)
source('functions.R')
av <- read.csv(file.path(datapath, 'allvars.csv'))



####################################################################
####################################################################
## too many parameters to do normal regression with so we start with
## machine learning!
firstp <- 10

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

latbeta <- lm(beta ~ Latitude, data=avb) #just like before latitude is the 
rbeta <- lm(beta ~ region, data=avb)     #better predictor

latalpha <- lm(alpha ~ Latitude, data=avb) #latitude is the predictor but
                                           #SFO distance is also correlated.

Anova(latalpha, type='II')

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
