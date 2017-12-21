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
library(parallel)
source('functions.R')
options(mc.cores = 3)

<<<<<<< HEAD
=======
>>>>>>> 3e43f67d1937046c24cea96d71069129d4cc282f:8_parameter_analysis.R
av <- read.csv(file.path(datapath, 'allvars.csv'))
>>>>>>> b8bebabe5dc8d25850b5df432983b28581cc56b2

av <- read.csv(file.path(datapath, 'allvars.csv'))

DPfx <- readRDS(file.path(datapath, 'models/DPfixedFX.RDS'))
Bfx <-readRDS(file.path(datapath, 'models/BETAfixedFX.RDS'))
Afx <-readRDS(file.path(datapath, 'models/alphafixedFX.RDS'))

####################################################################
####################################################################
<<<<<<< HEAD
set.seed(394885)
firstp <- 10
bestp <- c('Latitude','region')

### Random forest
declinePformulabest <- createformula(av, 'declineP', bestp)
declinerfbest <- randomForest(declinePformulabest, data=av)

declinePformulafirst <- createformula(av, 'declineP', predictorstart = firstp,
                                      predictorend=18)
declinerffirst <- randomForest(declinePformulafirst, data=av)
=======
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
>>>>>>> b8bebabe5dc8d25850b5df432983b28581cc56b2


betaformula <- createformula(av, 'beta', firstp)
betarf <- randomForest(betaformula, data=av)
brf <- randomForest(beta ~ region + Latitude, data=av)


alphaformula <- createformula(av, 'alpha', firstp)
alpharf <- randomForest(alphaformula, data=av)
arf <- randomForest(alpha ~ region + Latitude, data=av)

metricformula <- createformula(av, 'metric', firstp)
metricrf <- randomForest(metricformula, data=av)
<<<<<<< HEAD
##############################################################
set.seed(394885)
varinds <- c(10,18)
varnames <- c('Latitude','region')
respvars <- c('alpha','beta','declineP')

fullrf <- lapply(respvars, function(var) extractrf(av, var, varinds, 'model'))
selectedrf <- lapply(respvars, function(var) {
    extractrf(av, var, varnames, 'model') 
    })

saveRDS(fullrf, file.path(datapath, 'fullrandomForest.RDS'))
saveRDS(selectedrf, file.path(datapath, 'selectrandomForest.RDS'))
=======
mrf <- randomForest(metric ~ region + Latitude, data=av)
>>>>>>> b8bebabe5dc8d25850b5df432983b28581cc56b2



###########################################################
##############################################################
<<<<<<< HEAD
#bayesian
=======
##############################################################
#sanity checks
binaryregion <- convertbinary(av$region)
avb <- cbind(av, binaryregion)
>>>>>>> b8bebabe5dc8d25850b5df432983b28581cc56b2

latP <- lm(declineP ~ Latitude, data=avb) #latitude is more predictive than
rP <- lm(declineP ~ Latitude, data=avb)   #region. it's the coast vs everything
                                          # else

<<<<<<< HEAD
av$latitude <- scale(av$Latitude)
attributes(av$latitude) <- NULL
=======
<<<<<<< HEAD:10_parameter_analysis.R
latbeta <- lm(beta ~ Latitude, data=avb) #just like before latitude is the 
rbeta <- lm(beta ~ region, data=avb)     #better predictor
=======
declinelass <- glmnet(x=as.matrix(avs[,4:ncol(avs)]), y=avs$declineP, 
                      family='gaussian', alpha=1)
>>>>>>> b8bebabe5dc8d25850b5df432983b28581cc56b2

set.seed(1231)
DPlist <- list(declineP=av$declineP,
                  Latitude=av$latitude,
                  N=nrow(av))

DPmod <- stan(model_code = DPfx, data=DPlist, iter=25000, warmup = 5000,
              chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
DPpost <- extract(DPmod)
saveRDS(DPmod, file.path(datapath, 'models/DeclinePFixedModel.RDS'))
saveRDS(DPpost, file.path(datapath, 'models/DeclinePFixedPost.RDS'))



set.seed(29918)
Blist <- list(BETA=av$beta,
               Latitude=av$latitude,
               N=nrow(av))

Bmod <- stan(model_code = Bfx, data=Blist, iter=25000, warmup = 5000,
              chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
Bpost <- extract(Bmod)
saveRDS(Bmod, file.path(datapath, 'models/BetaFixedModel.RDS'))
saveRDS(Bpost, file.path(datapath, 'models/BetaFixedPost.RDS'))


set.seed(493828)
Alist <- list(alpha=av$alpha,
              Latitude=av$latitude,
              N=nrow(av))

Amod <- stan(model_code = Afx, data=Alist, iter=25000, warmup = 5000,
             chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
Apost <- extract(Amod)
saveRDS(Amod, file.path(datapath, 'models/AlphaFixedModel.RDS'))
saveRDS(Apost, file.path(datapath, 'models/AlphaFixedPost.RDS'))

>>>>>>> 3e43f67d1937046c24cea96d71069129d4cc282f:8_parameter_analysis.R

latalpha <- lm(alpha ~ Latitude, data=avb) #latitude is the predictor but
                                           #SFO distance is also correlated.

<<<<<<< HEAD
=======
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
>>>>>>> b8bebabe5dc8d25850b5df432983b28581cc56b2

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
