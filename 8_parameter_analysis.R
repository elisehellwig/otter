datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = TRUE)
library(rstan)
library(ggplot2)
library(rethinking)
library(randomForest)
library(glmnet)
library(parallel)
source('functions.R')
options(mc.cores = 3)


av <- read.csv(file.path(datapath, 'allvars.csv'))

DPfx <- readRDS(file.path(datapath, 'models/DPfixedFX.RDS'))
Bfx <-readRDS(file.path(datapath, 'models/BETAfixedFX.RDS'))
Afx <-readRDS(file.path(datapath, 'models/alphafixedFX.RDS'))

####################################################################
####################################################################
set.seed(394885)
firstp <- 10
bestp <- c('Latitude','region')

### Random forest
declinePformulabest <- createformula(av, 'declineP', bestp)
declinerfbest <- randomForest(declinePformulabest, data=av)

declinePformulafirst <- createformula(av, 'declineP', predictorstart = firstp,
                                      predictorend=18)
declinerffirst <- randomForest(declinePformulafirst, data=av)


betaformula <- createformula(av, 'beta', firstp)
betarf <- randomForest(betaformula, data=av)


alphaformula <- createformula(av, 'alpha', firstp)
alpharf <- randomForest(alphaformula, data=av)


metricformula <- createformula(av, 'metric', firstp)
metricrf <- randomForest(metricformula, data=av)
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



###########################################################
##############################################################
#bayesian


av$latitude <- scale(av$Latitude)
attributes(av$latitude) <- NULL

set.seed(1231)
DPlist <- list(declineP=av$declineP,
                  Latitude=av$latitude,
                  N=nrow(av))

DPmod <- stan(model_code = DPfx, data=DPlist, iter=25000, warmup = 5000,
              chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
DPpost <- extract(DPmod)
saveRDS(DPpost, file.path(datapath, 'models/DeclinePFixedPost.RDS'))



set.seed(29918)
Blist <- list(BETA=av$beta,
               Latitude=av$latitude,
               N=nrow(av))

Bmod <- stan(model_code = Bfx, data=Blist, iter=25000, warmup = 5000,
              chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
Bpost <- extract(Bmod)
saveRDS(Bpost, file.path(datapath, 'models/BetaFixedPost.RDS'))


set.seed(493828)
Alist <- list(alpha=av$alpha,
              Latitude=av$latitude,
              N=nrow(av))

Amod <- stan(model_code = Afx, data=Alist, iter=25000, warmup = 5000,
             chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
Apost <- extract(Amod)
saveRDS(Apost, file.path(datapath, 'models/AlphaFixedPost.RDS'))




