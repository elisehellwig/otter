datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = TRUE)
library(rstan)
library(rethinking)
library(randomForest)
library(parallel)
library(betareg)
source('functions.R')
options(mc.cores = 3)

av <- read.csv(file.path(datapath, 'allvars.csv'))

DPfx <- readRDS(file.path(datapath, 'models/DPfixedFX.RDS'))
DPbrfx <- readRDS(file.path(datapath, 'models/DPbetaregFX.RDS'))
Bfx <-readRDS(file.path(datapath, 'models/BETAfixedFX.RDS'))
Afx <-readRDS(file.path(datapath, 'models/alphafixedFX.RDS'))

####################################################################
###################################################################

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



##############################################################
##############################################################
# Bayesian 

av$latitude <- scale(av$Latitude)
attributes(av$latitude) <- NULL
  
set.seed(1231)
DPlist <- list(declineP=av$declineP,
                  Latitude=av$latitude,
                  N=nrow(av))

DPmod <- stan(model_code = DPfx, data=DPlist, iter=25000, warmup = 5000,
              chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
DPpost <- rstan::extract(DPmod)
saveRDS(DPmod, file.path(datapath, 'models/DeclinePFixedModel.RDS'))
saveRDS(DPpost, file.path(datapath, 'models/DeclinePFixedPost.RDS'))


Latmat <- matrix(c(rep(1, nrow(av)), av$latitude), ncol=2)
DPbrlist <- list(N=nrow(av), K=ncol(Latmat), declineP=av$declineP, X=Latmat)

DPbr_mod <- stan(model_code = DPbrfx, data=DPbrlist, iter=4000, warmup=2000, 
              chains=4)



##################
set.seed(29918)
Blist <- list(BETA=av$beta,
               Latitude=av$latitude,
               N=nrow(av))

Bmod <- stan(model_code = Bfx, data=Blist, iter=25000, warmup = 5000,
              chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
Bpost <- rstan::extract(Bmod)
saveRDS(Bmod, file.path(datapath, 'models/BetaFixedModel.RDS'))
saveRDS(Bpost, file.path(datapath, 'models/BetaFixedPost.RDS'))


set.seed(493828)
Alist <- list(alpha=av$alpha,
              Latitude=av$latitude,
              N=nrow(av))

Amod <- stan(model_code = Afx, data=Alist, iter=25000, warmup = 5000,
             chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
Apost <- rstan::extract(Amod)
saveRDS(Amod, file.path(datapath, 'models/AlphaFixedModel.RDS'))
saveRDS(Apost, file.path(datapath, 'models/AlphaFixedPost.RDS'))
