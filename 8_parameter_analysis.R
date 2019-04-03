datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = TRUE)
library(rstan)
library(rethinking)
library(randomForest)
library(parallel)
library(betareg)
library(plyr)
library(reshape2)
library(loo)
source('functions.R')
options(mc.cores = 3)

av <- read.csv(file.path(datapath, 'allvars.csv'))

DPfx <- readRDS(file.path(datapath, 'models/DPfixedFX.RDS'))
DPbrfx <- readRDS(file.path(datapath, 'models/DPbetaregFX.RDS'))
Bfx <-readRDS(file.path(datapath, 'models/BETAfixedFX.RDS'))
Afx <-readRDS(file.path(datapath, 'models/alphafixedFX.RDS'))
Apois <-readRDS(file.path(datapath, 'models/ApoisFX.RDS'))

BInt <-readRDS(file.path(datapath, 'models/BETAIntercept.RDS'))
AInt <-readRDS(file.path(datapath, 'models/alphaIntercept.RDS'))
DPInt <-readRDS(file.path(datapath, 'models/DPbetaIntercept.RDS'))


####################################################################
###################################################################


set.seed(394885)
varinds <- c(10,19)
varnames <- c('Latitude','visit')
respvars <- c('alpha','beta','declineP')
varnames2 <- c('Latitude', 'Longitude','region','Access','SFOdistance',
               'habitat', 'PopDensity')
predvars <- c('Access','Distance', 'Time', 'Region', 'Habitat','Visitors',
              'Density', 'LogDensity','Longitude','Latitude')

fullrf <- lapply(respvars, function(var) {
    extractrf(av, var, varinds, 'model', imp=TRUE)
    })
selectedrf <- lapply(respvars, function(var) {
    extractrf(av, var, varnames, 'model') 
    })

saveRDS(fullrf, file.path(datapath, 'fullrandomForest.RDS'))
fullimp <- as.data.frame(sapply(fullrf, function(i) {
    signif(as.numeric(importance(i, type=1)),2)}))

#fullimp <- as.data.frame(apply(fullimp, 2, scale, center=FALSE))

names(fullimp) <- respvars
rownames(fullimp) <- NULL
fullimp$attribute <- rownames(importance(fullrf[[1]]))
fullimp$attribute <- predvars

fullimpm <- melt(fullimp, id.var='attribute', variable.name = 'characteristic')
saveRDS(fullimpm, file.path(datapath, 'fullRFvariableimportance.RDS'))


write.csv(fullimp, file.path(datapath, 'fullRFimportanceTable.csv'),
          row.names = FALSE)

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

set.seed(1231)
Latmat <- matrix(c(rep(1, nrow(av)), av$latitude), ncol=2)
DPbrlist <- list(N=nrow(av), K=ncol(Latmat), declineP=av$declineP, X=Latmat)

DPbr_mod <- stan(model_code = DPbrfx, data=DPbrlist, iter=25000, warmup=5000, 
              chains=1)
DPbrpost <- rstan::extract(DPbr_mod)
saveRDS(DPbr_mod, file.path(datapath, 'models/DeclinePBetaRegModel.RDS'))
saveRDS(DPbrpost, file.path(datapath, 'models/DeclinePBetaRegPost.RDS'))



set.seed(1210331)
Latmat <- matrix(rep(1, nrow(av)), ncol=1)
DPbintlist <- list(N=nrow(av), K=ncol(Latmat), declineP=av$declineP, X=Latmat)

DPbInt_mod <- stan(model_code = DPInt, data=DPbintlist, iter=25000, 
                 warmup=5000, chains=1)
saveRDS(DPbInt_mod, file.path(datapath, 
                              'models/DeclinePBetaInterceptModel.RDS'))



##################
set.seed(29918)
Blist <- list(BETA=av$beta,
               Latitude=av$latitude,
               N=nrow(av))

Bmod <- stan(model_code = Bfx, data=Blist, iter=25000, warmup = 5000,
              chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
Bpost <- rstan::extract(Bmod)
Bloglik <- extract_log_lik(Bmod, parameter='loglik')

saveRDS(Bmod, file.path(datapath, 'models/BetaFixedModel.RDS'))
saveRDS(Bpost, file.path(datapath, 'models/BetaFixedPost.RDS'))

Bint_mod <- stan(model_code = BInt, data=Blist, iter=25000, warmup = 5000,
             chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
saveRDS(Bint_mod, file.path(datapath, 'models/BetaInterceptModel.RDS'))

##############################################################
##############################################################


set.seed(493828)
Alist <- list(alpha=av$alpha,
              Latitude=av$latitude,
              N=nrow(av))

Amod <- stan(model_code = Apois, data=Alist, iter=5000, warmup = 1000,
             chains=1, control=list(adapt_delta = 0.99, max_treedepth=15))

Amod <- stan(model_code = Afx, data=Alist, iter=25000, warmup = 5000,
             chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))

Apost <- rstan::extract(Amod)
Aloglik <- extract_log_lik(Amod, parameter='loglik')

saveRDS(Amod, file.path(datapath, 'models/AlphaFixedModel.RDS'))
saveRDS(Apost, file.path(datapath, 'models/AlphaFixedPost.RDS'))

set.seed(892629)
Aint_mod <- stan(model_code = AInt, data=Alist, iter=25000, warmup = 5000,
             chains=1, control=list(adapt_delta = 0.9999, max_treedepth=15))
saveRDS(Aint_mod, file.path(datapath, 'models/AlphaInterceptModel.RDS'))

Apost <- rstan::extract(Amod)
Aint_loglik <- extract_log_lik(Aint_mod, parameter='loglik')
