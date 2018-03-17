datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
library(rstan)
library(rethinking)
library(dismo)
source('functions.R')

av <- read.csv(file.path(datapath, 'allvars.csv'))
qual <- read.csv(file.path(datapath, 'ParameterEstimates.csv'))

nfold <- 7
set.seed(892277)
av$fold <- kfold(av, 7)

#models
DPfx <- readRDS(file.path(datapath, 'models/DPbetaregFX.RDS'))
Bfx <-readRDS(file.path(datapath, 'models/BETAfixedFX.RDS'))
Afx <-readRDS(file.path(datapath, 'models/alphafixedFX.RDS'))
BInt <-readRDS(file.path(datapath, 'models/BETAIntercept.RDS'))
AInt <-readRDS(file.path(datapath, 'models/alphaIntercept.RDS'))
DPInt <-readRDS(file.path(datapath, 'models/DPbetaIntercept.RDS'))

names(av)[8] <- 'BETA'
################################
##alpha 
set.seed(2937)
AlphaCV <- crossval(Afx, 'latitude', 'alpha', av, k=nfold)

set.seed(1938)
AlphaIntCV <- crossval(AInt, 'latitude', 'alpha', av, k=nfold, 
                       noPredictor=TRUE)

#beta
set.seed(6747)
BetaCV <- crossval(Bfx, 'latitude', 'BETA', av, k=nfold)

set.seed(1938)
BetaIntCV <- crossval(BInt, 'latitude', 'BETA', av, k=nfold, 
                       noPredictor=TRUE)

#DP
set.seed(8578)
dpCV <- crossval(DPfx, 'latitude', 'declineP', av, k=nfold, betareg=TRUE)

set.seed(7859)
dpIntCV <- crossval(DPInt, 'latitude', 'declineP', av, k=nfold,
                    noPredictor=TRUE, betareg=TRUE)



err <- round(c(AlphaCV, AlphaIntCV, BetaCV, BetaIntCV, dpCV, dpIntCV),3)
moderr <- c(err[1], '', err[3], '', err[5], '')
nullerr <- c(err[2], '', err[4], '', err[6], '')

qual$moderror <- moderr
qual$nullerror <- nullerr

write.csv(qual, file.path(datapath, 'ParameterEstimates.csv'), 
          row.names = FALSE)

