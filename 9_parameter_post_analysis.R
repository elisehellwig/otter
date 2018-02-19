datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rstan)
library(ggplot2)
library(rethinking)
library(reshape2)
library(loo)
source('functions.R')

av <- read.csv(file.path(datapath, 'allvars.csv'))
Afp <- readRDS(file.path(datapath, 'models/AlphaFixedPost.RDS'))
Bfp <- readRDS(file.path(datapath, 'models/BetaFixedPost.RDS'))
DPfp <- readRDS(file.path(datapath, 'models/DeclinePFixedPost.RDS'))
DPbr <- readRDS(file.path(datapath, 'models/DeclinePBetaRegPost.RDS'))


Amod <- readRDS(file.path(datapath, 'models/AlphaFixedModel.RDS'))
Aint_mod <- readRDS(file.path(datapath, 'models/AlphaInterceptModel.RDS'))
Bmod <- readRDS(file.path(datapath, 'models/BetaFixedModel.RDS'))
Bint_mod <- readRDS(file.path(datapath, 'models/BetaInterceptModel.RDS'))

DPmod <- readRDS(file.path(datapath, 'models/DeclinePFixedModel.RDS'))
DPbrmod <- readRDS(file.path(datapath, 'models/DeclinePBetaRegModel.RDS'))
DPbIntmod <- readRDS(file.path(datapath, 
                             'models/DeclinePBetaInterceptModel.RDS'))


Afit <- modfit(Amod)
Bfit <- modfit(Bmod)
DPfit <- modfit(DPbrmod, betareg = TRUE)
####



respvars <- c('alpha','beta','declineP')
char <- c('PopSize', 'PopTrend','DeclineProb')




###########################################################################
###########################################################################

##alpha

Apost <- sapply(1:2, function(column) Afp$beta[,column])
Adf <- data.frame(char='IGS', par=c('intercept','Latitude'),
                  mu=apply(Apost, 2, mean),
                  lower=apply(Apost, 2, HPDI, prob=0.975)[1,],
                  upper=apply(Apost, 2, HPDI,  prob=0.975)[2,])

Ares <- processFixedMod(Amod, av$latitude, av$alpha, rname='alpha')


###beta

Bpost <- sapply(1:2, function(column) Bfp$beta[,column])
Bdf <- data.frame(char='PGR', par=c('intercept','Latitude'),
                  mu=apply(Bpost, 2, mean),
                  lower=apply(Bpost, 2, HPDI, prob=0.95)[1,],
                  upper=apply(Bpost, 2, HPDI, prob=0.95)[2,])

Bres <- processFixedMod(Bmod, av$latitude, av$beta, rname='beta')
###DeclineP

DPpost <- sapply(1:2, function(column) DPbr$beta[,column])
DPdf <- data.frame(char='Decline', par=c('intercept','Latitude'),
                   mu=apply(DPpost, 2, mean),
                   lower=apply(DPpost, 2, HPDI, prob=0.95)[1,],
                   upper=apply(DPpost, 2, HPDI, prob=0.95)[2,])

DPres <- processFixedMod(DPbrmod, av$latitude, av$declineP, rname='declineP')

#######################################################
avars <- c('ID','loc','region','latitude','Latitude','Longitude')

modres <- cbind(Ares, Bres[,2:4], DPres[,2:4])
ResidDF <- merge(av[,avars], modres, by.x='latitude', by.y='lat')

write.csv(ResidDF, file.path(datapath, 'Residuals.csv'), row.names = FALSE)




#######################################################
#37.7-38.3 centered: 37.98501, scaled:0.10711

brvars <- c('declineP','alpha','beta','Latitude','latitude')

avr <- av[,brvars]
avr$type <- 'obs'

LatX <- seq(37.8, 38.22, by=0.01)
latX <- (LatX - 37.98501)/0.10711

avfit <- data.frame(Latitude=LatX,
                    latitude=latX,
                    type='fit',
                    alpha=Afit(latX),
                    beta=Bfit(latX),
                    declineP=DPfit(latX))

AV <- rbind(avr, avfit)
avm <- melt(AV, id.vars=c('Latitude','latitude','type'), 
            variable.name = 'response', value.name = 'value')

write.csv(avm, file.path(datapath, 'LatitudeModelResults.csv'), 
          row.names = FALSE)

#######################################################
#crossvalidation

estimates <- rbind(Adf, Bdf, DPdf)
write.csv(estimates, file.path(datapath, 'ParameterEstimates.csv'), 
          row.names = FALSE)

Aloglike <- extract_log_lik(Amod, parameter='loglik')
Aintlike <- extract_log_lik(Aint_mod, parameter='loglik')

Bloglike <- extract_log_lik(Bmod, parameter='loglik')
Bintlike <- extract_log_lik(Bint_mod, parameter='loglik')

DPloglike <- extract_log_lik(DPbrmod, parameter='log_lik')
DPintlike <- extract_log_lik(DPbIntmod, parameter='log_lik')

Aslope <- loo(Aloglike)
Aint <- loo(Aintlike)

Bslope <- waic(Bloglike)
Bint <- waic(Bintlike)

DPslope <- loo(DPloglike)
DPint <- loo(DPintlike)
