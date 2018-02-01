datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rstan)
library(ggplot2)
library(rethinking)
library(reshape2)
source('functions.R')

av <- read.csv(file.path(datapath, 'allvars.csv'))
Afp <- readRDS(file.path(datapath, 'models/AlphaFixedPost.RDS'))
Bfp <- readRDS(file.path(datapath, 'models/BetaFixedPost.RDS'))
DPfp <- readRDS(file.path(datapath, 'models/DeclinePFixedPost.RDS'))
DPbr <- readRDS(file.path(datapath, 'models/DeclinePBetaRegPost.RDS'))


Amod <- readRDS(file.path(datapath, 'models/AlphaFixedModel.RDS'))
Bmod <- readRDS(file.path(datapath, 'models/BetaFixedModel.RDS'))
DPmod <- readRDS(file.path(datapath, 'models/DeclinePFixedModel.RDS'))
DPbrmod <- readRDS(file.path(datapath, 'models/DeclinePBetaRegModel.RDS'))


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
ACIs <- data.frame(mu=apply(Apost, 2, mean),
                 lower=apply(Apost, 2, HPDI, prob=0.95)[1,],
                 upper=apply(Apost, 2, HPDI,  prob=0.95)[2,])
ACIs

Ares <- processFixedMod(Amod, av$latitude, av$alpha, rname='alpha')


###beta

Bpost <- sapply(1:2, function(column) Bfp$beta[,column])
Bdf <- data.frame(mu=apply(Bpost, 2, mean),
                  lower=apply(Bpost, 2, HPDI, prob=0.95)[1,],
                  upper=apply(Bpost, 2, HPDI, prob=0.95)[2,])

Bres <- processFixedMod(Bmod, av$latitude, av$beta, rname='beta')
###DeclineP

DPpost <- sapply(1:2, function(column) DPbr$beta[,column])
DPdf <- data.frame(mu=apply(DPpost, 2, mean),
                  lower=apply(DPpost, 2, HPDI, prob=0.95)[1,],
                  upper=apply(DPpost, 2, HPDI, prob=0.95)[2,])

DPres <- processFixedMod(DPmod, av$latitude, av$declineP, rname='declineP')

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

LatX <- seq(37.8, 38.22, by=0.1)
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


