datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rstan)
library(ggplot2)
library(rethinking)
av <- read.csv(file.path(datapath, 'allvars.csv'))
Afp <- readRDS(file.path(datapath, 'models/AlphaFixedPost.RDS'))
Bfp <- readRDS(file.path(datapath, 'models/BetaFixedPost.RDS'))
DPfp <- readRDS(file.path(datapath, 'models/DeclinePFixedPost.RDS'))

Amod <- readRDS(file.path(datapath, 'models/AlphaFixedModel.RDS'))
Bmod <- readRDS(file.path(datapath, 'models/BetaFixedModel.RDS'))
DPmod <- readRDS(file.path(datapath, 'models/DeclinePFixedModel.RDS'))


source('functions.R')

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

DPpost <- sapply(1:2, function(column) DPfp$beta[,column])
DPdf <- data.frame(mu=apply(DPpost, 2, mean),
                  lower=apply(DPpost, 2, HPDI, prob=0.95)[1,],
                  upper=apply(DPpost, 2, HPDI, prob=0.95)[2,])

DPres <- processFixedMod(DPmod, av$latitude, av$declineP, rname='declineP')

#######################################################
avars <- c('ID','loc','region','latitude','Latitude','Longitude')

modres <- cbind(Ares, Bres[,2:4], DPres[,2:4])
ResidDF <- merge(av[,avars], modres, by.x='latitude', by.y='lat')

write.csv(ResidDF, file.path(datapath, 'Residuals.csv'), row.names = FALSE)


