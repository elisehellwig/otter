datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = TRUE)
library(rstan)
library(ggplot2)
library(rethinking)
library(randomForest)
library(glmnet)
source('functions.R')

av <- read.csv(file.path(datapath, 'allvars.csv'))



####################################################################
####################################################################

firstp <- c('Latitude','region')

### Random forest
declinePformula <- createformula(av, 'declineP', firstp)
declinerf <- randomForest(declinePformula, data=av)


betaformula <- createformula(av, 'beta', firstp)
betarf <- randomForest(betaformula, data=av)


alphaformula <- createformula(av, 'alpha', firstp)
alpharf <- randomForest(alphaformula, data=av)


metricformula <- createformula(av, 'metric', firstp)
metricrf <- randomForest(metricformula, data=av)



##############################################################
#shrinkage methods
shrinkcols <- c('declineP', 'beta','alpha', 'SFOdistance','SFOtime',
                'PopDensity','logDensity','Longitude','Latitude')
avs <- av[,shrinkcols]
binaryregion <- convertbinary(av$region)
binaryhabitat <- convertbinary(av$habitat)
binaryaccess <- convertbinary((av$Access))
avs <- cbind(avs, binaryregion, binaryhabitat, binaryaccess)


declinelass <- glmnet(x=as.matrix(avs[,4:ncol(avs)]), y=avs$declineP, 
                      family='gaussian', alpha=1)







