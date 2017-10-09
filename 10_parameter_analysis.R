datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = TRUE)
library(rstan)
library(ggplot2)
library(rethinking)
library(randomForest)
library(glmnet)
library(car)
source('functions.R')
av <- read.csv(file.path(datapath, 'allvars.csv'))



####################################################################
####################################################################

firstp <- 10

### Random forest
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

sanitycheck <- lm(beta ~ Latitude, data=av)
sanity
#forward/backward selection



##############################################################
## Anova
